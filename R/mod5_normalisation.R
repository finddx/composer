
# UI ----------------------------------------------------------------------

normalisation_ui <- function(id) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Normalisation",

      "Bring indicators onto a common scale by normalising using the method selected.
    Click on table columns to visualise indicators before and after normalisation.",

      selectInput(
        ns("i_normalisation_method"),
        "Normalisation method",
        choices = c("Min-max" = "n_minmax",
                    "Z-score" = "n_zscore",
                    "Borda scores" = "n_borda",
                    "Percentile ranks" = "n_prank"
        ),
        selected = "n_minmax"
      ),
      numericInput(inputId = ns("i_norm_para_1"),
                   label = "Min",
                   value = 0
      ),
      numericInput(inputId = ns("i_norm_para_2"),
                   label = "Max",
                   value = 100
      ),
      actionButton(ns("i_normalisation_btn"), "Run"),

      # SCATTER PLOT
      plotlyOutput(ns("o_plot_normalised"))

    ),


    # MAIN PANEL
    card(
      full_screen = TRUE,
      card_header("Normalised data"),
      # TREATED DATA TABLE
      card_body(
        DT::dataTableOutput(ns("dataNormalised"))
      )
    )
  )
}

# SERVER ------------------------------------------------------------------

normalisation_server <- function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    # conditional parameters for normalisation method
    observeEvent(input$i_normalisation_method, {

      switch(
        input$i_normalisation_method,
        n_zscore = {
          shinyjs::enable(id="i_norm_para_1")
          shinyjs::enable(id="i_norm_para_2")
          updateNumericInput(inputId = "i_norm_para_1", label = "Mean", value = 10)
          updateNumericInput(inputId = "i_norm_para_2", label = "Std. dev.", min = 0.01, value = 1)
        },
        n_minmax = {
          shinyjs::enable(id="i_norm_para_1")
          shinyjs::enable(id="i_norm_para_2")
          updateNumericInput(inputId = "i_norm_para_1", label = "Min", value = 1)
          updateNumericInput(inputId = "i_norm_para_2", label = "Max", value = 100)
        },
        n_borda = {
          shinyjs::disable(id="i_norm_para_1")
          shinyjs::disable(id="i_norm_para_2")
        },
        n_prank = {
          shinyjs::disable(id="i_norm_para_1")
          shinyjs::disable(id="i_norm_para_2")
        }
      )

    })

    # normalise data on click
    observeEvent(input$i_normalisation_btn, {
      req(coin())
      coin_normalised <- f_normalisation(
        coin(),
        dset = f_select_dset(coin(), "f_normalisation"),
        norm_method = input$i_normalisation_method,
        norm_params = c(input$i_norm_para_1, input$i_norm_para_2)
      )
      make_dset_toast("Normalised")

      # regenerate any subsequent ops if needed
      coin_normalised <- regen_outdated_coin(coin_normalised, "f_normalisation")
      coin(coin_normalised)
      r_share$data_is_normalised <- TRUE
    })

    # normalised data frame
    df_normalised <- reactive({
      req(coin()$Data$Normalised)
      COINr::get_dset(coin(), dset = "Normalised", also_get = "uName") |>
        COINr::signif_df(digits = 4)
    })

    # normalised data table
    output$dataNormalised <- DT::renderDataTable({
      req(df_normalised())
      f_style_norm_table(df_normalised())
    }, server = FALSE)

    # scatter plot
    output$o_plot_normalised <- renderPlotly({
      req(df_normalised())
      req(input$dataNormalised_columns_selected)
      f_plot_scatter_norm(
        coin(),
        df_normalised(),
        input$dataNormalised_columns_selected,
        f_select_dset(coin(), "f_normalisation")
      )
    })

  })

}

# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Normalise
#'
#' Takes the normalisation method and parameters as inputs, and returns the coin with normalised data set.
#'
#' We will need some kind of conditional inputs in the app. For `"n_borda"` and `"n_prank"` methods, no additional parameters are needed,
#' but for `"n_minmax"` we would need two inputs for upper and lower bounds respectively. With the constraint that upper must be less than
#' lower. For `"n_zscore"` we would need again two inputs but labelled "mean" and "standard deviation" respectively. Standard deviation
#' constrained to be greater than zero.
#'
#' This sub-tab ideally also has a scatter plot and this is now provided by the function
#' `f_plot_scatter()` to avoid regenerating the data treatment if the plot changes and vice versa.
#'
#' @param coin The coin
#' @param dset The data set to apply the operation to. Probably not used so leave `NULL` unless allow the user to change order.
#' @param norm_method A string: one of `"n_minmax"`, `"n_zscore"`, `"n_borda"`, `"n_prank"`, which defines
#' which normalisation method to use.
#' @param norm_params Additional parameters to support the normalisation method:
#'
#' * if `norm_method = "n_minmax"`, then `norm_params` should be a numerical vector `c(x_lower, x_upper)` where `x_lower` and `x_upper` are
#' the lower and upper bounds of the interval to scale indicators on. E.g. `c(0, 100)` scales to the 0-100 interval.
#' * if `norm_method = "n_zscore"` then `norm_params` should be a numerical vector `c(x_mean, x_sd)` defining the mean and standard deviation
#' to use in rescaling. E.g. `c(0, 1)` scales all indicators to have mean 0 and standard deviation 1.
#'
#' Otherwise leave `NULL`.
#'
#' @importFrom plotly layout
#'
#' @return The updated coin.
f_normalisation <- function(coin, dset = NULL, norm_method, norm_params = NULL) {

  stopifnot(norm_method %in% c("n_minmax", "n_zscore", "n_borda", "n_prank"))

  dset <- dset %||% f_select_dset(coin, "f_normalisation")

  # adjust parameters for compatibility: Normalise() requires a named list input, to make things
  # simpler we allow a vector input to f_normalistion() which is tweaked here.
  if(!is.null(norm_params)){
    norm_params <- switch(norm_method,
                          n_minmax = list(l_u = norm_params),
                          n_zscore = list(m_sd = norm_params),
                          NULL)
  }

  COINr::qNormalise(
    coin,
    dset = dset,
    f_n = norm_method,
    f_n_para = norm_params) |>
    suppressMessages()

}

#' Plot scatter plot before/after normalisation
#'
#' Plot of selected indicator
#'
#' @param coin The coin
#' @param df_normalised is the normalised data frame
#' @param icol_selected is the column index selected (to plot)
#' @param dset1 The data set to plot on x axis
#
f_plot_scatter_norm <- function(coin, df_normalised, icol_selected, dset1){

  # get icode to plot
  icode_plot <- names(df_normalised)[icol_selected + 1]

  if (icode_plot %in% c("uCode", "uName")) return(NULL)

  # TODO if we allow users to skip operations, will have to change dset1 here
  iCOINr::iplot_scatter(
    coin,
    iCodes = icode_plot,
    dsets = c(dset1, "Normalised"),
    Levels = 1,
    axes_label = "iName+dset"
  )
}

# set options for normalisation table
f_style_norm_table <- function(df_normalised){
  DT::datatable(
    df_normalised,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      buttons =
        list('copy', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        ))
    ),
    rownames = FALSE,
    selection = list(target = "column", mode = "single", selected = 2),
  )
}
