
# UI ----------------------------------------------------------------------

correlations_ui <- function(id, sidebar_width) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Correlations",

      "View correlation heat maps that show how indicators are related
      to other indicators and aggregate scores.",

      selectInput(
        ns("i_corr_family_plot"),
        "Plot type",
        choices = list(
          "Group against group" = FALSE,
          "Group against parents" = TRUE
        )
      ),

      strong("Correlate:"),
      selectInput(ns("i_group1"), label = NULL, choices = NULL),
      numericInput(ns("i_corr_level1"), "At level:", 1),

      strong("Against:"),
      selectInput(ns("i_group2"), label = NULL, choices = NULL),
      numericInput(ns("i_corr_level2"), "At level", 1),

      input_switch(ns("i_corr_show_values"), label = "Show values"),
      input_switch(ns("i_corr_discrete_colours"), label = "Discrete colours"),
      input_switch(ns("i_corr_by_groups"), label = "Show groupings"),

      actionButton(ns("i_explore_corr_submit_btn"), "Run")

    ),


    # MAIN PANEL
    card(
      full_screen = TRUE,
      card_header("Correlation heat map"),
      # HEAT MAP
      card_body(
        plotOutput(ns("o_plot_corr"), height = "80vh")
      )
    )
  )

}

# SERVER ------------------------------------------------------------------

correlations_server <- function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    # update inputs based on coin
    observe({
      req(coin())
      req(r_share$results_calculated)
      group_choices <- get_indicator_codes(coin(), code_types = "Aggregate", with_levels = TRUE)
      updateSelectInput(inputId = "i_group1", choices = group_choices)
      updateSelectInput(inputId = "i_group2", choices = group_choices)
    }) |> bindEvent(r_share$results_calculated)

    # CORRELATION PLOT
    output$o_plot_corr <- renderPlot({

      req(r_share$results_calculated)

      f_plot_correlation(
        coin(),
        dset = "Aggregated",
        group1 = input$i_group1,
        group2 = input$i_group2,
        level1 = input$i_corr_level1,
        level2 = input$i_corr_level2,
        #only_with_parents = input$i_corr_with_parents,
        family_plot = as.logical(input$i_corr_family_plot),
        show_values = input$i_corr_show_values,
        discrete_colours = input$i_corr_discrete_colours,
        plot_by_groups = input$i_corr_by_groups
      )
    }) |>
      bindEvent(input$i_explore_corr_submit_btn)

    # disable correlation plot second group if family plot selected
    observeEvent(input$i_corr_family_plot, {
      switch(
        input$i_corr_family_plot,
        "TRUE" = {
          shinyjs::disable(id="i_group2")
          shinyjs::disable(id="i_corr_level2")
        },
        "FALSE" = {
          shinyjs::enable(id="i_group2")
          shinyjs::enable(id="i_corr_level2")
        }
      )
    })

  })

}

# SUPPORTING FUNCTIONS ----------------------------------------------------


# Takes the df as input which is output from COINr::get_corr_flags().
#
# Gathers high correlations into a single string per indicator, suitable for
# passing to the display table.
f_gather_correlations <- function(X){

  Xpairs <- data.frame(v1 = c(X$Ind1, X$Ind2),
                       v2 = c(X$Ind2, X$Ind1))

  Xpairs <- Xpairs[order(Xpairs$v1, Xpairs$v2), ]

  tapply(Xpairs$v2, Xpairs$v1, paste0, collapse = ", ")

}


#' Correlation plots
#'
#' Plots the correlations between specified groups of indicators/aggregates according to specifications.
#'
#' This calls [COINr::plot_corr()] which is a quite flexible function but comes with some complexity. The
#' question is how much flexibility/complexity to pass to the user. At the moment it is set up expecting
#' the user inputs as described in the function arguments. I expect that we may need some adjustments when
#' this is hooked up to the UI, as it becomes clearer what is useful and what not.
#'
#' Another question: if we want interactive plots we can feed this into `ggplotly()`. It will need a little
#' tweaking.
#'
#' For the `group1` and `group2` args, the input to the *function* should be one of
#' `get_indicator_codes(coin, with_levels = FALSE)`, whereas the *dropdown* in the app should display instead
#' `get_indicator_codes(coin, with_levels = TRUE)`. The latter is hopefully clearer to display.
#'
#' NOTE that for the `level1` an `level2` user inputs, these must be set up as follows:
#'
#' * if `dset == "Aggregated"` the dropdown for both should be `1 : get_n_levels(coin)`
#' * else for any other `dset` the dropdowns should be hidden or should only contain a single entry: 1.
#'
#' At the moment the function will set levels to 1 and issue a warning if the latter case is not followed.
#'
#' @param coin The coin
#' @param dset Data set to use, one of `get_dset_names()`
#' @param group1 First group of indicators. See details.
#' @param group2 Second group of indicators.
#' @param level1 Level to take first group from. See details.
#' @param level2 Level to take second group from. See details.
#' @param only_with_parents Logical (switch in app?)
#' @param family_plot Logical (switch in app?)
#'
#' @param show_values Logical (switch in app?)
#' @param discrete_colours Logical (switch in app?)
#' @param plot_by_groups Logical (switch in app?)
#'
#' @return Plot generated by ggplot2
f_plot_correlation <- function(
    coin,
    dset = "Raw",
    group1,
    group2,
    level1 = 1,
    level2 = 1,
    only_with_parents = FALSE,
    family_plot = FALSE,
    show_values = FALSE,
    discrete_colours = TRUE,
    plot_by_groups = TRUE){

  # Input checks ----

  over_level_1 <- any(level1 > 1, level2 > 1)

  if(dset != "Aggregated" && over_level_1){
    warning("Cannot specify level1 or level2 greater than 1 if dset is not 'Aggregated'.")
    level1 <- level2 <- 1
  }

  n_levels <- get_n_levels(coin)

  stopifnot(is.logical(only_with_parents),
            is.logical(family_plot),
            is.logical(show_values),
            is.logical(discrete_colours),
            is.logical(plot_by_groups),
            level1 %in% 1:n_levels,
            level2 %in% 1:n_levels)

  # Ajust inputs for COINr ----

  if (family_plot) {
    only_with_parents <- "family"
  }

  # automatic specification of how correlations are grouped on the plot
  # to avoid passing this control to the user
  if (plot_by_groups) {

    if (n_levels == 3) {
      group_level <- 2
      box_level <- NULL
    } else if (n_levels > 3) {
      group_level <- 3
      box_level <- 2
    }
  } else {
    group_level <- box_level <- NULL
  }

  # Plot ----

  COINr::plot_corr(
    coin,
    dset = dset,
    iCodes = c(group1, group2),
    Levels = c(level1, level2),
    withparent = only_with_parents,
    showvals = show_values,
    flagcolours = discrete_colours,
    grouplev = group_level,
    box_level = box_level
  ) +
    ggplot2::theme(text = ggplot2::element_text(size = 15))

}
