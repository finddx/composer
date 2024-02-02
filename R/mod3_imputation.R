
# UI ---------------------------------------------------------------------

imputation_ui <- function(id, sidebar_width) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Imputation of missing data",

      "Optional imputation of missing data points. Select imputation method
    and unit grouping if a group method is selected.",

      # select imputation type
      selectInput(
        ns("i_imputation_method"),
        "Impute using:",
        choices = c("Indicator mean" = "i_mean",
                    "Indicator median" = "i_median",
                    "Unit group mean" = "i_mean_grp",
                    "Unit group median" = "i_median_grp"
        ),
        selected = "i_mean"
      ),
      # select group level if relevant to imputation type
      selectInput(ns("i_group_level"),
                  "Use group:",
                  choices = NULL
      ),

      actionButton(ns("i_imputation_btn"), "Run"),
      actionButton(inputId = ns("i_undo"), label = "Remove operation")

    ),


    # MAIN PANEL
    layout_columns(

      col_widths = c(6, 6, 12),
      row_heights = c(1, 5),

      # INFO BOXES
      value_box(
        title = "Missing points before imputation",
        value = textOutput(NS(id, "imputedText_pre")),
        theme = "text-info",
        showcase = icon("info"), showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE
      ),
      value_box(
        title = "Missing points after imputation",
        value = textOutput(NS(id, "imputedText_post")),
        theme = "text-info",
        showcase = icon("info"), showcase_layout = "left center",
        full_screen = FALSE, fill = TRUE
      ),

      # SCREENED DATA TABLE
      card(
        full_screen = TRUE,
        card_header("Imputed data"),
        card_body(
          p(id = ns("placeholder_text"), "Run imputation first..."),
          DT::dataTableOutput(ns("dataImputed"))
        )
      )
    )
  )
}

# SERVER ------------------------------------------------------------------

imputation_server <- function(id, r_shared, coin) {

  moduleServer(id, function(input, output, session) {

    # update unit groups for imputation
    observe({
      group_choices <- unlist(f_get_group_list(coin()))
      updateSelectInput(inputId = "i_group_level", choices = group_choices)
    })

    # conditional parameters for imputation method
    observeEvent(input$i_imputation_method, {
      switch(
        input$i_imputation_method,
        i_mean = {
          shinyjs::disable("i_group_level")
        },
        i_median = {
          shinyjs::disable("i_group_level")
        },
        i_mean_grp = {
          shinyjs::enable("i_group_level")
        },
        i_median_grp = {
          shinyjs::enable("i_group_level")
        }
      )
    })

    # impute data on click
    observeEvent(input$i_imputation_btn, {
      req(coin())
      coin_imputed <- f_imputation(
        coin(),
        dset = f_select_dset(coin(), "f_imputation"),
        imputation_method = input$i_imputation_method,
        imputation_group = input$i_group_level
      )
      make_dset_toast("Imputed")

      # regenerate any subsequent ops if needed
      coin_imputed <- regen_outdated_coin(coin_imputed, "f_imputation")
      # update coin
      coin(coin_imputed)
      shinyjs::hide("placeholder_text")
    })

    # undo on click
    observeEvent(input$i_undo, {
      req(coin()$Data$Imputed)
      coin(undo_data_op(coin(), "f_imputation"))
    })

    # get info about the imputation
    imp_stats <- reactive({
      req(coin()$Data$Imputed)
      f_imputation_stats(coin())
    })

    # imputed data table
    output$dataImputed <- DT::renderDataTable({
      req(imp_stats())
      f_style_imputation_table(
        imp_stats()$df_imputed,
        imp_stats()$imputed_points,
        highlight_background = "#C6EFCE",
        highlight_text = "#006100"
      )
    }, server = FALSE)



    # text summary before imputation
    output$imputedText_pre <- renderText({
      req(imp_stats())
      n <- imp_stats()$n_missing_data_pre
      prc <- round(imp_stats()$frac_missing_data_pre*100)
      glue::glue("{n} ({prc}%)")
    })

    # text summary after imputation
    output$imputedText_post <- renderText({
      req(imp_stats())
      n <- imp_stats()$n_missing_data_post
      prc <- round(imp_stats()$frac_missing_data_post*100)
      glue::glue("{n} ({prc}%)")
    })

  })

}


# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Impute
#'
#' Takes a coin and imputation specifications and outputs coin with imputed data set.
#'
#' If imputation method is set as `"i_mean_grp"` or `"i_median_grp"`, a group is required to be specified via
#' the `imputation_group` argument. These groups are any that were input with the input data and metadata. If no
#' groups were input, these methods will not work.
#'
#' We could possibly add a missing data visualisation as well at some point.
#'
#' @param coin A coin
#' @param dset The data set to operate on. If not defined, used a set order as defined by `f_select_dset()`.
#' @param imputation_method One of `"i_mean"`, `"i_median"`, `"i_mean_grp"`, `"i_median_grp"`.
#' @param imputation_group Only specify if `imputation_method %in% c("i_mean_grp", "i_median_grp")`, else `NULL`.
#' If specified, points to a group variable which was entered with the input data to use in group imputation.
#'
#' @importFrom COINr Impute get_dset
#' @importFrom stats na.omit
#'
#' @return The updated coin
#'
f_imputation <- function(coin, dset = NULL, imputation_method, imputation_group = NULL){

  dset <- dset %||% f_select_dset(coin, "f_imputation")

  # not sure if we would anyway get a NULL for imputation_group from the front end, but if not:
  if (!(imputation_method %in% c("i_mean_grp", "i_median_grp"))) {

    # set this to NULL so call to Impute works for non-group methods
    imputation_group <- NULL

  } else {

    # some basic checks for group imputation

    if(is.null(imputation_group)){
      stop("A group must be specified to use group imputation.")
    }

    groups_available <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Group"] |>
      stats::na.omit()
    if(length(groups_available) == 0){
      stop("Cannot use group imputation: no groups were input with input data and metadata.", call. = FALSE)
    }

    stopifnot(imputation_group %in% groups_available)

    # issue warning if group has NAs in it
    group_vector <- coin$Meta$Unit[[imputation_group]]
    if(any(is.na(group_vector))){
      send_input_warning(
        paste0("Missing values detected in selected group variable ", imputation_group, ". This may result in missing values still present after imputation.")
      )
    }

  }

  coin <- COINr::Impute(coin,
                        dset = dset,
                        f_i = imputation_method,
                        impute_by = "column",
                        use_group = imputation_group,
                        out2 = "coin",
                        write_to = "Imputed") |>
    suppressMessages()

  coin

}

# generates a load of accessory information for imputation, including which
# points were imputed, and number and percentage missing before and after.
f_imputation_stats <- function(coin, dset = NULL){

  dset <- dset %||% f_select_dset(coin, "f_imputation")

  # stats before imputation ----
  # also_get = "none" returns only the numerical (indicator columns) of the data set
  data_dset <- COINr::get_dset(coin, dset, also_get = "none")
  n_missing_data_pre <- sum(is.na(data_dset))
  frac_missing_data_pre <- n_missing_data_pre/(nrow(data_dset)*ncol(data_dset))

  # stats after imputation
  data_imputed <- COINr::get_dset(coin, "Imputed", also_get = "none")
  n_missing_data_post <- sum(is.na(data_imputed))
  frac_missing_data_post <- n_missing_data_post/(nrow(data_imputed)*ncol(data_imputed))

  # also_get = "uName" returns additionally the unit name column and the uCode column which is returned by default
  # see ?COINr::get_dset if this is not clear
  # also round to 4 significant figures
  df_imputed <- COINr::get_dset(coin, dset = "Imputed", also_get = "uName") |>
    COINr::signif_df(digits = 4)

  # flags where data has been imputed
  imputed_points <- is.na(COINr::get_dset(coin, dset = dset, also_get = "uName")) & !is.na(df_imputed)

  list(df_imputed = df_imputed, # df to display
       n_missing_data_pre = n_missing_data_pre,
       frac_missing_data_pre = frac_missing_data_pre,
       n_missing_data_post = n_missing_data_post,
       frac_missing_data_post = frac_missing_data_post,
       imputed_points = as.data.frame(imputed_points)
  )

}


#' DT table with highlighted cells
#'
#' Adds highlighted cells for imputed points, and summary row for number
#' of missing points per column (indicator).
#'
#'
#' @param df_display The data frame to display (imputed data)
#' @param df_highlight A data frame, same size as `df_display`, with only logical
#' values. Any cell with `TRUE` will highlight the corresponding cell in `df_display`.
#' @param highlight_background Colour to highlight background of highlighted cells
#' @param highlight_text Highlight text colour
#'
#' @return DT table
#' @export
f_style_imputation_table <- function(df_display, df_highlight, highlight_background = "#ffc266",
                                     highlight_text = "red"){

  stopifnot(identical(dim(df_display), dim(df_highlight)),
            is.data.frame(df_display),
            is.data.frame(df_highlight))

  column_types <- sapply(df_highlight, is.logical)
  stopifnot(is.logical(all(column_types)))

  ncol_display <- ncol(df_display)

  make_binary_for_DT <- function(x) {
    if(is.logical(x)) return(as.numeric(x))
    x
  }

  df_highlight_numeric <- lapply(df_highlight, make_binary_for_DT) |>
    as.data.frame()

  # join dfs: the latter will be hidden in the output
  X <- cbind(df_display, df_highlight_numeric)

  # highlight colours for 0 and 1 respectively
  highlight_colours <- c("white", highlight_background)
  text_colours <- c("black", highlight_text)

  # character vector for summarising n. missing per column
  n_per_col <- colSums(df_highlight)
  missing_summary <- as.character(n_per_col)
  missing_summary[1:2] <- c("", "Missing:")

  summary_row = htmltools::withTags(table(
    DT::tableHeader(X),
    DT::tableFooter(missing_summary)
  ))

  # The +1 and -1 here and elsewhere are to do with two things:
  # 1. That DT uses JS which uses 0 as the first index
  # 2. That the rownames are counted as a column unless removed
  # See https://rstudio.github.io/DT/ (note just above sec 2.5)
  DT::datatable(
    X,
    rownames = FALSE,
    caption = NULL,
    container = summary_row,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = list('copy', list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      )),
      columnDefs = list(
        list(
          visible=FALSE,
          targets=ncol_display:(ncol(X)-1)
        )
      ),
      scrollX = TRUE
    )
  ) |>
    DT::formatStyle(
      columns = 1:ncol_display,
      valueColumns = (ncol_display + 1):ncol(X),
      backgroundColor = DT::styleEqual(c(0,1), highlight_colours),
      color = DT::styleEqual(c(0,1), text_colours))

}

#' Summarise missing data points before imputation
#'
#' @param l The list `r_coin_imputed()` in the server
f_print_pre_imputation <-  function(l){

  n <- l$n_missing_data_pre
  prc <- round(l$frac_missing_data_pre*100)

  glue::glue("<b>Missing data points before imputation: {n} ({prc}%)<b>")

}

#' Summarise missing data points after imputation
#'
#' @param l The list `r_coin_imputed()` in the server
f_print_post_imputation <-  function(l){

  n <- l$n_missing_data_post
  prc <- round(l$frac_missing_data_post*100)

  glue::glue("<b>Missing data points after imputation: {n} ({prc}%)<b>")

}
