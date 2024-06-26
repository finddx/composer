# UI ----------------------------------------------------------------------

compose_ui <- function(id, sidebar_width) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Compose Index",

      "Aggregate indicators using one of the methods selected below. Optionally,
    set a data availability threshold to calculate aggregate scores at each level.",

      selectInput(
        ns("i_aggregation_type"),
        "Select aggregation type",
        choices = c("Weighted arithmetic mean" = "a_amean",
                    "Weighted geometric mean" = "a_gmean"
        ),
        selected = "a_amean"
      ),
      selectInput(ns("i_weight_set"), "Weight set", choices = NULL),
      sliderInput(ns("i_min_data_availability"), "Min. data availability", 0, 1, .1),
      actionButton(ns("i_compose_btn"), "Run")

    ),


    # MAIN PANEL
    card(
      height = "90vh",
      full_screen = TRUE,
      card_header("Results"),
      # RESULTS TABLE
      card_body(
        DT::dataTableOutput(ns("indicator_table"))
      )
    )
  )

}

# SERVER ------------------------------------------------------------------

compose_server <- function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    # populate with weight sets
    observe({
      req(coin())
      weight_set <- get_weight_sets_names(coin())
      selected_weight_set <- if(is.null(coin()$Log$Aggregate$w)){
        weight_set[1]
      } else {
        coin()$Log$Aggregate$w
      }
      updateSelectInput(inputId = "i_weight_set", choices = weight_set,
                        selected = coin()$Log$Aggregate$w) # the last used weight set
    })

    # calculate results on button click
    observeEvent(input$i_compose_btn, {

      req(coin())
      # check for incompatibility: geometric mean with neg/zero values
      # if this is the case, send error to user

      can_aggregate <- f_check_can_aggregate(coin(), input[["i_aggregation_type"]])

      if (can_aggregate) {

        # check if normalisation has been run first, if not return error
        if(!r_share$data_is_normalised){
          showNotification(
            ui = glue::glue(
              "Cannot aggregate because you have not yet normalised your data. ",
              "Go to the 'Normalisation' tab and normalise first."
            ),
            type = "error",
            duration = 10
          )
          req(FALSE)
        }

        coin_aggregated <- f_aggregation(
          coin(),
          dset = "Normalised",
          aggregation_type = input$i_aggregation_type,
          weight_set = input$i_weight_set,
          min_dat_avail = input$i_min_data_availability
        )
        make_dset_toast("Aggregated")

        # regenerate any subsequent ops if needed
        coin_aggregated <- regen_outdated_coin(coin_aggregated, "f_aggregation")
        # update coin, set results flag
        coin(coin_aggregated)
        r_share$results_calculated <- TRUE

      } else {

        showNotification(
          ui = glue::glue(
            "Cannot aggregate using geometric mean because negative or zero values ",
            "found in the normalised data. Adjust normalisation method ",
            "or parameters or change the aggregation method."
          ),
          type = "error",
          duration = 10
        )
        req(FALSE)

      }
    })

    # get sorted results table
    df_aggregated <- reactive({
      req(r_share$results_calculated)
      get_results(coin(), dset = "Aggregated", tab_type = "Aggs", also_get = "uName")
    })

    # render aggregated data table
    output$indicator_table <- DT::renderDataTable({
      req(df_aggregated())
      f_style_compose_table(df_aggregated())
    }, server = FALSE) # server FALSE means whole dataframe passed to client rather than one page at a time (enables table download)

  })

}

# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Outputs for aggregation sub-tab
#'
#' This takes a coin and aggregates the selected data set. It returns the updated
#' coin plus a data frame of results that is sorted from highest to lowest-scoring
#' units.
#'
#' By default this will use the "Normalised" data set and in fact if this dset is
#' not present it will show a warning. This is because aggregating indicators that are
#' not normalised is usually a bad idea - they may be on completely different scales.
#' We could make normalisation a hard constraint in the app. To discuss.
#'
#' As it is this function will only allow one aggregation type for all levels. Although COINr can
#' take a different aggregation type for each level, we could move to that later if needed. The
#' `aggregation_type` should be a dropdown with two entries (`"a_mean"` and `"g_mean"`).
#'
#' `weight_set` should be a dropdown which consists of the entries returned by `get_weight_sets_names()`.
#' This will return a character vector of names of any weight sets in the coin, plus "Equal". Weight
#' sets can be generated by the user in a later tab.
#'
#' The `min_dat_avail` argument should be a slider input between 0 and 1, with default 1. This controls
#' the proportion of data that is needed *within each aggregation group* to calculate an aggregate score.
#' See the [aggregate vignette in COINr for more info](https://bluefoxr.github.io/COINr/articles/aggregate.html#data-availability-limits).
#'
#' On the same tab there should also be a bar chart output and possibly a map, although I will make
#' separate functions for these.
#'
#' @param coin The coin
#' @param dset Data set to operate on. Leave `NULL` unless we allow to change operation order.
#' @param aggregation_type One of `"a_amean"` (arithmetic mean) or `"a_gmean"` (geometric mean).
#' @param weight_set A string: any one of the entries returned by `get_weight_sets_names()`.
#' @param min_dat_avail A number between 0 and 1.
#'
#' @importFrom COINr get_results
#'
#' @return A list with `.$coin` the coin and `.$df_aggregated` the data frame of results to display.
#'
f_aggregation <- function(coin, dset = NULL, aggregation_type, weight_set = NULL, min_dat_avail = NULL) {

  dset <- dset %||% f_select_dset(coin, "f_aggregation")

  # TODO we can make the following a hard constraint (data must be normalised) but if not at least a warning.
  if (dset != "Normalised") {
    warning("You are aggregating data that has not been normalised. This is not normally recommended.")
  }

  stopifnot(aggregation_type %in% c("a_amean", "a_gmean"))

  # if equal weights, we build the df
  if(identical(weight_set, "Equal")){
    weight_set<- get_equal_weights_df(coin)
  }

  # If weight_set or min_dat_avail are NULL this is dealt with in COINr:Aggregate
  # However we can set defaults for these in the UI.
  coin <- COINr::Aggregate(
    coin,
    dset = dset,
    f_ag = aggregation_type,
    w = weight_set,
    dat_thresh = min_dat_avail,
    out2 = "coin",
    write_to = "Aggregated"
  )

  # get full results to attach to coin
  coin <- get_results(
    coin,
    dset = "Aggregated",
    tab_type = "Full",
    also_get = "uName", dset_indicators = "Raw", out2 = "coin")

  coin

}

# Returns a character vector of weight set names that is passed to f_aggregation
# as well as "Equal".
get_weight_sets_names <- function(coin){
  c(names(coin$Meta$Weights), "Equal")
}

# returns a data frame of equal weights which can be passed to COINr::Aggregate()
get_equal_weights_df <- function(coin){

  w <- coin$Meta$Weights$Original
  stopifnot(!is.null(w))

  w$Weight <- 1

  w
}

# Creates a DT table for display in the Compose tab.
f_style_compose_table <- function(df_aggregated){

  # find min and max of score ranges ----
  all_columns <- names(df_aggregated)
  factor_columns <-  c("uCode", "uName", "Rank")
  numeric_columns <- setdiff(all_columns, factor_columns)
  df_numeric <- as.matrix(df_aggregated[numeric_columns])
  min_all <- min(df_numeric, na.rm = TRUE)
  max_all <- max(df_numeric, na.rm = TRUE)

  # generate colours ----
  breaks <- seq(min_all, max_all, length.out = 12)[2:11]
  colour_func <- grDevices::colorRampPalette(c("white", "#6dc3c8"))
  colour_palette <- colour_func(length(breaks) + 1)


  # create formatted table ----
  DT::datatable(
    df_aggregated,
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      pageLength = 20,
      buttons =
        list("copy", list(
          extend = 'collection',
          buttons = list(list(extend = 'csv',
                              filename = 'composer_results',
                              title = "Results"), # note: any spaces here result in weird output!
                         list(extend = 'excel',
                              filename = 'composer_results',
                              title = "Results"),
                         list(extend = 'pdf',
                              filename = 'composer_results',
                              title = "Results")),
          text = 'Download'
        ))
    )
  ) |>
    DT::formatStyle(
      numeric_columns,
      backgroundColor = DT::styleInterval(breaks, colour_palette)
    )

}

#' Check whether aggregation is possible
#'
#' Checks whether normalised data is compatible with aggregation method.
#' The only incompatibility is using geometric mean with data containing zeroes
#' or negative numbers (due to log()). In this case returns FALSE, otherwise TRUE.
#'
#' @return FALSE if cannot aggregate, otherwise TRUE
#' @noRd
f_check_can_aggregate <- function(coin, agg_method) {

  if (agg_method != "a_gmean") return(TRUE)

  min_normalised_data <- get_dset(coin, dset = "Normalised", also_get = "none") |>
    as.matrix() |>
    min(na.rm = TRUE)

  can_aggregate <- (min_normalised_data > 0)

  can_aggregate

}

