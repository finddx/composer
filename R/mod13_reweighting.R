
# UI ----------------------------------------------------------------------

reweighting_ui <- function(id, sidebar_width) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = "30vw",
      title = "Weight adjustment",
      "Select the level that you wish to adjust the weights at, and use the sliders
      to set the weights. Click 'Recalculate' to calculate the index with these
      adjusted weights and compare it against the existing weight set. To use the adjusted
      weights in your index, save them with a name and go to the Compose tab to regenerate
      the full results there, selecting the new weight set.",

      card(
        card_header("Weights"),
        fill = FALSE,

        card_body(
          numericInput(
            ns("i_weight_slider_level"),
            label = "Select framework level to adjust weights",
            value = 3, min = 1, max = 3),

          uiOutput(ns("o_weights_sliders")),
          actionButton(ns("i_regenerate_button"), label = "Recalculate", icon = icon("calculator"))
        )
      ),

      strong("Save weights"),
      textInput(ns("i_weight_set_name"), "Enter name to save weight set (you must click 'Recalculate' first):"),
      actionButton(ns("i_save_weights_button"), label = "Save")

    ),


    # MAIN PANEL
    card(
      full_screen = TRUE,
      card_header("Rank comparison"),

      # COMPARISON TABLE
      card_body(
        DT::dataTableOutput(ns("df_comparison"))
      )
    )
  )

}

# SERVER ------------------------------------------------------------------

reweighting_server <- function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    # for storing the new weight set in
    weight_set <- reactiveVal(NULL)

    # generate weights sliders
    output$o_weights_sliders <- renderUI({
      req(r_share$results_calculated)
      # TODO set initial values to weight values inside coin (currently set at 1)
      weights_slider_group(coin(), input$i_weight_slider_level, ns_id = id)
    })

    # update level selection
    observe({
      req(coin())
      req(r_share$results_calculated)
      max_level <- coin()$Meta$maxlev
      updateNumericInput(
        inputId = "i_weight_slider_level",
        min = 1,
        max = max_level - 1,
        value = max_level - 1)
    })

    # calculate reweighted coin
    coin_reweighted <- reactive({

      req(r_share$results_calculated)
      # since I have to access multiple values of input, I have to convert
      # to a list - not sure if there is a better way
      l_input <- isolate(reactiveValuesToList(input))

      # extract weights from input list
      icodes_level <- get_iCodes(coin(), input$i_weight_slider_level)
      weight_set(l_input[icodes_level])

      # regenerate coin with new weights
      f_reweight_coin(coin(), weight_set())

    }) |> bindEvent(input$i_regenerate_button)

    # Comparison table
    output$df_comparison <- DT::renderDataTable({

      req(r_share$results_calculated)
      COINr::compare_coins(
        coin(),
        coin_reweighted(),
        dset = "Aggregated",
        iCode = get_index_code(coin()),
        also_get = "uName",
        sort_by = "coin.1"
      ) |> f_style_comparison_table()

    }, server = FALSE)

    # save weight set in coin
    observeEvent(input$i_save_weights_button, {
      req(coin())
      req(input$i_weight_set_name)
      req(r_share$results_calculated)

      # get weights currently used in aggregation
      w_new <- f_get_last_weights(coin())
      # update this df using reweighted weights
      w_new$Weight[match(names(weight_set()), w_new$iCode)] <- as.numeric(weight_set())
      # have to do a copy since for some reason cannot assign by name to the coin as reactive?
      coin_copy <- coin()
      # save to coin - note, can't add this directly with a name for some reason...
      coin_copy[["Meta"]][["Weights"]][[input$i_weight_set_name]] <- w_new
      coin(coin_copy)

      showNotification(
        ui = "Weight set successfully saved. Go to the 'Compose' tab to select this weight set for use in aggregation.",
        type = "default",
        duration = 10
      )

    })

  })

}

# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Regenerate results
#'
#' Regenerates results based on specified weights
#'
#' @param coin The coin
#' @param w can either be a named list with names as iCodes and values
#' as the new weights, OR as a data frame with columns "iCode" and "Weight" with
#' codes and corresponding weights.
#'
#' @importFrom COINr is.coin
#'
#' @return Updated coin
#' @export
f_reweight_coin <- function(coin, w){

  stopifnot(is.coin(coin))

  if(is.null(coin$Data$Aggregated)){
    stop("Can't find results in the coin. Did you forget to build the index first?", call. = FALSE)
  }

  # Get weights that were last used to aggregate ----

  w_new <- f_get_last_weights(coin)

  # Alter weights based on input type ----

  if(is.data.frame(w)){

    stopifnot(all(c("iCode", "Weight") %in% names(w)),
              all(w$iCode %in% w_new$iCode),
              is.numeric(w$Weight))

    # subst new weights in
    w_new$Weight[match(w$iCode, w_new$iCode)] <- w$Weight

  } else if (is.list(w)){

    stopifnot(all(names(w) %in% w_new$iCode),
              all(sapply(w, is.numeric)),
              all(lengths(w) == 1))

    # subst new weights in
    w_new$Weight[match(names(w), w_new$iCode)] <- as.numeric(w)

  }

  # Regen with new weights ----

  # subst weights back into log
  coin$Log$Aggregate$w <- w_new

  # extract analysis: otherwise will be lost
  ind_analysis <- coin$Analysis$Raw
  analysis_exists <- !is.null(ind_analysis)

  coin <- COINr::Regen(coin, from = "Aggregate")

  if(analysis_exists){
    coin$Analysis$Raw <- ind_analysis
  }

  coin

}

# Returns the last set of weights used when aggregating the index, as a data
# frame.
f_get_last_weights <- function(coin){

  if(is.null(coin$Log$Aggregate)){
    stop("Cannot return last weights: coin has not yet been aggregated.")
  }

  # w_log is the weighting specification it is not always the df of weights -
  # see below:

  w_log <- coin$Log$Aggregate$w

  if (is.null(w_log)) {

    # default to original weights
    w_new <- coin$Meta$Weights$Original

  } else if (is.character(w_log)) {

    # named weight set
    w_new <- coin$Meta$Weights[[w_log]]

  } else if (is.data.frame(w_log)) {

    w_new <- w_log

  } else {
    stop("Weights not recognised at coin$Log$Aggregate$w")
  }

  stopifnot(is.data.frame(w_new))

  w_new

}

# style comparison table (reweighted vs. current weights)
f_style_comparison_table <- function(df_compare){
  DT::datatable(
    df_compare,
    rownames = FALSE,
    colnames = c("Code", "Name", "Original rank", "Reweighted rank",
                 "Rank difference", "Abs. rank difference"),
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
    )
  )
}

