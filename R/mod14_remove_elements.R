# UI ----------------------------------------------------------------------

remove_elements_ui <- function(id, sidebar_width) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Remove elements",

      "Select a level of the index and run the analysis. The bar chart shows the
      mean absolute rank change as a result of removing each indicator/aggregate
      in that level",

      numericInput(
        ns("i_remove_elem_level"),
        label = "Select level",
        value = 1, min = 1, max = 1, step = 1),

      actionButton(ns("i_remove_elem_bttn"), label = "Run")

    ),


    # MAIN PANEL
    card(
      full_screen = TRUE,
      card_header("Impact of removing elements"),
      max_height = "800px",
      # MAP
      card_body(
        plotly::plotlyOutput(ns("remove_elements_bar"))
      )
    )
  )

}

# SERVER ------------------------------------------------------------------

remove_elements_server <- function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    # update levels dropdown
    observe({
      req(coin())
      req(r_share$results_calculated)
      n_levels <- get_n_levels(coin())
      updateNumericInput(
        inputId = "i_remove_elem_level",
        max = n_levels - 1, value = n_levels - 1)
    })

    # BAR CHART
    output$remove_elements_bar <- plotly::renderPlotly({

      req(coin())
      req(r_share$results_calculated)

      l_remove <- f_remove_elements(coin(), input$i_remove_elem_level)
      f_plot_bar_remove_elements(l_remove, coin())

    }) |> bindEvent(input$i_remove_elem_bttn)

  })

}

# SUPPORTING FUNCTIONS ----------------------------------------------------

# run remove elements for a given level
f_remove_elements <- function(coin, at_level){

  # code at top level
  index_code <- get_iCodes(coin, get_n_levels(coin))

  COINr::remove_elements(
    coin,
    Level = at_level,
    dset = "Aggregated",
    iCode = index_code
  )
}

#' Bar of remove elements analysis
#'
#' plot bar of remove elements analysis
#' note I keep this separate from the previous function in case we want to use
#' the output of remove_elements for other things.
#'
#' @param l_remove The list output from `f_remove_elements()`
#' @param coin The coin
#'
#' @noRd
f_plot_bar_remove_elements <- function(l_remove, coin){

  # get vector of interest, remove nominal change (always zero)
  x_plot <- l_remove$MeanAbsDiff
  x_plot <- x_plot[names(x_plot) != "Nominal"]

  x_names <- COINr::icodes_to_inames(coin, names(x_plot))

  hovertext <- glue::glue("<b>{x_names}</b><br>{round(x_plot, 2)}")

  plotly::plot_ly(
    x = names(x_plot),
    y = x_plot,
    name = "Mean abs. diff.",
    text = hovertext,
    type = "bar",
    hoverinfo = "text",
    textposition = "none"
  ) |>
    plotly::layout(
      yaxis = list(title = "Mean absolute rank change")
    )

}
