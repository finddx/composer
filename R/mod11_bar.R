
# UI ----------------------------------------------------------------------

bar_ui <- function(id) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Bar chart",

      "Plot the values of any indicator/aggregate as a sorted bar chart, optionally
    showing the underlying component scores (only for aggregates).",

      selectInput(ns("i_indicator_explore_bar"), "Indicator", choices = NULL),

      shinyWidgets::prettySwitch(ns("i_stack_explore_bar"), label = "Show component scores"),
      shinyWidgets::prettySwitch(ns("i_explore_bar_vert"), label = "Make vertical", value = FALSE),
      shinyWidgets::prettySwitch(ns("i_explore_bar_uname"), label = "Use unit names", value = TRUE),

      selectInput(
        ns("i_explore_bar_subset"),
        label = "Plot subset",
        choices = list("All" = "all", "Top 25" = "top25", "Top 50" = "top50",
                       "Bottom 25" = "bot25", "Bottom 50" = "bot50")
      ),

      actionButton(ns("i_explore_bar_submit_btn"), "Run")

    ),


    # MAIN PANEL
    card(
      full_screen = TRUE,
      card_header("Bar chart"),
      height = "90vh",

      # BAR CHART
      card_body(
        plotlyOutput(ns("o_plot_bar"), height = "80vh")
      )
    )
  )

}

# SERVER ------------------------------------------------------------------

bar_server <- function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    # update indicator dropdown
    observe({
      req(coin())
      req(r_share$results_calculated)
      ind_group_choices <- get_indicator_codes(coin(), code_types = "all", with_levels = TRUE)
      updateSelectInput(inputId = "i_indicator_explore_bar", choices = ind_group_choices)
    }) |>
      bindEvent(r_share$results_calculated)

    # BAR CHART
    output$o_plot_bar <-  renderPlotly({
      req(r_share$results_calculated)
      f_plot_bar(
        coin(),
        iCode = input$i_indicator_explore_bar,
        stack_children = input$i_stack_explore_bar,
        make_vertical = input$i_explore_bar_vert,
        use_names = input$i_explore_bar_uname,
        plot_subset = input$i_explore_bar_subset)
    }) |>
      bindEvent(input$i_explore_bar_submit_btn)

  })

}

# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Bar chart of selected indicator
#'
#' Wrapper for bar chart: selects the correct data set and plotting mode
#' then passes to iCOINr.
#' Arguments are quick conversions to iCOINr inputs - see iCOINr documentation.
#'
#' @return Plotly bar chart
#' @noRd
f_plot_bar <- function(coin, iCode, stack_children, make_vertical,
                       use_names, plot_subset) {

  iCode_level <- get_icode_level(coin, iCode)

  dset <- if (iCode_level == 1) "Raw" else "Aggregated"

  # override stack plot if less than 2 children
  # note stack_children is set by a user switch
  if (get_number_of_children(coin, iCode) < 2) {
    stack_children <- FALSE
  }

  stopifnot(is.logical(make_vertical),
            is.logical(use_names))

  orientation <- if(make_vertical) "vertical" else "horizontal"
  ulabs <- if(use_names) "uName" else "uCode"

  # covert to iCOINr input
  plot_subset <- switch(
    plot_subset,
    all = NULL,
    top25 = 25,
    top50 = 50,
    bot25 = -25,
    bot50 = -50
  )

  iCOINr::iplot_bar(
    coin,
    dset = dset,
    iCode = iCode,
    ulabs = ulabs,
    stack_children = stack_children,
    ilabs = "iName",
    orientation = orientation,
    plot_subset = plot_subset
  )
}
