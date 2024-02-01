
# UI ----------------------------------------------------------------------

#' @importFrom echarts4r echarts4rOutput renderEcharts4r
#'
map_ui <- function(id, sidebar_width) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Map",

      "Plot the values of any indicator or aggregate on the map.",

      selectInput(
        ns("i_indicator_explore_map"),
        "Indicator",
        choices = NULL
      ),
      actionButton(ns("i_explore_map_submit_btn"), "Run")

    ),


    # MAIN PANEL
    card(
      full_screen = TRUE,
      card_header("Map"),
      height = "90vh",
      # MAP
      card_body(
        echarts4rOutput(ns("o_plot_map"))
      )
    )
  )

}

# SERVER ------------------------------------------------------------------

map_server <- function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    # update inputs
    observe({

      req(coin())
      req(r_share$results_calculated)
      ind_group_choices <- get_indicator_codes(coin(), code_types = "all", with_levels = TRUE)
      updateSelectInput(inputId = "i_indicator_explore_map", choices = ind_group_choices)

    }) |> bindEvent(r_share$results_calculated)

    # MAP
    output$o_plot_map <-  renderEcharts4r({

      req(r_share$results_calculated)

      # only plot map if country data detected, otherwise user message on click
      if(r_share$is_country_data){
        f_plot_map(coin(), dset = "Aggregated", iCode = input$i_indicator_explore_map)
      } else {
        # user notification
        showNotification(
          ui = "Cannot plot map. Map can only be plotted if the 'uCode' entries in your input data correspond to ISO3c codes.",
          type = "warning",
          duration = 10
        )
      }
    }) |> bindEvent(input$i_explore_map_submit_btn)

  })

}


# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Map of an indicator/aggregate
#'
#' @param coin The coin
#' @param dset Name of data set containing the indicator/aggregate to plot
#' @param iCode The code of the indicator/aggregate to plot
#'
#' @return An echarts4r plot
#' @noRd
f_plot_map <- function(coin, dset = "Aggregated", iCode = NULL, roam = TRUE) {

  stopifnot(!is.null(iCode))

  indicator <- rlang::enexpr(iCode)

  colors <- c(pal_find()[14],  pal_find()[12], pal_find()[2])

  map_df <- get_dset(coin, dset) |>
    COINr::round_df()

  # hack to appease CMD check
  uCode <- NULL

  map_df |>
    e_country_names(uCode, type = "iso3c") |>
    e_charts(uCode) |>
    e_map_(indicator,
           roam = roam,
           bind = "updated",
           zoom = 1,
           #center = c(0, 10),
           top = "0px",
           bottom = "98px"
    ) |>
    e_toolbox() |>
    e_toolbox_feature(feature = "saveAsImage", name = "FIND_map") |>
    e_visual_map_(
      indicator,
      type = "piecewise",
      bottom = "130px",
      left = "0px",
      calculable = FALSE,
      inRange = list(color = colors), # scale colors
    ) |>
    #add_find_logo() |>
    e_tooltip()

}
