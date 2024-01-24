
# UI ----------------------------------------------------------------------

bubble_ui <- function(id) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Bubble chart",

      "Plot the values of any indicator/aggregate on the x and y axes,
    mapping other indicators to the bubble size and colour.",

      # variables to plot
      selectInput(ns("i_indicator_explore_bubble_x"), "Indicator for x-axis", choices = NULL),
      selectInput(ns("i_indicator_explore_bubble_y"), "Indicator for y-axis", choices = NULL),
      selectInput(ns("i_indicator_explore_bubble_size"), "Indicator for bubble size", choices = NULL),
      selectInput(ns("i_indicator_explore_bubble_iCode_group"), "Colour by group", choices = NULL),

      # log switches
      shinyWidgets::prettySwitch(ns("i_indicator_explore_log_x"), label = "Log x-axis", value = FALSE),
      shinyWidgets::prettySwitch(ns("i_indicator_explore_log_y"), label = "Log y-axis", value = FALSE),

      # threshold settings
      numericInput(ns("i_indicator_explore_threshold_x"), "Threshold for x-axis", value = NULL),
      numericInput(ns("i_indicator_explore_threshold_y"), "Threshold for y-axis", value = NULL),

      # units to highlight
      selectizeInput(
        inputId = ns("i_marked_country_unit"),
        "Highlight units", #
        choices = NULL,
        multiple = TRUE,
        selected = NULL,
        options = list(plugins= list('remove_button'))
      ),

      # buttons
      actionButton(ns("i_explore_bubble_submit_btn"), "Run")

    ),


    # MAIN PANEL
    card(
      full_screen = TRUE,
      card_header("Bubble chart"),
      height = "90vh",
      # MAP
      card_body(
        echarts4rOutput(ns("o_plot_bubble"), height = "80vh")
      )
    )
  )

}

# SERVER ------------------------------------------------------------------

bubble_server <- function(id, r_share, coin) {

  moduleServer(id, function(input, output, session) {

    # update inputs
    observe({

      req(coin())
      req(r_share$results_calculated)

      ind_group_choices <- get_indicator_codes(coin(), code_types = "all", with_levels = TRUE)

      updateSelectInput(inputId = "i_indicator_explore_bubble_x", choices = ind_group_choices)
      updateSelectInput(inputId = "i_indicator_explore_bubble_y", choices = ind_group_choices, selected = ind_group_choices[[2]])
      updateSelectInput(inputId = "i_indicator_explore_bubble_size", choices = ind_group_choices, selected = ind_group_choices[[3]])

      # TODO deal with possibility of no groups
      group_names <- f_get_group_list(coin())
      updateSelectInput(inputId = "i_indicator_explore_bubble_iCode_group", choices = group_names, selected = group_names)

    }) |> bindEvent(r_share$results_calculated)

    # bubble thresholds - get correct scales for X
    observeEvent(input$i_indicator_explore_bubble_x,{
      if(input$i_indicator_explore_bubble_x != ""){
        dfres <- get_results(coin(), dset = "Aggregated", tab_type = "Full",
                             dset_indicators = "Raw")
        minx <- min(dfres[[input$i_indicator_explore_bubble_x]], na.rm = TRUE)
        maxx <- max(dfres[[input$i_indicator_explore_bubble_x]], na.rm = TRUE)
        updateNumericInput(inputId = "i_indicator_explore_threshold_x",
                           min = minx, max = maxx, value = mean(c(minx, maxx)))
      }
    })

    # bubble thresholds - get correct scales for Y
    observeEvent(input$i_indicator_explore_bubble_y,{
      # bubble thresholds - get correct scales
      if(input$i_indicator_explore_bubble_y != ""){
        dfres <- get_results(coin(), dset = "Aggregated", tab_type = "Full",
                             dset_indicators = "Raw")
        miny <- min(dfres[[input$i_indicator_explore_bubble_y]], na.rm = TRUE)
        maxy <- max(dfres[[input$i_indicator_explore_bubble_y]], na.rm = TRUE)
        updateNumericInput(inputId = "i_indicator_explore_threshold_y",
                           min = miny, max = maxy, value = mean(c(miny, maxy)))
      }
    })

    # update groups for selected group iCode
    observe({
      req(r_share$results_calculated)

      group_choices <- f_get_group_choices(
        coin(),
        input$i_indicator_explore_bubble_iCode_group
      )

      updateSelectizeInput(
        inputId = "i_selected_groups",
        choices = group_choices,
        selected = group_choices)
    })

    # BUBBLE PLOT
    output$o_plot_bubble <- renderEcharts4r({

      req(r_share$results_calculated)
      axis_x <- if(input$i_indicator_explore_log_x) "log" else "value"
      axis_y <- if(input$i_indicator_explore_log_y) "log" else "value"

      f_plot_bubble(
        coin(),
        dset = "Aggregated",
        iCode_x = input$i_indicator_explore_bubble_x,
        iCode_y = input$i_indicator_explore_bubble_y,
        iCode_size = input$i_indicator_explore_bubble_size,
        threshold_x = input$i_indicator_explore_threshold_x,
        threshold_y = input$i_indicator_explore_threshold_y,
        iCode_group = input$i_indicator_explore_bubble_iCode_group,
        marked_country_unit = input$i_marked_country_unit,
        axis_x = axis_x,
        axis_y = axis_y
      )
    })  |>
      bindEvent(input$i_explore_bubble_submit_btn)

  })

}

# SUPPORTING FUNCTIONS ----------------------------------------------------

#' Bubble of an indicator/aggregate
#'
#' @param coin The coin
#' @param dset Name of data set containing the indicator/aggregate to plot
#' @param iCode The code of the indicator/aggregate to plot
#'
#' @import echarts4r
#'
#' @return An echarts4r plot
#' @noRd
f_plot_bubble <- function(coin,
                          dset = "Aggregated",
                          iCode_x = NULL,
                          iCode_y = NULL,
                          iCode_size = NULL,
                          threshold_x = NULL,
                          threshold_y = NULL,
                          iCode_group = NULL,
                          marked_country_unit = NULL,
                          axis_x = "value",
                          axis_y = "value"
) {


  stopifnot(!is.null(iCode_x))

  # ensure group code is valid
  iCodes_groups <- f_get_group_codes(coin)
  stopifnot(iCode_group %in% iCodes_groups)

  colors <- c("#fdee65",  pal_find()[2], pal_find()[1])

  # get data: include selected group and raw data for indicators
  bubble_df <- get_results(coin, dset = "Aggregated", tab_type = "Full",
                           also_get = c(iCode_group, "uName"), dset_indicators = "Raw")

  # remove NAs
  bubble_df <- bubble_df[!is.na(bubble_df[[iCode_x]]), ]
  bubble_df <- bubble_df[!is.na(bubble_df[[iCode_y]]), ]

  # check for non positive for log
  if(axis_x == "log"){
    if(any(bubble_df[[iCode_x]] <= 0)) axis_x <- "value"
  }
  if(axis_y == "log"){
    if(any(bubble_df[[iCode_y]] <= 0)) axis_y <- "value"
  }

  validate(
    need(nrow(bubble_df) > 0, "No data available for this selection...")
  )

  # axis names (add "log" if plotting log axis)
  xaxis_name <- COINr::icodes_to_inames(coin, iCode_x)
  if(axis_x == "log"){
    xaxis_name <- paste0(xaxis_name, " (log)")
  }
  yaxis_name <- COINr::icodes_to_inames(coin, iCode_y)
  if(axis_y == "log"){
    yaxis_name <- paste0(yaxis_name, " (log)")
  }

  # hack to appease CMD check: although dplyr recognises the .data pronoun,
  # echarts4r doesn't seem to.
  .data <- NULL

  p <-
    bubble_df |>
    dplyr::group_by(.data[[iCode_group]]) |>
    e_charts_( {{ iCode_x }} ) |>
    e_scatter_(
      {{ iCode_y }},
      scale = NULL,
      country_label = FALSE,
      emphasis = list(focus = "series", blurScope = "coordinateSystem"),
      opacity = 0.4,
      bind = "uName",
      itemStyle = list(borderWidth = 1, borderColor = "black"),
      size = {{ iCode_size }},
      symbol_size = 1,
      animation = TRUE
    ) |>
    e_legend(show = TRUE) |>
    e_grid(left = "50px", right = "60px", top = "85px", bottom = "90px") |>
    e_animation(show = F) |>
    e_toolbox() |>
    e_legend(top = "30px", type = "scroll") |>
    e_toolbox_feature(feature = "dataZoom") |>
    e_toolbox_feature(feature = "saveAsImage", name = "FIND Plot") |>
    e_y_axis(
      name = yaxis_name,
      type = axis_y,
      nameLocation = "middle",
      nameGap = 30#,
      #max = 100
    ) |>
    e_x_axis(
      name = xaxis_name,
      type = axis_x,
      nameLocation = "middle",
      nameGap = 35#,
      #max = 100
    ) |>
    e_mark_line(
      data = list(
        yAxis = threshold_y,
        lineStyle = list(color = pal_find(2)[2], width = 2, type = "solid")
      ),
      symbol = "none",
      silent = TRUE,
      title = "",
      animation = FALSE,
      title_position = "left"
    ) |>
    e_mark_line(
      data = list(
        xAxis = threshold_x,
        lineStyle = list(color = pal_find(2)[2], width = 2, type = "solid")
      ),
      symbol = "none",
      silent = TRUE,
      title = "",
      animation = FALSE,
      title_position = "left"
    ) |>
    e_grid(height = "80%") |>
    add_find_logo() |>
    e_theme_custom(pal_find_to_echarts()) |>
    e_tooltip(formatter = htmlwidgets::JS(tooltip_JS(
      xname = COINr::icodes_to_inames(coin, iCode_x),
      yname = COINr::icodes_to_inames(coin, iCode_y),
      sizename = COINr::icodes_to_inames(coin, iCode_size)
    )))

  add_marked_country_unit_to_plot <- function(p, marked_country_unit_i) {

    # hack to appease CMD check
    uCode <- uName <- NULL

    point <-
      bubble_df |>
      dplyr::filter(uCode == {
        marked_country_unit_i
      }) |>
      dplyr::transmute(
        xAxis = !!rlang::ensym(iCode_x),
        yAxis =  !!rlang::ensym(iCode_y),
        value = uName,
        name = uCode
      ) |>
      as.list()

    p |>
      e_mark_point(
        data = point,
        symbol = "pin",
        itemStyle = list(color = pal_find(3))
      )
  }

  p <- Reduce(add_marked_country_unit_to_plot, marked_country_unit, p, .data)

  p

}

tooltip_JS <- function(xname, yname, sizename){
  glue::glue(
    "function(params){{",
    "return('<strong>' + params.name + ",
    "'</strong><br />{xname}: ' + params.value[0] + ",
    "'<br />{yname}: ' + params.value[1] + ",
    "'<br />{sizename}: ' + params.value[2])}}"
  )
}

# Function converts a character vector of hex colour codes from the pal_find()
# function to a format recognised by e_theme_custom() (echarts). Previously
# this was done as as.character(jsonlite::toJSON(list(color = pal_find())))
# This function is there to remove the dependency on jsonlite.
pal_find_to_echarts <- function(){
  pf <- pal_find()
  str_out <- paste0(pf, collapse = '\",\"')
  str_out <- paste0('{\"color\":[\"', str_out, '\"]}')
  str_out
}
