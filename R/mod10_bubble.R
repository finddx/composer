
# UI ----------------------------------------------------------------------

bubble_ui <- function(id, sidebar_width) {

  ns <- NS(id)

  page_sidebar(

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Bubble chart",

      "Plot the values of any indicator/aggregate on the x and y axes,
    mapping other indicators to the bubble size and colour.",

      # variables to plot
      selectInput(ns("i_indicator_explore_bubble_dsetx"), "Dataset for x-axis",
                  choices = list("Normalised scores" = "Aggregated", "Raw values" = "Raw")),
      selectInput(ns("i_indicator_explore_bubble_x"), "Indicator for x-axis", choices = NULL),
      selectInput(ns("i_indicator_explore_bubble_dsety"), "Dataset for y-axis",
                  choices = list("Normalised scores" = "Aggregated", "Raw values" = "Raw")),
      selectInput(ns("i_indicator_explore_bubble_y"), "Indicator for y-axis", choices = NULL),
      selectInput(ns("i_indicator_explore_bubble_size"), "Indicator for bubble size", choices = NULL),
      selectInput(ns("i_indicator_explore_bubble_iCode_group"), "Colour by group", choices = NULL),

      # log switches
      input_switch(ns("i_indicator_explore_log_x"), label = "Log x-axis", value = FALSE),
      input_switch(ns("i_indicator_explore_log_y"), label = "Log y-axis", value = FALSE),

      # threshold settings
      input_switch(ns("enable_lines"), label = "Enable threshold lines", value = FALSE),
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

      ind_group_choices_all <- get_indicator_codes(coin(), code_types = "all", with_levels = TRUE)

      # POINT SIZE
      updateSelectInput(inputId = "i_indicator_explore_bubble_size",
                        choices = c("-- Don't map to size --", ind_group_choices_all))

      # POINT GROUPING (COLOUR)
      group_names <- f_get_group_list(coin())
      if(is.null(group_names)){
        updateSelectInput(inputId = "i_indicator_explore_bubble_iCode_group", label = "<No groups found in input data>")
        shinyjs::disable("i_indicator_explore_bubble_iCode_group")
      } else {
        shinyjs::enable("i_indicator_explore_bubble_iCode_group")
        updateSelectInput(inputId = "i_indicator_explore_bubble_iCode_group", choices = c("-- Don't group --", group_names))
      }

      # Unit highlighting
      df_agg <- get_dset(coin(), dset = "Aggregated", also_get = "uName")
      l_units <- as.list(df_agg$uCode)
      names(l_units) <- df_agg$uName
      updateSelectInput(inputId = "i_marked_country_unit", choices = l_units)

    }) |>
      bindEvent(r_share$results_calculated)

    # update indicator dropdown for X
    observe({

      req(coin())
      req(r_share$results_calculated)

      code_types_x <- if(input$i_indicator_explore_bubble_dsetx == "Raw") "Indicator" else "all"
      ind_group_choices_x <- get_indicator_codes(coin(), code_types = code_types_x, with_levels = TRUE)

      updateSelectInput(inputId = "i_indicator_explore_bubble_x", choices = ind_group_choices_x)

    })

    # update indicator dropdown for Y
    observe({
      req(coin())
      req(r_share$results_calculated)

      code_types_y <- if(input$i_indicator_explore_bubble_dsety == "Raw") "Indicator" else "all"
      ind_group_choices_y <- get_indicator_codes(coin(), code_types = code_types_y, with_levels = TRUE)

      updateSelectInput(inputId = "i_indicator_explore_bubble_y", choices = ind_group_choices_y,
                        selected = ind_group_choices_y[[2]])
    })

    # disable/enable line boxes
    observeEvent(input$enable_lines, {
      if(input$enable_lines){
        shinyjs::enable("i_indicator_explore_threshold_x")
        shinyjs::enable("i_indicator_explore_threshold_y")
      } else {
        shinyjs::disable("i_indicator_explore_threshold_x")
        shinyjs::disable("i_indicator_explore_threshold_y")
      }
    })

    # bubble thresholds - get correct scales for X
    observeEvent(input$i_indicator_explore_bubble_x,{
      if(input$i_indicator_explore_bubble_x != ""){
        dfres <- get_results(coin(), dset = "Aggregated", tab_type = "Full",
                             dset_indicators = input$i_indicator_explore_bubble_dsetx)
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
                             dset_indicators = input$i_indicator_explore_bubble_dsety)
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

      # point colour grouping
      enable_groups <- !(input$i_indicator_explore_bubble_iCode_group == "-- Don't group --")

      if(enable_groups){
        iCode_group <- input$i_indicator_explore_bubble_iCode_group

        # check number in group is not too high
        n_in_group <- coin()$Meta$Unit[[input$i_indicator_explore_bubble_iCode_group]] |>
          unique() |>
          length()

        # if too high, tell user and stop
        if(n_in_group > 8){
          showNotification(
            ui = "Cannot plot chart: selected group variable has too many groups (> 8) for effective colour mapping.",
            type = "warning",
            duration = 10
          )
          req(FALSE)
        }
      } else {
        iCode_group <- NULL
      }

      # point size mapping
      enable_size <- !(input$i_indicator_explore_bubble_size == "-- Don't map to size --")
      iCode_size <- if(enable_size){
        input$i_indicator_explore_bubble_size
      } else {
        NULL
      }

      f_plot_bubble(
        coin(),
        dset = "Aggregated",
        iCode_x = input$i_indicator_explore_bubble_x,
        iCode_y = input$i_indicator_explore_bubble_y,
        dset_x = input$i_indicator_explore_bubble_dsetx,
        dset_y = input$i_indicator_explore_bubble_dsety,
        iCode_size = iCode_size,
        threshold_x = input$i_indicator_explore_threshold_x,
        threshold_y = input$i_indicator_explore_threshold_y,
        iCode_group = iCode_group,
        marked_country_unit = input$i_marked_country_unit,
        axis_x = axis_x,
        axis_y = axis_y,
        enable_groups = enable_groups,
        enable_size = enable_size,
        enable_lines = input$enable_lines
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
                          dset_x = NULL,
                          dset_y = NULL,
                          iCode_size = NULL,
                          threshold_x = NULL,
                          threshold_y = NULL,
                          iCode_group = NULL,
                          marked_country_unit = NULL,
                          axis_x = "value",
                          axis_y = "value",
                          enable_size = TRUE,
                          enable_groups = TRUE,
                          enable_lines = FALSE
) {


  stopifnot(!is.null(iCode_x))

  if(enable_groups){

    # check groups in coin
    iCodes_groups <- f_get_group_codes(coin)

    if(is.null(iCodes_groups)){
      enable_groups <- FALSE # cannot group, no groups
    } else {
      stopifnot(iCode_group %in% iCodes_groups)
    }
  }


  colors <- c("#fdee65",  pal_find()[2], pal_find()[1])

  # get data: include selected group and raw data for indicators
  # bubble_df <- get_results(coin, dset = "Aggregated", tab_type = "Full",
  #                          also_get = c(iCode_group, "uName"), dset_indicators = "Raw")

  # separately collect x and y variables, rename so that merge works in case same variable twice
  dfx <- COINr::get_data(coin, dset = dset_x, iCodes = iCode_x, also_get = c(iCode_group, "uName"))
  colname_x <- paste0(iCode_x, "_", dset_x)
  names(dfx)[names(dfx) == iCode_x] <- colname_x

  # y variable data
  dfy <- COINr::get_data(coin, dset = dset_y, iCodes = iCode_y, also_get = c(iCode_group, "uName"))
  colname_y <- paste0(iCode_y, "_", dset_y)
  names(dfy)[names(dfy) == iCode_y] <- colname_y

  bubble_df <- if(colname_x != colname_y){
    base::merge(dfx, dfy)
  } else {
    # in case we have same iCode and dset for both x and y
    dfx
  }

  # bubble size data
  if(!is.null(iCode_size)){
    dfsize <- COINr::get_data(coin, dset = "Aggregated", iCodes = iCode_size)
    bubble_df <- base::merge(bubble_df, dfsize)
  }

  # remove NAs
  bubble_df <- bubble_df[!is.na(bubble_df[[colname_x]]), ]
  bubble_df <- bubble_df[!is.na(bubble_df[[colname_y]]), ]

  # round
  bubble_df <- COINr::signif_df(bubble_df, 4)

  # check for non positive for log
  if(axis_x == "log"){
    if(any(bubble_df[[colname_x]] <= 0)) axis_x <- "value"
  }
  if(axis_y == "log"){
    if(any(bubble_df[[colname_y]] <= 0)) axis_y <- "value"
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

  if(enable_groups){
    bubble_df <- dplyr::group_by(bubble_df, .data[[iCode_group]])
  }

  if(enable_size){
    sizename <- COINr::icodes_to_inames(coin, iCode_size)
    symbol_size <- 4
    min_size <- min(bubble_df[[iCode_size]], na.rm = TRUE)
    max_size <- max(bubble_df[[iCode_size]], na.rm = TRUE)
  } else {
    sizename <- NULL
    symbol_size <- 10
  }

  # point size scaling function
  # min and max here are min/max point sizes
  # NOTE: this requires passing in the global min and max for the size vector
  # because echarts passes data one group at a time.
  min_bubble_size <- 1
  max_bubble_size <- 50
  point_scaler <- function(x){
    (x-min_size)/(max_size - min_size)*(max_bubble_size - min_bubble_size) + min_bubble_size
  }

  p <-
    bubble_df |>
    e_charts_( {{ colname_x }} ) |>
    e_scatter_(
      {{ colname_y }},
      scale = point_scaler,
      country_label = FALSE,
      emphasis = list(focus = "series", blurScope = "coordinateSystem"),
      opacity = 0.4,
      bind = "uName",
      itemStyle = list(borderWidth = 1, borderColor = "black"),
      size = {{ iCode_size }},
      symbol_size = symbol_size,
      animation = TRUE
    ) |>
    e_legend(show = enable_groups) |>
    e_grid(left = "50px", right = "60px", top = "85px", bottom = "90px") |>
    e_animation(show = F) |>
    e_toolbox() |>
    e_legend(top = "30px", type = "scroll") |>
    e_toolbox_feature(feature = "dataZoom") |>
    e_toolbox_feature(feature = "saveAsImage", name = "composer plot") |>
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
    e_grid(height = "80%") |>
    e_theme_custom(pal_find_to_echarts()) |>
    e_tooltip(formatter = htmlwidgets::JS(tooltip_JS(
      xname = COINr::icodes_to_inames(coin, iCode_x),
      yname = COINr::icodes_to_inames(coin, iCode_y),
      sizename = sizename
    )))

  # turn on lines
  if(enable_lines){
    p <- p |>
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
      )
  }

  add_marked_country_unit_to_plot <- function(p, marked_country_unit_i) {

    # hack to appease CMD check
    uCode <- uName <- NULL

    point <-
      bubble_df |>
      dplyr::filter(uCode == {
        marked_country_unit_i
      }) |>
      dplyr::transmute(
        xAxis = !!rlang::ensym(colname_x),
        yAxis =  !!rlang::ensym(colname_y),
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

  p <- Reduce(add_marked_country_unit_to_plot, marked_country_unit, p)

  p

}

tooltip_JS <- function(xname, yname, sizename = NULL){

  if(is.null(sizename)){
    glue::glue(
      "function(params){{",
      "return('<strong>' + params.name + ",
      "'</strong><br />{xname}: ' + params.value[0] + ",
      "'<br />{yname}: ' + params.value[1])}}"
    )
  } else {
    glue::glue(
      "function(params){{",
      "return('<strong>' + params.name + ",
      "'</strong><br />{xname}: ' + params.value[0] + ",
      "'<br />{yname}: ' + params.value[1] + ",
      "'<br />{sizename}: ' + params.value[2])}}"
    )
  }
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
