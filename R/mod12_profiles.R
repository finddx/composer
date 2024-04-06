
# UI ----------------------------------------------------------------------

profiles_ui <- function(id, sidebar_width) {

  ns <- NS(id)

  page_sidebar(

    fillable = FALSE,

    # SIDEBAR
    sidebar = sidebar(
      width = sidebar_width,
      title = "Unit profiles",

      "Select a unit to view a summary of its scores, ranks, and low- and high-
    ranking indicators.",

      selectInput(ns("i_units"),
                  "Select units",
                  choices = NULL
      ),
      input_switch(
        ns("i_stack_cp_bar"),
        label = "Show component scores (bar chart)",
        value = TRUE
      ),
      selectInput(ns("i_profiles_ulabs"), label = "Unit labels",
                  choices = c("Names", "Truncated names", "Codes")),
      actionButton(ns("i_countryprofile_submit_btn"), "Run"),
      downloadButton(ns("o_download_unit_report"), "Download Report")

    ),


    # MAIN PANEL
    h1(textOutput(ns("unit_name"))),
    hr(),

    layout_columns(

      col_widths = c(4, 8, 12, 6, 6),
      #row_heights = c(2, 5, 4),

      # INFO BOXES
      layout_columns(
        col_widths = c(12, 12),
        row_heights = c(1, 1),

        value_box(
          max_height = "150px",
          title = "Rank",
          value = textOutput(ns("o_rank")),
          theme = "text-info",
          showcase = icon("ranking-star"), showcase_layout = "left center",
          full_screen = FALSE, fill = TRUE
        ),

        value_box(
          max_height = "150px",
          title = "Score",
          value = textOutput(ns("o_index")),
          theme = "text-info",
          showcase = icon("chart-simple"), showcase_layout = "left center",
          full_screen = FALSE, fill = TRUE
        )
      ),

      # SCORE TABLE
      card(
        max_height = "320px",
        full_screen = TRUE,
        card_header("Score and rank summary"),
        card_body(
          DT::dataTableOutput(ns("o_table_country"))
        )
      ),

      # BAR CHART
      card(
        #height = "300px",
        #min_height = "300px",
        full_screen = TRUE,
        card_header("Bar chart"),
        card_body(
          plotlyOutput(ns("o_plot_bar"), height = "40vh")
        )
      ),

      # STRENGTHS
      card(
        full_screen = TRUE,
        card_header("Top-ranking indicators"),
        card_body(
          DT::dataTableOutput(ns("o_table_country_strengths"))
        )
      ),

      # WEAKNESSES
      card(
        full_screen = TRUE,
        card_header("Bottom-ranking indicators"),
        card_body(
          DT::dataTableOutput(ns("o_table_country_weaknesses"))
        )
      )
    )
  )

}

# SERVER ------------------------------------------------------------------

profiles_server <- function(id, r_share, coin) {

  .data <- uCode <- uName <- NULL

  moduleServer(id, function(input, output, session) {

    observe({
      req(coin())
      req(r_share$results_calculated)

      df_results <- get_results(coin(), dset = "Aggregated", tab_type = "Aggs", also_get = "uName")
      countries <- stats::setNames(df_results$uCode, df_results$uName)
      updateSelectInput(inputId = "i_units", choices = countries)
    })

    r_unit_df <- reactive({
      req(r_share$results_calculated)
      unit_results <- get_results(coin(), dset = "Aggregated", tab_type = "Aggs", also_get = "uName") |>
        dplyr::filter(.data$uCode == input$i_units)
    })

    # UNIT NAME
    output$unit_name <- renderText({
      req(r_share$results_calculated)
      r_unit_df()[["uName"]]
    }) |>
      bindEvent(input$i_countryprofile_submit_btn)

    # SCORE BOX
    output$o_index <- renderText({
      req(r_share$results_calculated)
      index_iCode <- get_iCodes(coin(), coin()$Meta$maxlev)
      r_unit_df()[[index_iCode]]
    }) |>
      bindEvent(input$i_countryprofile_submit_btn)

    # RANK BOX
    output$o_rank <- renderText({
      req(r_share$results_calculated)
      paste0(r_unit_df()$Rank, "/", nrow(coin()$Data$Aggregated))
    }) |>
      bindEvent(input$i_countryprofile_submit_btn)

    # BAR CHART
    output$o_plot_bar <- renderPlotly({

      req(r_share$results_calculated)

      ulabs <- if(input$i_profiles_ulabs == "Codes") "uCode" else "uName"
      trunc_ulabs <- (input$i_profiles_ulabs == "Truncated names")

      iCOINr::iplot_bar(
        coin(),
        dset = "Aggregated",
        iCode = get_iCodes(coin(), coin()$Meta$maxlev),
        usel = input$i_units,
        ulabs = ulabs,
        stack_children = input$i_stack_cp_bar,
        ilabs = "iName",
        trunc_ulabs = trunc_ulabs,
        colour_pal = pal_find()
      )
    }) |>
      bindEvent(input$i_countryprofile_submit_btn)

    # UNIT SUMMARY TABLE
    output$o_table_country <- DT::renderDataTable({
      req(r_share$results_calculated)
      report_levels <- setdiff(1:coin()$Meta$maxlev, 1)
      COINr::get_unit_summary(coin(), usel = input$i_units,
                              Levels = report_levels, dset = "Aggregated")
    }, rownames = FALSE, options = list(dom = 't')) |>
      bindEvent(input$i_countryprofile_submit_btn)

    # STRENGTHS TABLE
    output$o_table_country_strengths <- DT::renderDataTable({
      req(r_share$results_calculated)
      COINr::get_str_weak(coin(), dset = "Aggregated", usel = input$i_units)$Strengths
    }, rownames = FALSE, options = list(dom = 't')) |>
      bindEvent(input$i_countryprofile_submit_btn)

    # WEAKNESSES TABLE
    output$o_table_country_weaknesses <- DT::renderDataTable({
      req(r_share$results_calculated)
      COINr::get_str_weak(coin(), dset = "Aggregated", usel = input$i_units)$Weaknesses
    }, rownames = FALSE, options = list(dom = 't')) |>
      bindEvent(input$i_countryprofile_submit_btn)

    # UNIT REPORT
    output$o_download_unit_report <- downloadHandler(
      filename = function() {
        paste0(r_unit_df()$uName, "_unit-report_", Sys.Date(), ".html")
      },
      content = function(file) {
        req(coin())
        make_unit_profile(
          coin(),
          usel = input$i_units,
          stack_children = input$i_stack_cp_bar,
          ulabs = input$i_profiles_ulabs,
          unit_results = r_unit_df(),
          out_path = file)
      }
    )

  })

}

# SUPPORTING FUNCTIONS ----------------------------------------------------

make_unit_profile <- function(coin, stack_children, usel, unit_results,
                              ulabs, out_path){

  rmarkdown::render(
    system.file("unit_report.Rmd", package = "composer"),
    output_file = out_path,
    params = list(
      COIN = coin,
      unit_results = unit_results,
      usel = usel,
      stack_children = stack_children,
      ulabs = ulabs
    ),
    envir = new.env(),
    intermediates_dir = tempdir()
  )

}
