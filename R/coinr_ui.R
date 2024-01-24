coinr_ui <- function(id = NULL) {

  bookmark_type <- "URL" # set either "URL" or "RDS"

  navbarPage(
    title = NULL,
    id = "active_tab",

    # EXPORT/SAVE SESH
    header = tagList(

      shinyjs::useShinyjs(), # set as header to avoid warning

      # NOTE: at the moment this floats over the whole page. Depending on screen
      # resolution, it may appear in different places... Not sure how to always
      # position this inside the header.
      span(
        downloadButton("export_to_excel", label = "Export", icon = icon("file-excel")),
        make_bookmark_button(bookmark_type),
        style = "position: absolute; top: 170px; right: 80px; z-index:10000;"
      ),

      conditionalPanel(
        "false", # always hide the download button
        downloadButton("download_session")
      ),
    ),

    # OVERVIEW
    tabPanel(
      "Overview",
      value = "overview-panel",
      overview_ui("overview")
    ),

    # DATA OPS DROPDOWN
    navbarMenu(
      "Data Operations",
      tabPanel(
        title = "Unit Screening",
        value = "screening-panel",
        screening_ui("screening")
      ),
      tabPanel(
        title = "Imputation",
        value = "imputation-panel",
        imputation_ui("imputation")
      ),
      tabPanel(
        title = "Outlier Treatment",
        value = "treat-panel",
        treat_ui("treat")
      ),
      tabPanel(
        title = "Normalisation",
        value = "normalisation-panel",
        normalisation_ui("normalisation")
      )
    ),

    # COMPOSE
    tabPanel(
      "Compose",
      value = "compose-panel",
      compose_ui("compose")
    ),

    # EXPLORE DROPDOWN
    navbarMenu(
      "Explore",
      tabPanel(
        title = "Indicator statistics",
        value = "stats-panel",
        stats_ui("stats")
      ),
      tabPanel(
        title = "Correlations",
        value = "correlations-panel",
        correlations_ui("correlations")
      ),
      tabPanel(
        title = "Map",
        value = "map-panel",
        map_ui("map")
      ),
      tabPanel(
        title = "Bubble chart",
        value = "bubble-panel",
        bubble_ui("bubble")
      ),
      tabPanel(
        title = "Bar chart",
        value = "bar-panel",
        bar_ui("bar")
      )
    ),

    # PROFILES
    tabPanel(
      "Unit profiles",
      value = "profiles-panel",
      profiles_ui("profiles")
    ),

    # ADJUST DROPDOWN
    navbarMenu(
      "Adjust",
      tabPanel(
        title = "Re-weighting",
        value = "reweighting-panel",
        reweighting_ui("reweighting")
      ),
      tabPanel(
        title = "Remove elements",
        value = "remove_elements-panel",
        remove_elements_ui("remove_elements")
      ),
      tabPanel(
        title = "Sensitivity analysis",
        value = "sensitivity-panel",
        sensitivity_ui("sensitivity")
      )
    ),

    # DOCUMENTATION
    footer = tagList(
      br(),
      htmlOutput("docu")
    )
  )
}
