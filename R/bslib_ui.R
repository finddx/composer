#' @import bslib
bslib_ui <- function(id = NULL){

  sidebar_width <- "25vw" # sidebar width - passed to modules
  bookmark_type <- "URL" # set either "URL" or "RDS"

  page_navbar(
    id = "navpanel_selected",
    title = span(icon("music"), "composer"),
    bg = "#0062cc",
    underline = TRUE,

    header = tagList(
      shinyjs::useShinyjs(),
    ),

    # OVERVIEW
    nav_panel(
      "Data input",
      value = "overview-panel",
      overview_ui("overview", sidebar_width = sidebar_width),
      icon = icon("file-import")
    ),


    # DATA OPS DROPDOWN
    nav_menu(
      title = "Data Operations",
      icon = icon("code"),
      nav_panel(
        title = "Unit Screening",
        value = "screening-panel",
        screening_ui("screening", sidebar_width = sidebar_width)
      ),
      nav_panel(
        title = "Imputation",
        value = "imputation-panel",
        imputation_ui("imputation", sidebar_width = sidebar_width)
      ),
      nav_panel(
        title = "Outlier Treatment",
        value = "treat-panel",
        treat_ui("treat", sidebar_width = sidebar_width)
      )
    ),

    # NORMALISE
    nav_panel(
      title = "Normalise",
      value = "normalisation-panel",
      normalisation_ui("normalisation", sidebar_width = sidebar_width),
      icon = icon("compress")
    ),

    # COMPOSE
    nav_panel(
      "Compose index",
      value = "compose-panel",
      compose_ui("compose", sidebar_width = sidebar_width),
      icon = icon("bolt")
    ),

    # EXPLORE DROPDOWN
    nav_menu(
      title = "Explore",
      icon = icon("chart-simple"),
      nav_panel(
        title = "Indicator statistics",
        value = "stats-panel",
        stats_ui("stats", sidebar_width = sidebar_width)
      ),
      nav_panel(
        title = "Correlations",
        value = "correlations-panel",
        correlations_ui("correlations", sidebar_width = sidebar_width)
      ),
      nav_panel(
        title = "Map",
        value = "map-panel",
        map_ui("map", sidebar_width = sidebar_width)
      ),
      nav_panel(
        title = "Bubble chart",
        value = "bubble-panel",
        bubble_ui("bubble", sidebar_width = sidebar_width)
      ),
      nav_panel(
        title = "Bar chart",
        value = "bar-panel",
        bar_ui("bar", sidebar_width = sidebar_width)
      )
    ),

    # PROFILES
    nav_panel(
      title = "Unit profiles",
      icon = icon("file-contract"),
      value = "profiles-panel",
      profiles_ui("profiles", sidebar_width = sidebar_width)
    ),

    # ADJUST DROPDOWN
    navbarMenu(
      title = "Adjust",
      icon = icon("sliders"),
      nav_panel(
        title = "Re-weighting",
        value = "reweighting-panel",
        reweighting_ui("reweighting", sidebar_width = sidebar_width)
      ),
      nav_panel(
        title = "Remove elements",
        value = "remove_elements-panel",
        remove_elements_ui("remove_elements", sidebar_width = sidebar_width)
      ),
      nav_panel(
        title = "Sensitivity analysis",
        value = "sensitivity-panel",
        sensitivity_ui("sensitivity", sidebar_width = sidebar_width)
      )
    ),

    nav_spacer(),

    # EXPORT TO EXCEL
    nav_menu(
      title = "Save/export",
      align = "right",
      nav_item(make_bookmark_button(bookmark_type)),
      nav_item(
        downloadButton("export_to_excel", label = "Export to Excel", icon = icon("file-excel"))
      ),
      nav_item(
        downloadButton("export_to_R", label = "Export to R")
      )
    ),


    nav_item(
      tags$a(href="https://finddx.github.io/composer/", target="_blank", icon("circle-question"))
    ),

    footer = p(
      "Developed by the ",
      tags$a(href="https://www.finddx.org/", "Foundation for Innovative New Diagnostics"),
      " by ",
      tags$a(href="https://www.willbecker.me/", "William Becker"),
      " Anna Mantsoki and Ben Ubah.",
      style = "font-size: 0.8em; color: grey; padding-right: 20px; text-align: right;"
    )

  )

}
