#' @import bslib
bslib_ui <- function(id = NULL){

  sidebar_width <<- "25vw"
  bookmark_type <- "URL" # set either "URL" or "RDS"

  page_navbar(
    title = span(icon("coins"), "ShinyCOINr"),
    bg = "#0062cc",
    underline = TRUE,

    header = tagList(
      shinyjs::useShinyjs(),
    ),

    # OVERVIEW
    nav_panel(
      "Data input",
      value = "overview-panel",
      overview_ui("overview"),
      icon = icon("file-import")
    ),


    # DATA OPS DROPDOWN
    nav_menu(
      title = "Data Operations",
      icon = icon("code"),
      nav_panel(
        title = "Unit Screening",
        value = "screening-panel",
        screening_ui("screening")
      ),
      nav_panel(
        title = "Imputation",
        value = "imputation-panel",
        imputation_ui("imputation")
      ),
      nav_panel(
        title = "Outlier Treatment",
        value = "treat-panel",
        treat_ui("treat")
      )
    ),

    # NORMALISE
    nav_panel(
      title = "Normalise",
      value = "normalisation-panel",
      normalisation_ui("normalisation"),
      icon = icon("compress")
    ),

    # COMPOSE
    nav_panel(
      "Compose index",
      value = "compose-panel",
      compose_ui("compose"),
      icon = icon("bolt")
    ),

    # EXPLORE DROPDOWN
    nav_menu(
      title = "Explore",
      icon = icon("chart-simple"),
      nav_panel(
        title = "Indicator statistics",
        value = "stats-panel",
        stats_ui("stats")
      ),
      nav_panel(
        title = "Correlations",
        value = "correlations-panel",
        correlations_ui("correlations")
      ),
      nav_panel(
        title = "Map",
        value = "map-panel",
        map_ui("map")
      ),
      nav_panel(
        title = "Bubble chart",
        value = "bubble-panel",
        bubble_ui("bubble")
      ),
      nav_panel(
        title = "Bar chart",
        value = "bar-panel",
        bar_ui("bar")
      )
    ),

    # PROFILES
    nav_panel(
      title = "Unit profiles",
      icon = icon("file-contract"),
      value = "profiles-panel",
      profiles_ui("profiles")
    ),

    # ADJUST DROPDOWN
    navbarMenu(
      title = "Adjust",
      icon = icon("sliders"),
      nav_panel(
        title = "Re-weighting",
        value = "reweighting-panel",
        reweighting_ui("reweighting")
      ),
      nav_panel(
        title = "Remove elements",
        value = "remove_elements-panel",
        remove_elements_ui("remove_elements")
      ),
      nav_panel(
        title = "Sensitivity analysis",
        value = "sensitivity-panel",
        sensitivity_ui("sensitivity")
      )
    ),

    nav_spacer(),

    # EXPORT TO EXCEL
    nav_menu(
      title = "Save/export",
      align = "right",
      nav_item(
        downloadButton("export_to_excel", label = "Export", icon = icon("file-excel"))
      ),
      nav_item(make_bookmark_button(bookmark_type))
    ),


    nav_item(icon("circle-question")),

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
