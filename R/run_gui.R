#' Launch the app
#'
#' This function launches the FIND-CI app. The coinr_ui function is wrapped by
#' the shinyfind package for styling.
#'
#' @import shiny
#' @export
run_gui <- function() {

  page_title <- "FIND Composite Indicator"
  desc_text <- glue::glue('<p>
                           An app for building and analysing composite indicators.
                         </p>'
  )

  enableBookmarking(store = "server")

  # apply CSS style sheet and add shinyfind banner
  ui <- function(request) {
    shinyfind::find_dashboard_page(
      banner = shinyfind::find_banner(page_title, extra_html = desc_text),
      tags$head(includeCSS(system.file("app", "style.css", package = "findcompositeindicator"))),
      tags$head(tags$link(rel="shortcut icon", href="favicon.jpg")),
      coinr_ui()
    )
  }

  # Run the application
  shinyApp(ui = bslib_ui, server = coinr_server)

}
