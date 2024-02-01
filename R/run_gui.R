#' Launch the app
#'
#' This function launches the composer app.
#'
#' @import shiny
#' @export
run_gui <- function() {

  enableBookmarking(store = "server")
  # Run the application
  shinyApp(ui = bslib_ui, server = coinr_server,
           options = list(launch.browser = TRUE))
}
