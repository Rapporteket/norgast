#' Run the NORGAST Shiny Application
#'
#' @return An object representing the NORGAST app
#' @export

norgastApp <- function(logAsJson = true) {
  if (logAsJson) {
    rapbase::loggerSetup()
  }
  shiny::shinyApp(ui = appUi, server = appServer)
}
