#' Run the NORGAST Shiny Application
#'
#' @param logAsJson Logical indicating whether to log messages as JSON
#' @param browser Logical indicating whether to open the app in a browser
#'
#' @return An object representing the NORGAST app
#' @export

norgastApp <- function(logAsJson = TRUE, browser = FALSE) {
  if (logAsJson) {
    rapbase::loggerSetup()
  }
  if (browser) {
    options(shiny.launch.browser = TRUE)
  }
  shiny::shinyApp(
    ui = appUi,
    server = appServer
  )
}
