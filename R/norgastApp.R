#' Run the NORGAST Shiny Application
#'
#' @return An object representing the NORGAST app
#' @export

norgastApp <- function() {
  shiny::shinyApp(ui = appUi, server = appServer)
}
