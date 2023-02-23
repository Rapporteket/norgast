#' Run the NoRGast Shiny Application
#'
#' @return An object representing the NoRGast app
#' @export

norgastApp <- function() {
  shiny::shinyApp(ui = appUi, server = appServer)
}
