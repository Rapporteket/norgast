#' Run the NoRGast Shiny Application
#'
#' @return An object representing the NoRGast app
#' @export

norgastApp <- function() {

  library("norgast")
  library("tidyverse")
  library("kableExtra")
  library("DT")
  library("shiny")
  library("shinyjs")
  library("shinyalert")
  library("lubridate")
  library("survival")
  library("survminer")
  library("ggplot2")
  library("funnelR")

  RegData <- rapbase::loadStagingData("norgast", "RegData") #Benyttes i appen
  skjemaoversikt <- rapbase::loadStagingData("norgast", "skjemaoversikt") #Benyttes i appen
  if (isFALSE(RegData) | isFALSE(skjemaoversikt)) {
    norgast::norgastMakeStagingData()
    RegData <- rapbase::loadStagingData("norgast", "RegData") #Benyttes i appen
    skjemaoversikt <- rapbase::loadStagingData("norgast", "skjemaoversikt") #Benyttes i appen
  }

  enhetsliste <- RegData[match(unique(RegData$AvdRESH), RegData$AvdRESH), c("AvdRESH", "Sykehusnavn")]
  BrValg <- norgast::BrValgNorgastShiny(RegData)




  shiny::shinyApp(ui = appUi, server = appServer)
}
