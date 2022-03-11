#' Make staging data for NoRGast
#'
#' This function makes queries and pre-processing of registry data before
#' storing relevant staging data. Running this function may take a while so use
#' with care!
#'
#' @return Character vector of staging files, invisibly
#' @export

makeStagingData <- function() {

  RegData <-  norgast::NorgastHentRegData()
  skjemaoversikt <- norgast::NorgastHentSkjemaOversikt()
  skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
  RegData <- norgast::NorgastPreprosess(RegData, behold_kladd = TRUE)
  skjemaoversikt <- merge(skjemaoversikt, RegData[,c("ForlopsID", "Op_gr", "Hovedoperasjon")], by = "ForlopsID", all.x = T)
  RegData <- RegData[which(RegData$RegistreringStatus==1),]
  RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
  rapbase::saveStagingData("norgast", "RegData", RegData)
  rapbase::saveStagingData("norgast", "skjemaoversikt", skjemaoversikt)

  invisible(rapbase::listStagingData("norgast"))
}
