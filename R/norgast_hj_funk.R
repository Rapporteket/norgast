#' R-shiny alternativ til dateInput med mulighet til  bare år, måned og år
#'
#' @param inputId Definerer Namespace
#' @param label Visningstekst til brukerkontrollen
#' @param minview Fineste nivå tidsoppløsning
#' @param maxview Groveste nivå tidsoppløsning
#'
#' @return En brukerkontroll for bruk i R-shiny som tillater valg av f.eks. kun år og måned
#'        uten å angi dag.
#'
#' @export
dateInput2 <- function(inputId, label, minview = "months", maxview = "years", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}

#' Skriv til csv i nordisk format med Latin1 som default tegnsetting
#'
#' @export
write.csv3 <- function(x, file = "", tegnsetting = 'latin1', ...) {
  write.csv2(x, file = file, fileEncoding = tegnsetting, ...)
}

#' Generer kvartalsrapport og returner filnavn og sti til fil.
#'
#' @export
abonnement_kvartal_norgast <- function(baseName, reshID=0, valgtShus='', brukernavn='Pjotr') {

  # rapbase::autLogger(user = brukernavn, registryName = 'NORGAST',
  #                   reshId = reshID[[1]], msg = "Abonnement: kvartalsrapport")

  src <- system.file(paste0(baseName, '.Rnw'), package="norgast")
  tmpFile <- tempfile(paste0(baseName, Sys.Date()), fileext = '.Rnw')

  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  file.copy(src, tmpFile, overwrite = TRUE)

  # texfil <- knitr::knit(tmpFile, encoding = 'UTF-8')
  # tools::texi2pdf(texfil, clean = TRUE)


  pdfFile <- knitr::knit2pdf(tmpFile)
  utfil <- paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')

  file.copy(pdfFile, utfil)

  # rapbase::subLogger(author = brukernavn, registryName = 'NORGAST',
  #                   reshId = reshID[[1]], msg = paste("Sendt: ", utfil))

  return(utfil)
}

#' Hvis NULL erstatt med valgt verdi, default ''
#'
#' @export
fiksNULL <- function(x, erstatt='') {
  if (!is.null(x)) {x} else {erstatt}
}


#' Identifiserer pasienter med flere enn ett forløp med samme operasjonsdato
#'
#' @param RegData Datasettet som kan inneholde dobbeltregistreringer
#'
#' @return En dataramme med utvalgte variabler for potensielt dobbeltregistrerte forløp
#'
#' @export
dobbelreg <- function(RegData, skjemaoversikt, usrRole = 'LU', reshID) {
  flere_sammedato <- RegData %>%
    dplyr::group_by(PasientID, HovedDato) %>%
    dplyr::summarise(Op_pr_dag = dplyr::n())
  flere_sammedato <- flere_sammedato[flere_sammedato$Op_pr_dag > 1, ]

  flere_sammedato <- merge(flere_sammedato, RegData,
                           by = c('PasientID', 'HovedDato'), all.x = T)
  flere_sammedato <- flere_sammedato[ , c("PasientID", "ForlopsID", "OperasjonsDato",
                                          "AvdRESH", "Sykehusnavn","Hovedoperasjon",
                                          "Operasjonsgrupper", "Hoveddiagnose",
                                          "OppfStatus")]
  skjemaoversikt <- skjemaoversikt %>%
    dplyr::summarise(OpprettetAv = paste(unique(OpprettetAv), collapse = ", "),
                     SistLagretAv = paste(unique(SistLagretAv), collapse = ", "),
                     .by = ForlopsID) %>%
    dplyr::filter(ForlopsID %in% flere_sammedato$ForlopsID)
  flere_sammedato <- merge(flere_sammedato, skjemaoversikt, by = "ForlopsID")
  flere_sammedato$OperasjonsDato <- format(flere_sammedato$OperasjonsDato, format="%Y-%m-%d")
  flere_sammedato$PasientID <- as.numeric(flere_sammedato$PasientID)
  flere_sammedato$ForlopsID <- as.numeric(flere_sammedato$ForlopsID)
  flere_sammedato$AvdRESH <- as.numeric(flere_sammedato$AvdRESH)
  flere_sammedato <- flere_sammedato[order(flere_sammedato$OperasjonsDato, flere_sammedato$PasientID, decreasing = T), ]
  if (usrRole != 'SC') {
    flere_sammedato <- flere_sammedato[flere_sammedato$AvdRESH == reshID, ]
  }
  # flere_sammedato <- flere_sammedato %>%
  #   dplyr::select(-c(OpprettetAv, SistLagretAv)) # Foreløpig variant i påvente av at brukernavn fikses
  return(flere_sammedato)
}


#' Make staging data for norgast
#'
#' This function makes queries and pre-processing of registry data before
#' storing relevant staging data. Running this function may take a while so use
#' with care!
#'
#' @return Character vector of staging files, invisibly
#' @export
norgastMakeStagingData <- function() {

  RegData <-  norgast::NorgastHentRegData()
  skjemaoversikt <- norgast::NorgastHentskjemaoversikt()
  skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
  RegData <- norgast::NorgastPreprosess(RegData, behold_kladd = TRUE)
  skjemaoversikt <- merge(skjemaoversikt, RegData[,c("ForlopsID", "Op_gr", "Hovedoperasjon")], by = "ForlopsID", all.x = T)
  RegData <- RegData[which(RegData$RegistreringStatus==1),]
  RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
  rapbase::saveStagingData("norgast", "RegData", RegData)
  rapbase::saveStagingData("norgast", "skjemaoversikt", skjemaoversikt)

  invisible(rapbase::listStagingData("norgast"))
}


#' Transponer output fra tidyr::summarize
#'
#' Denne funksjonen tar som input resultatet av tidyr::summarize og returnerer dens
#' transponerte uten at formatene endres.
#'
#' Her kan detaljer skrives
#'
#' @return tr_frame Den transponerte av inputen
#'
#' @export
#'
tr_summarize_output <- function(x, kolnavn1 = ""){

  rekkefolge <- names(x)[-1]
  y <- x %>% tidyr::gather(names(x)[-1], key=nokkel, value = verdi) %>%
    tidyr::spread(key=names(x)[1], value = verdi)
  y <- y[match(rekkefolge, y$nokkel), ]
  names(y)[1] <- kolnavn1

  return(y)
}
