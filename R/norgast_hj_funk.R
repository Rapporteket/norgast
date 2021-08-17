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

  rapbase::subLogger(author = brukernavn, registryName = 'NoRGast',
                    reshId = reshID[[1]], msg = "Abonnement: kvartalsrapport")

  src <- system.file(paste0(baseName, '.Rnw'), package="norgast")
  tmpFile <- tempfile(paste0(baseName, Sys.Date()), fileext = '.Rnw')

  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  file.copy(src, tmpFile, overwrite = TRUE)

  texfil <- knitr::knit(tmpFile, encoding = 'UTF-8')
  tools::texi2pdf(texfil, clean = TRUE)

  utfil <- paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')
  rapbase::subLogger(author = brukernavn, registryName = 'NoRGast',
                    reshId = reshID[[1]], msg = paste("Sendt: ", utfil))

  return(utfil)
}

#' Hvis NULL erstatt med valgt verdi, default ''
#'
#' @export
fiksNULL <- function(x, erstatt='') {
  if (!is.null(x)) {x} else {erstatt}
}

