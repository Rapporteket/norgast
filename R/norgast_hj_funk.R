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
