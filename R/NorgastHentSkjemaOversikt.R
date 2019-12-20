#' Provide SkjemaOversikt for NoRGast
#'
#' Provides SkjemaOversikt for NoRGast data staging
#'
#' @inheritParams FigAndeler
#'
#' @return SkjemaOversikt data frame
#' @export

NorgastHentSkjemaOversikt <- function() {

  registryName <- "norgast"
  dbType <- "mysql"

  query <- paste0("SELECT *
                  FROM SkjemaOversikt")

  SkjemaOversikt <- rapbase::LoadRegData(registryName, query, dbType)

  return(SkjemaOversikt)
}
