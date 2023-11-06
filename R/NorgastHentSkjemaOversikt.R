#' Provide SkjemaOversikt for NORGAST
#'
#' Provides SkjemaOversikt for NORGAST data staging
#'
#' @return SkjemaOversikt data frame
#' @export

NorgastHentSkjemaOversikt <- function() {

  registryName <- "norgast"
  dbType <- "mysql"

  query <- paste0("SELECT *
                  FROM SkjemaOversikt")

  SkjemaOversikt <- rapbase::loadRegData(registryName, query, dbType)

  return(SkjemaOversikt)
}
