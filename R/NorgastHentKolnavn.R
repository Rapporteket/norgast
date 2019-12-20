#' Hent navnene til variablene i datadump NoRGast
#'
#' @inheritParams FigAndeler
#'
#' @return kolnavn dataframe
#' @export

NorgastHentKolnavn <- function(tabellnavn = 'AlleVarNum') {

  registryName <- "norgast"
  dbType <- "mysql"

  query <- paste0("SELECT COLUMN_NAME
                   FROM INFORMATION_SCHEMA.COLUMNS
                   WHERE table_name = ", tabellnavn)

  kolnavn <- rapbase::LoadRegData(registryName, query, dbType)

  return(kolnavn)
}
