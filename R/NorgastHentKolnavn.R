#' Hent navnene til variablene i datadump NORGAST
#'
#' @inheritParams FigAndeler
#'
#' @return kolnavn dataframe
#' @export

NorgastHentKolnavn <- function(tabellnavn = 'allevarnum') {

  registryName <- "norgast"
  dbType <- "mysql"

  query <- paste0("SELECT COLUMN_NAME
                   FROM INFORMATION_SCHEMA.COLUMNS
                   WHERE table_name = ", tabellnavn)

  kolnavn <- rapbase::LoadRegData(registryName, query, dbType)

  return(kolnavn)
}
