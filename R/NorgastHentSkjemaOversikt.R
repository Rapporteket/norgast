#' Provide SkjemaOversikt for NORGAST
#'
#' Provides SkjemaOversikt for NORGAST data staging
#'
#' @return SkjemaOversikt data frame
#' @export

NorgastHentSkjemaOversikt <- function() {

  registryName <- "norgast"
  dbType <- "mysql"

  query <- paste0("SELECT * FROM SkjemaOversikt")
  SkjemaOversikt <- rapbase::loadRegData(registryName, query, dbType)
  query <- "SELECT * FROM user"
  brukerinfo <- rapbase::loadRegData(registryName, query, dbType) %>%
    mutate(fullname = paste0(FIRSTNAME, " ", LASTNAME))

  SkjemaOversikt$OpprettetAv <-
    brukerinfo$fullname[match(SkjemaOversikt$OpprettetAv, brukerinfo$ID)]
  SkjemaOversikt$SistLagretAv <-
    brukerinfo$fullname[match(SkjemaOversikt$SistLagretAv, brukerinfo$ID)]

  return(SkjemaOversikt)
}
