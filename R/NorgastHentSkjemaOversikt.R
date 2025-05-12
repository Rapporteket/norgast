#' Provide skjemaoversikt for NORGAST
#'
#' Provides skjemaoversikt for NORGAST data staging
#'
#' @return skjemaoversikt data frame
#' @export

NorgastHentskjemaoversikt <- function() {

  if (Sys.getenv("R_RAP_INSTANCE") %in% c("QAC", "PRODUCTIONC")){
    registryName <- "data"
  } else {
    registryName <- "norgast"
  }
  dbType <- "mysql"

  query <- paste0("SELECT * FROM skjemaoversikt")
  skjemaoversikt <- rapbase::loadRegData(registryName, query, dbType)
  # query <- "SELECT * FROM user"
  # brukerinfo <- rapbase::loadRegData(registryName, query, dbType) %>%
  #   dplyr::mutate(fullname = paste0(FIRSTNAME, " ", LASTNAME))
  #
  # skjemaoversikt$OpprettetAv <-
  #   brukerinfo$fullname[match(skjemaoversikt$OpprettetAv, brukerinfo$ID)]
  # skjemaoversikt$SistLagretAv <-
  #   brukerinfo$fullname[match(skjemaoversikt$SistLagretAv, brukerinfo$ID)]

  return(skjemaoversikt)
}
