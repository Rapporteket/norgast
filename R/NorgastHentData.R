#' Provide global dataframe for NORGAST
#'
#' Provides NORGAST data from staging
#'
#' @inheritParams FigAndeler
#'
#' @return RegData data frame
#' @export
#'
NorgastHentRegData <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {

  if (Sys.getenv("R_RAP_INSTANCE") %in% c("QAC", "PRODUCTIONC")){
    registryName <- "data"
  } else {
    registryName <- "norgast"
  }

  dbType <- "mysql"

  if (rapbase::isRapContext()){
    query1 <- "SELECT * FROM allevarnum"
    allevarnum <- rapbase::loadRegData(registryName, query1, dbType)
    query2 <- paste0("SELECT * FROM forlopsoversikt WHERE HovedDato >= \'",
                     datoFra, "\' AND HovedDato <= \'", datoTil, "\' ")
    forlopsoversikt <- rapbase::loadRegData(registryName, query2, dbType)
    skjemaoversikt <- NorgastHentskjemaoversikt()
  } else {
    allevarnum <- read.table("C:/GIT/data/norgast/allevarnum", header = TRUE,
                             sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE)
    forlopsoversikt <- read.table("C:/GIT/data/norgast/forlopsoversikt", header = TRUE,
                             sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE)
    forlopsoversikt <- forlopsoversikt[forlopsoversikt$HovedDato >= datoFra &
                                         forlopsoversikt$HovedDato <= datoTil, ]
  }
  RegData <- merge(allevarnum,
                   forlopsoversikt[, c(setdiff(names(forlopsoversikt),
                                               names(allevarnum)), "ForlopsID")],
                   by = "ForlopsID") %>%
    merge(skjemaoversikt %>% dplyr::filter(SkjemaRekkeflg == 5) %>%
            dplyr::select(ForlopsID, OpprettetAv, SistLagretAv),
          by = "ForlopsID", all.x = TRUE) %>%
    merge(skjemaoversikt %>% dplyr::filter(SkjemaRekkeflg == 10) %>%
            dplyr::select(ForlopsID, OpprettetAv, SistLagretAv),
          suffixes = c("", "_oppf"), by = "ForlopsID", all.x = TRUE) %>%
    dplyr::rename(OppfOpprettetAv = OpprettetAv_oppf,
                  OppfSistLagretAv = SistLagretAv_oppf)

}

