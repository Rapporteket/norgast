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

  registryName <- "norgast"
  dbType <- "mysql"

  if (rapbase::isRapContext()){
    query1 <- "SELECT * FROM AlleVarNum"
    AlleVarNum <- rapbase::loadRegData(registryName, query1, dbType)
    query2 <- paste0("SELECT * FROM ForlopsOversikt WHERE HovedDato >= \'",
                     datoFra, "\' AND HovedDato <= \'", datoTil, "\' ")
    ForlopsOversikt <- rapbase::loadRegData(registryName, query2, dbType)
    SkjemaOversikt <- NorgastHentSkjemaOversikt()
  } else {
    AlleVarNum <- read.table("C:/GIT/data/norgast/AlleVarNum", header = TRUE,
                             sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE)
    ForlopsOversikt <- read.table("C:/GIT/data/norgast/ForlopsOversikt", header = TRUE,
                             sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE)
    ForlopsOversikt <- ForlopsOversikt[ForlopsOversikt$HovedDato >= datoFra &
                                         ForlopsOversikt$HovedDato <= datoTil, ]
  }
  RegData <- merge(AlleVarNum,
                   ForlopsOversikt[, c(setdiff(names(ForlopsOversikt),
                                               names(AlleVarNum)), "ForlopsID")],
                   by = "ForlopsID") %>%
    merge(SkjemaOversikt %>% dplyr::filter(SkjemaRekkeflg == 5) %>%
            dplyr::select(ForlopsID, OpprettetAv, SistLagretAv),
          by = "ForlopsID", all.x = TRUE) %>%
    merge(SkjemaOversikt %>% dplyr::filter(SkjemaRekkeflg == 10) %>%
            dplyr::select(ForlopsID, OpprettetAv, SistLagretAv),
          suffixes = c("", "_oppf"), by = "ForlopsID", all.x = TRUE) %>%
    dplyr::rename(OppfOpprettetAv = OpprettetAv_oppf,
                  OppfSistLagretAv = SistLagretAv_oppf)

}

