#' Provide global dataframe for NORGAST
#'
#' Provides NORGAST data from staging
#'
#' @inheritParams FigAndeler
#'
#' @return RegData data frame
#' @export

# NorgastHentRegData <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {
#
#   registryName <- "norgast"
#   dbType <- "mysql"
#
#   query <- paste0("SELECT
#                   BMIKategori,
#                   BMI,
#                   VekttapProsent,
#                   MedDiabetes,
#                   KunCytostatika,
#                   KunStraaleterapi,
#                   KjemoRadioKombo,
#                   WHOECOG,
#                   ModGlasgowScore,
#                   ASA,
#                   AnestesiStartKl,
#                   Hovedoperasjon,
#                   OpDato,
#                   NyAnastomose,
#                   NyStomi,
#                   Tilgang,
#                   Robotassistanse,
#                   ThoraxTilgang,
#                   ReLapNarkose,
#                   AccordionGrad,
#                   PRSScore,
#                   OppfStatus,
#                   RegistreringStatus,
#                   ViktigsteFunn,
#                   OppfAccordionGrad,
#                   OppfReLapNarkose,
#                   OppfViktigsteFunn,
#                   AlleVarNum.Avdod,
#                   AlleVarNum.AvdodDato,
#                   Hoveddiagnose,
#                   Hastegrad,
#                   AvstandAnalVerge,
#                   Albumin,
#                   CRP,
#                   TelefonKontroll,
#                   FysiskKontroll,
#                   Rekonstruksjon,
#                   Rekonstruksjonstype,
#                   EndoInterBlod,
#                   EndoInterLekkasje,
#                   PerkDrenasje,
#                   HoyAmylaseKons,
#                   KunDrenasje,
#                   ForlopsOversikt.erMann,
#                   ForlopsOversikt.AvdRESH,
#                   ForlopsOversikt.SykehusNavn,
#                   ForlopsOversikt.PasientAlder,
#                   ForlopsOversikt.HovedDato,
#                   ForlopsOversikt.BasisRegStatus,
#                   ForlopsOversikt.ForlopsID,
#                   ForlopsOversikt.PasientID
#                   FROM AlleVarNum INNER JOIN ForlopsOversikt
#                   ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID
#                   WHERE HovedDato >= \'", datoFra, "\' AND HovedDato <= \'", datoTil, "\' ")
#
#   RegData <- rapbase::loadRegData(registryName, query, dbType)
#
#   return(RegData)
# }


NorgastHentRegData <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {

  registryName <- "norgast"
  dbType <- "mysql"

  if (rapbase::isRapContext()){
    query1 <- "SELECT * FROM AlleVarNum"
    AlleVarNum <- rapbase::loadRegData(registryName, query1, dbType)
    query2 <- paste0("SELECT * FROM ForlopsOversikt WHERE HovedDato >= \'", datoFra, "\' AND HovedDato <= \'", datoTil, "\' ")
    ForlopsOversikt <- rapbase::loadRegData(registryName, query2, dbType)
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
                   by = "ForlopsID")

}

