#' Provide global dataframe for NoRGast
#'
#' Provides NoRGast data from staging
#'
#' @inheritParams FigAndeler
#'
#' @return RegData data frame
#' @export

NorgastHentRegData <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {

  registryName <- "norgast"
  dbType <- "mysql"

  query <- paste0("SELECT
                  BMIKategori,
                  BMI,
                  VekttapProsent,
                  MedDiabetes,
                  KunCytostatika,
                  KunStraaleterapi,
                  KjemoRadioKombo,
                  WHOECOG,
                  ModGlasgowScore,
                  ASA,
                  AnestesiStartKl,
                  Hovedoperasjon,
                  OpDato,
                  NyAnastomose,
                  NyStomi,
                  Tilgang,
                  Robotassistanse,
                  ThoraxTilgang,
                  ReLapNarkose,
                  AccordionGrad,
                  PRSScore,
                  OppfStatus,
                  RegistreringStatus,
                  ViktigsteFunn,
                  OppfAccordionGrad,
                  OppfReLapNarkose,
                  OppfViktigsteFunn,
                  Avdod,
                  AvdodDato,
                  Hoveddiagnose,
                  ForlopsOversikt.ErMann,
                  ForlopsOversikt.AvdRESH,
                  ForlopsOversikt.Sykehusnavn,
                  ForlopsOversikt.PasientAlder,
                  ForlopsOversikt.HovedDato,
                  ForlopsOversikt.BasisRegStatus,
                  ForlopsOversikt.ForlopsID,
                  ForlopsOversikt.PasientID
                  FROM AlleVarNum INNER JOIN ForlopsOversikt
                  ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID
                  WHERE HovedDato >= \'", datoFra, "\' AND HovedDato <= \'", datoTil, "\' ")

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
