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
                  BMI_CATEGORY,
                  WEIGHTLOSS,
                  DIABETES,
                  CHEMOTHERAPY_ONLY,
                  RADIATION_THERAPY_ONLY,
                  CHEMORADIOTHERAPY,
                  WHO_ECOG_SCORE,
                  MODIFIED_GLASGOW_SCORE,
                  ASA,
                  ANESTHESIA_START,
                  NCSP,
                  OPERATION_DATE,
                  ANASTOMOSIS,
                  OSTOMY,
                  ABDOMINAL_ACCESS,
                  ROBOTASSISTANCE,
                  THORAX_ACCESS,
                  RELAPAROTOMY,
                  ACCORDION_SCORE,
                  PRS_SCORE,
                  READMISSION_STATUS,
                  STATUS,
                  RELAPAROTOMY_YES,
                  READMISSION_ACCORDION_SCORE,
                  READMISSION_RELAPAROTOMY,
                  READMISSION_RELAPAROTOMY_YES,
                  DECEASED,
                  DECEASED_DATE,
                  ForlopsOversikt.ErMann,
                  ForlopsOversikt.AvdRESH,
                  ForlopsOversikt.Sykehusnavn,
                  ForlopsOversikt.PasientAlder,
                  ForlopsOversikt.HovedDato,
                  ForlopsOversikt.BasisRegStatus,
                  ForlopsOversikt.ForlopsID
                  FROM AlleVarNum INNER JOIN ForlopsOversikt
                  ON AlleVarNum.MCEID = ForlopsOversikt.ForlopsID
                  WHERE HovedDato >= \'", datoFra, "\' AND HovedDato <= \'", datoTil, "\' ")

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
