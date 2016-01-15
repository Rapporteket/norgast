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
                  all_variables.AvdRESH + \'0\' as AvdRESH,
                  Avdeling,
                  BMI_CATEGORY,
                  WEIGHTLOSS + \'0\' as WEIGHTLOSS,
                  DIABETES + \'0\' as DIABETES,
                  CHEMOTHERAPY_ONLY + \'0\' as CHEMOTHERAPY_ONLY,
                  RADIATION_THERAPY_ONLY + \'0\' as RADIATION_THERAPY_ONLY,
                  CHEMORADIOTHERAPY + \'0\' as CHEMORADIOTHERAPY,
                  WHO_ECOG_SCORE + \'0\' as WHO_ECOG_SCORE,
                  MODIFIED_GLASGOW_SCORE + \'0\' as MODIFIED_GLASGOW_SCORE,
                  ASA + \'0\' as ASA,
                  ANESTHESIA_START,
                  NCSP,
                  cast(OPERATION_DATE as char(19)) as OPERATION_DATE,
                  ANASTOMOSIS + \'0\' as ANASTOMOSIS,
                  OSTOMY + \'0\' as OSTOMY,
                  ABDOMINAL_ACCESS + \'0\' as ABDOMINAL_ACCESS,
                  ROBOTASSISTANCE + \'0\' as ROBOTASSISTANCE,
                  THORAX_ACCESS + \'0\' as THORAX_ACCESS,
                  RELAPAROTOMY + \'0\' as RELAPAROTOMY,
                  ACCORDION_SCORE, isMale + \'0\' as isMale,
                  decimalAge,
                  PRS_SCORE + \'0\' as PRS_SCORE,
                  READMISSION_STATUS + \'0\' as READMISSION_STATUS,
                  STATUS + \'0\' as STATUS,
                  RELAPAROTOMY_YES + \'0\' as RELAPAROTOMY_YES,
                  READMISSION_ACCORDION_SCORE,
                  READMISSION_RELAPAROTOMY + \'0\' as READMISSION_RELAPAROTOMY,
                  READMISSION_RELAPAROTOMY_YES + \'0\' as READMISSION_RELAPAROTOMY_YES,
                  Sykehusnavn,
                  HovedDato
                  FROM NoRGastReportDataStaging.all_variables INNER JOIN NoRGastReportDataStaging.ForlopsOversikt
                  ON NoRGastReportDataStaging.all_variables.MCEID = NoRGastReportDataStaging.ForlopsOversikt.ForlopsID
                  WHERE HovedDato >= \'", datoFra, "\' AND HovedDato <= \'", datoTil, "\' ")

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
