#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#'
#' @inheritParams FigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

NorgastLibUtvalg <- function(RegData, datoFra, datoTil, minald, maxald, erMann, op_gruppe, elektiv, BMI,
                             valgtShus=valgtShus, fargepalett='BlaaRapp')
{
  # Definerer intersect-operator
  "%i%" <- intersect
  # "%u%" <- union

  #Hvis "Variabel" ikke definert
  if (length(which(names(RegData) == 'Variabel')) == 0 ) {RegData$Variabel <- 0}
  Ninn <- dim(RegData)[1]
  indVarMed <- which(RegData$Variabel != 'NA') %i% which(RegData$Variabel != 'NaN') %i%
                         which(RegData$Variabel != '') %i% which(!is.na(RegData$Variabel)) %i% which(!is.nan(RegData$Variabel))
  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  indDato <- which(RegData$OperasjonsDato >= as.POSIXlt(datoFra) & RegData$OperasjonsDato <= as.POSIXlt(datoTil))
  indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {indKj <- 1:Ninn}
  indOp_gr <- if (op_gruppe %in% c(1,2,3,4,5,6,9)){which(RegData$Op_gr == op_gruppe)} else {indOp_gr <- 1:Ninn}
  indElekt <- if (elektiv %in% c(0,1)){which(RegData$Hastegrad == elektiv)} else {indElekt <- 1:Ninn}
  indBMI <- if (BMI[1] != '') {which(RegData$BMI_kodet %in% as.numeric(BMI))} else {indBMI <- 1:Ninn}
  # indRisk <- if (max(RiskFakt) > 0) {}
  indMed <- indAld %i% indDato %i% indKj %i% indVarMed %i% indOp_gr %i% indElekt %i% indBMI
  RegData <- RegData[indMed,]

  utvalgTxt <- c(paste('Operasjonsdato: ',
                       min(RegData$OperasjonsDato, na.rm=T), ' til ', max(RegData$OperasjonsDato, na.rm=T), sep='' ),
                 if ((minald>0) | (maxald<120)) {
                   paste('Pasienter fra ', min(RegData$Alder, na.rm=T), ' til ', max(RegData$Alder, na.rm=T), ' år', sep='')},
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                 if (op_gruppe %in% c(1,2,3,4,5,6,9)) {paste('Operasjonsgruppe: ', c('Kolonreseksjoner', 'Rektumreseksjoner',
                                                        'Øsofagusreseksjoner', 'Ventrikkelreseksjoner', 'Leverreseksjoner',
                                                        "Whipples operasjon", rep('',2), 'Øvrige')[op_gruppe], sep='')},
                 if (elektiv %in% c(0,1)) {c('Øyeblikkelig hjelp', 'Elektiv kirurgi')[elektiv+1]},
                 if (BMI[1] != '') {paste0('BMI-gruppe: ', paste(BMI, collapse=','))},
                 if (length(valgtShus)>1) {paste0('Valgte RESH: ', paste(as.character(valgtShus), collapse=','))}
  )


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
  return(invisible(UtData))
}
