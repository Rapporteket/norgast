#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#'
#' @inheritParams FigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

NorgastLibUtvalg <- function(RegData, datoFra, datoTil, minald, maxald, erMann, op_gruppe, elektiv, fargepalett='BlaaRapp')
{

  #Hvis "Variabel" ikke definert
  if (length(which(names(RegData) == 'Variabel')) == 0 ) {RegData$Variabel <- 0}
  Ninn <- dim(RegData)[1]
  indVarMed <- intersect(intersect(intersect(intersect(which(RegData$Variabel != 'NA'), which(RegData$Variabel != 'NaN')),
                         which(RegData$Variabel != '')), which(!is.na(RegData$Variabel))), which(!is.nan(RegData$Variabel)))
  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  indDato <- which(RegData$OperasjonsDato >= as.POSIXlt(datoFra) & RegData$OperasjonsDato <= as.POSIXlt(datoTil))
  indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {indKj <- 1:Ninn}
  indOp_gr <- if (op_gruppe %in% c(1,2,3,4,5,6,9)){which(RegData$Op_gr == op_gruppe)} else {indOp_gr <- 1:Ninn}
  indElekt <- if (elektiv %in% c(0,1)){which(RegData$Hastegrad == elektiv)} else {indElekt <- 1:Ninn}

  indMed <- intersect(intersect(intersect(indAld, intersect(indDato, intersect(indKj, indVarMed))), indOp_gr), indElekt)
  RegData <- RegData[indMed,]

  utvalgTxt <- c(paste('Operasjonsdato: ',
                       min(RegData$OperasjonsDato, na.rm=T), ' til ', max(RegData$OperasjonsDato, na.rm=T), sep='' ),
                 if ((minald>0) | (maxald<120)) {
                   paste('Pasienter fra ', min(RegData$Alder, na.rm=T), ' til ', max(RegData$Alder, na.rm=T), ' år', sep='')},
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                 if (op_gruppe %in% c(1,2,3,4,5,6,9)) {paste('Operasjonsgruppe: ', c('Kolonreseksjoner', 'Rektumreseksjoner',
                                                        'Øsofagusreseksjoner', 'Ventrikkelreseksjoner', 'Leverreseksjoner',
                                                        "Whipples operasjon", rep('',2), 'Øvrige')[op_gruppe], sep='')},
                 if (elektiv %in% c(0,1)) {c('Øyeblikkelig hjelp', 'Elektiv kirurgi')[elektiv+1]}
  )


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
  return(invisible(UtData))
}
