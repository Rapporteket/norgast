#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#'
#' @inheritParams FigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

NorgastUtvalg <- function(RegData, datoFra, datoTil, minald, maxald, erMann, elektiv, BMI,
                             valgtShus='', tilgang='', minPRS=0, maxPRS=2, ASA='', whoEcog='',
                             forbehandling='', malign=99, fargepalett='BlaaRapp', op_gruppe='', ncsp='')
{
  # Definerer intersect-operator
  "%i%" <- intersect

  Ninn <- dim(RegData)[1]
  indVarMed <- 1:Ninn
  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  indDato <- which(RegData$OperasjonsDato >= datoFra & RegData$OperasjonsDato <= datoTil)
  indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {indKj <- 1:Ninn}
  indOp_gr <- if (op_gruppe[1] != ''){which(RegData$Op_gr %in% as.numeric(op_gruppe))} else {indOp_gr <- 1:Ninn}
  indElekt <- if (elektiv %in% c(0,1)){which(RegData$Hastegrad == elektiv)} else {indElekt <- 1:Ninn}
  indBMI <- if (BMI[1] != '') {which(RegData$BMI_kodet %in% as.numeric(BMI))} else {indBMI <- 1:Ninn}
  indTilgang <- if (tilgang[1] != '') {which(RegData$Tilgang %in% as.numeric(tilgang))} else {indTilgang <- 1:Ninn}
  indPRS <- if ((minPRS>0) | (maxPRS<2)) {which(RegData$PRSScore >= minPRS & RegData$PRSScore <= maxPRS)} else {indPRS <- 1:Ninn}
  indASA <- if (ASA[1] != '') {which(RegData$ASA %in% as.numeric(ASA))} else {indASA <- 1:Ninn}
  indWHO <- if (whoEcog[1] != '') {which(RegData$WHOECOG %in% as.numeric(whoEcog))} else {indWHO <- 1:Ninn}
  indNCSP <- if (ncsp[1] != '') {which(substr(RegData$Hovedoperasjon, 1, 5) %in% ncsp)} else {indNCSP <- 1:Ninn}
  indForb <- if (forbehandling[1] != '') {which(RegData$Forbehandling %in% as.numeric(forbehandling))} else {indForb <- 1:Ninn}
  indMalign <- if (malign %in% c(0,1)){which(RegData$Malign == malign)} else {indMalign <- 1:Ninn}

  indMed <- indAld %i% indDato %i% indKj %i% indVarMed %i% indOp_gr %i% indElekt %i% indBMI %i%
    indTilgang %i% indPRS %i% indASA %i% indWHO %i% indForb %i% indMalign %i% indNCSP
  RegData <- RegData[indMed,]
  if (ncsp[1] != '') {ncsp <- sort(unique(substr(RegData$Hovedoperasjon, 1, 5)))}

  utvalgTxt <- c(paste('Operasjonsdato: ',
                       min(RegData$OperasjonsDato, na.rm=T), ' til ', max(RegData$OperasjonsDato, na.rm=T), sep='' ),
                 if ((minald>0) | (maxald<120)) {
                   paste('Pasienter fra ', min(RegData$Alder, na.rm=T), ' til ', max(RegData$Alder, na.rm=T), ' år', sep='')},
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                 if (op_gruppe[1] != '') {paste0('Operasjonsgruppe(r): ',
                                             paste(RegData$Operasjonsgrupper[match(op_gruppe, RegData$Op_gr)], collapse = ', '))},
                 if (ncsp[1] != '') {paste0('NCSP-kode(r): ', paste(ncsp[which(!is.na(ncsp[1:9]))], collapse=', '))},
                 if (length(ncsp) > 9) {paste0('  ', paste(ncsp[which(!is.na(ncsp[10:20]))+9], collapse=', '))},
                 if (length(ncsp) > 20) {paste0('  ', paste(ncsp[which(!is.na(ncsp[21:31]))+20], collapse=', '))},
                 if (length(ncsp) > 31) {paste0('  ', paste(ncsp[which(!is.na(ncsp[32:42]))+31], collapse=', '))},
                 if (elektiv %in% c(0,1)) {paste0('Hastegrad: ', c('Øyeblikkelig hjelp', 'Elektiv kirurgi')[elektiv+1])},
                 if (BMI[1] != '') {paste0('BMI-gruppe: ', paste(BMI, collapse=','))},
                 if (length(valgtShus)>1) {paste0('Valgte RESH: ', paste(as.character(valgtShus), collapse=', '))},
                 if (tilgang %in% c(1,2,3,5)) {paste0('Tilgang: ', c('Åpen', 'Laparoskopisk', 'Konvertert', '','Endoskopisk')[as.numeric(tilgang)])},
                 if ((minPRS>0) | (maxPRS<2)) {paste0('PRS-score fra ', sprintf('%.2f', min(RegData$PRSScore, na.rm=T)), ' til ',
                          sprintf('%.2f', max(RegData$PRSScore, na.rm=T)))},
                 if (ASA[1] != '') {paste0('ASA-grad: ', paste(ASA, collapse=','))},
                 if (whoEcog[1] != '') {paste0('WHO ECOG score: ', paste(whoEcog, collapse=','))},
                 if (forbehandling %in% 1:4) {paste0('Onkologisk forbehandling: ',
                                                     paste(c('Cytostatika', 'Stråleterapi', 'Komb. kjemo/radioterapi', 'Ingen')[forbehandling], collapse = ', '))},
                 if (malign %in% c(0,1)){paste0('Diagnose: ', c('Benign', 'Malign')[malign+1])}
  )


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
  return(invisible(UtData))
}