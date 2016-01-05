NorgastLibUtvalg <- function(RegData, datoFra, datoTil, minald, maxald, erMann, op_gruppe, fargepalett='BlaaRapp')
{
  #Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
  # Inndata:
  #		erMann - kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
  #		minald - alder, fra og med
  #		maxald - alder, til og med
  #		datoFra <- '2010-01-01'    # min og max dato i utvalget vises alltid i figuren.
  #		datoTil <- '2013-05-25'
  #   op_gruppe - hvilken operasjonsgruppe, default alle

  #Hvis "Variabel" ikke definert
  if (length(which(names(RegData) == 'Variabel')) == 0 ) {RegData$Variabel <- 0}
  Ninn <- dim(RegData)[1]
  indVarMed <- intersect(intersect(intersect(intersect(which(RegData$Variabel != 'NA'), which(RegData$Variabel != 'NaN')), 
                         which(RegData$Variabel != '')), which(!is.na(RegData$Variabel))), which(!is.nan(RegData$Variabel)))
  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  indDato <- which(RegData$OperasjonsDato >= as.POSIXlt(datoFra) & RegData$OperasjonsDato <= as.POSIXlt(datoTil))
  indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {indKj <- 1:Ninn}
  indOp_gr <- if (op_gruppe %in% c(1,2,3,4,5,6,9)){which(RegData$Op_gr == op_gruppe)} else {indOp_gr <- 1:Ninn}
  
  indMed <- intersect(intersect(indAld, intersect(indDato, intersect(indKj, indVarMed))), indOp_gr)
  RegData <- RegData[indMed,]
  
  utvalgTxt <- c(paste('Operasjonsdato: ', 
                       min(RegData$OperasjonsDato, na.rm=T), ' til ', max(RegData$OperasjonsDato, na.rm=T), sep='' ),
                 if ((minald>0) | (maxald<120)) {
                   paste('Pasienter fra ', min(RegData$Alder, na.rm=T), ' til ', max(RegData$Alder, na.rm=T), ' år', sep='')},
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                 if (op_gruppe %in% c(1,2,3,4,5,6,9)) {paste('Operasjonsgruppe: ', c('Kolonreseksjoner', 'Rektumreseksjoner', 
                                                        'Øsofagusreseksjoner', 'Ventrikkelreseksjoner', 'Leverreseksjoner',
                                                        "Whipples operasjon", rep('',2), 'Øvrige')[op_gruppe], sep='')}
  )
  
  
  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
  return(invisible(UtData)) 
}