#' Tidstrend (år) av rate/andel for en gitt variabel
#'
#' Årlige (etterhvert også månedlige) rater for valgt variabel.
#' Hvis man har valgt å sammenlikne, vises  konfidensintervall for resten av landet (evt. annen
#' sammenlikningsgruppe) i bakgrunnen.
#'
#' Konfidensintervallet er basert på Clopper Pearsons "eksakte" metode for binominalfordelt data.
#'
#' @inheritParams FigAndeler
#'
#' @return En figur med tidsutvikling av rate over år
#'
#' @export
#'
NorgastFigAndelTid <- function(RegData=0, valgtVar='RELAPAROTOMY', datoFra='2014-01-01', datoTil='2050-12-31',
                               minald=0, maxald=130, erMann=99, op_gruppe=0, outfile='',
                               reshID, enhetsUtvalg=1, stabel=F, preprosess=F, inkl_konf=F,
                               elektiv=99, BMI='', tilgang=99, valgtShus=c(''), minPRS=0,
                               maxPRS=2, ASA='', whoEcog= '', forbehandling=99, hentData=F)
{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- NorgastHentRegData(datoFra = datoFra, datoTil = datoTil)
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- NorgastPreprosess(RegData=RegData)
  }

  ## Preparer variabler for fremstilling i figur
  PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
  RegData <- PlotParams$RegData
  PlotParams$RegData <- NA

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NorgastUtvalg <- NorgastLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                                    maxald=maxald, erMann=erMann, op_gruppe=op_gruppe, elektiv=elektiv,
                                    BMI=BMI, valgtShus=valgtShus, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                                    ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt

  if (valgtShus[1]!='') {
    valgtShus <- as.numeric(valgtShus)
    if (length(valgtShus)==1) {reshID<-valgtShus[1]}
  }

  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  }	#'Eget sykehus' #

  if (enhetsUtvalg!=0 & length(valgtShus)>1) {
    RegData$AvdRESH[RegData$AvdRESH %in% valgtShus] <- 99
    shtxt <- 'Ditt utvalg'
    reshID <- 99
  }

  #Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}	#{indHovedUt <- which(RegData$AvdRESH != reshID)}

  utvalg <- c('Hoved', 'Rest')
  Andeler <- list(Hoved = 0, Rest =0)

  indHoved <-which(RegData$AvdRESH == reshID)
  indRest <- which(RegData$AvdRESH != reshID)
  RegDataLand <- RegData
  ind <- list(Hoved=indHoved, Rest=indRest)
  # NHoved <- dim(RegData)[1]
  Nrest <- 0

  for (teller in 1:2) {
    if (teller==2 & enhetsUtvalg != 1) {break}

    if (enhetsUtvalg == 1) {RegData <- RegDataLand[switch(utvalg[teller], Hoved = ind$Hoved, Rest=ind$Rest), ]}

    #Variablene kjøres to ganger for sammenligning med Resten.

    if (teller == 1) {Andeler$Hoved <- round(table(RegData$VariabelGr)/length(RegData$VariabelGr)*100,2)
    NHoved <- dim(RegData)[1]}
    if (teller == 2) {Andeler$Rest <- round(table(RegData$VariabelGr)/length(RegData$VariabelGr)*100,2)
    Nrest <- dim(RegData)[1]}
  }


  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; grtxt2 <- PlotParams$grtxt2;
  stabel <- PlotParams$stabel; subtxt <- PlotParams$subtxt; incl_N <- PlotParams$incl_N;
  incl_pst <- PlotParams$incl_pst; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
  FigTypUt <- figtype(outfile=outfile, fargepalett=NorgastUtvalg$fargepalett, pointsizePDF=12)













}
