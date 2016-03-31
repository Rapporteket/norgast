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

  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    indRest <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg == 1) {
      indHoved <-which(as.numeric(RegData$ReshId)==reshID)
      smltxt <- 'landet forøvrig'
      indRest <- which(as.numeric(RegData$ReshId) != reshID)
    }
  }

  #-------------------------Beregning av andel-----------------------------------------
  Aartxt <- min(RegData$Aar):max(RegData$Aar)
  RegData$Aar <- factor(RegData$Aar, levels=Aartxt)

  NAarRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest], length)
  NAarHendRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest],sum, na.rm=T)
  AndelRest <- NAarHendRest/NAarRest*100
  NAarHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'], length)
  NAarHendHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'],sum, na.rm=T)
  AndelHoved <- NAarHendHoved/NAarHoved*100
  Andeler <- rbind(AndelRest, AndelHoved)


  if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}




  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; grtxt2 <- PlotParams$grtxt2;
  stabel <- PlotParams$stabel; subtxt <- PlotParams$subtxt; incl_N <- PlotParams$incl_N;
  incl_pst <- PlotParams$incl_pst; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
  FigTypUt <- figtype(outfile=outfile, fargepalett=NorgastUtvalg$fargepalett, pointsizePDF=12)













}
