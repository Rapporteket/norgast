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
                               reshID, enhetsUtvalg=1, preprosess=F, inkl_konf=F,
                               elektiv=99, BMI='', tilgang=99, valgtShus=c(''), minPRS=0,
                               maxPRS=2, ASA='', whoEcog= '', forbehandling=99, hentData=F, tidsenhet='Aar')
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

  RegData$TidsEnhet <- switch(tidsenhet,
                              Aar = RegData$Aar-min(RegData$Aar)+1,
                              Mnd = RegData$Mnd-min(RegData$Mnd)+1+(RegData$Aar-min(RegData$Aar))*12)

  if (tidsenhet == 'Mnd') {
    Tidtxt <- paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                    sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='.')
  }
  if (tidsenhet == 'Aar') {
    Tidtxt <- as.character(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)])
  }
  RegData$TidsEnhet <- factor(RegData$TidsEnhet, levels=1:max(RegData$TidsEnhet))


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
      indHoved <-which(as.numeric(RegData$AvdRESH)==reshID)
      smltxt <- 'landet forøvrig'
      indRest <- which(as.numeric(RegData$AvdRESH) != reshID)
    }
  }


  NHovedRes <- length(indHoved)
  NSmlRes <- length(indRest)

  #-------------------------Beregning av andel-----------------------------------------

  NTidRest <- tapply(RegData$Variabel[indRest], RegData$TidsEnhet[indRest], length)
  NTidHendRest <- tapply(RegData$Variabel[indRest], RegData$TidsEnhet[indRest],sum, na.rm=T)
  AndelRest <- NTidHendRest/NTidRest*100
  NTidHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'TidsEnhet'], length)
  NTidHendHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'TidsEnhet'],sum, na.rm=T)
  AndelHoved <- NTidHendHoved/NTidHoved*100
  Andeler <- rbind(AndelRest, AndelHoved)

  binomkonf <- function(n, N, konfnivaa) {
    binkonf <- matrix(nrow=2, ncol = length(n))
    for (i in 1:length(n)) {
      binkonf[,i] <- binom.test(n[i],N[i], alternative = 'two.sided', conf.level = konfnivaa)$conf.int[1:2]
    }
    return(invisible(binkonf))
  }

  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; grtxt2 <- PlotParams$grtxt2;
  stabel <- PlotParams$stabel; subtxt <- PlotParams$subtxt; incl_N <- PlotParams$incl_N;
  incl_pst <- PlotParams$incl_pst; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
  FigTypUt <- figtype(outfile=outfile, fargepalett=NorgastUtvalg$fargepalett)
  VarTxt <- PlotParams$VarTxt



  #----------FIGUR------------------------------
  #Hvis for få observasjoner..
  #if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & medSml == 1)) {
  if (length(indHoved) < 10 | (medSml ==1 & length(indRest)<10)) {
    #-----------Figur---------------------------------------
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {




    #-----------Figur---------------------------------------

    #Plottspesifikke parametre:
    FigTypUt <- figtype(outfile, fargepalett=NorgastUtvalg$fargepalett)
    farger <- FigTypUt$farger
    fargeHoved <- farger[3]
    fargeRest <- farger[1]
    NutvTxt <- length(utvalgTxt)
    hmarg <- 0.04+0.01*NutvTxt
    par('fig' = c(0,1,0,1-hmarg))
    cexleg <- 1	#Størrelse på legendtekst


    ymax <- min(119, 1.25*max(Andeler,na.rm=T))
    plot(Aartxt, AndelHoved,  font.main=1,  type='o', pch="'", col='white', #type='o',
         xlim= c(Aartxt[1], max(Aartxt)), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(Aartxt), max(Aartxt),length(Aartxt)-1)
         cex=2, xlab='Innleggelsesår', ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i') 	#Operasjonsår,

    #plot(Aartxt, Midt, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #ylim=c(ymin-0.05*ymax, ymax),
    #		#cex=0.8, cex.lab=0.9, cex.axis=0.9,
    #		ylab=c(ytxt,'med 95% konfidensintervall'),
    #		xlab='Operasjonsår', xaxt='n',
    #		sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
    axis(side=1, at = Aartxt)

    title(tittel, line=1, font.main=1)

    #Legge på linjer i plottet. Denne kan nok gjøres mer elegant...
    if ((ymax > 10) & (ymax < 40)) {lines(range(Aartxt),rep(10,2), col=farger[4])}
    if (ymax > 20) {lines(range(Aartxt),rep(20,2), col=farger[4])}
    if ((ymax > 30) & (ymax < 40)) {lines(range(Aartxt),rep(30,2), col=farger[4])}
    if (ymax > 40) {lines(range(Aartxt),rep(40,2), col=farger[4])}
    if (ymax > 60) {lines(range(Aartxt),rep(60,2), col=farger[4])}
    if (ymax > 80) {lines(range(Aartxt),rep(80,2), col=farger[4])}
    if (ymax > 100) {lines(range(Aartxt),rep(100,2), col=farger[4])}
    #		axis(2, at=c(0,20,40,60,80,100), pos=0),


    lines(Aartxt, AndelHoved, col=fargeHoved, lwd=3)
    points(Aartxt, AndelHoved, pch="'", cex=2, col=fargeHoved)
    text(Aartxt, AndelHoved, pos=3, NAarHendHoved, cex=0.9, col=fargeHoved)

    lines(Aartxt, AndelRest, col=fargeRest, lwd=3)
    points(Aartxt, AndelRest, pch="'", cex=2, col=fargeRest)	#}

    Ttxt <- paste('(Tall ved punktene angir antall ', VarTxt, ')', sep='')
    if (medSml == 1) {
      text(Aartxt, AndelRest, pos=3, NAarHendRest, cex=0.9, col=fargeRest)
      legend('topleft', border=NA, c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''),
                                     paste(smltxt, ' (N=', NSmlRes, ')', sep=''), Ttxt), bty='n', ncol=1, cex=cexleg,
             col=c(fargeHoved, fargeRest, NA), lwd=3)
    } else {
      legend('top', c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''), Ttxt),
             col=c(fargeHoved, NA), lwd=3, bty='n')
    }

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
    #------------------------------------------------------------------------------

  }













}



