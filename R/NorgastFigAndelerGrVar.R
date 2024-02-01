#' Rate/andel for en gitt variabel for hvert sykehus
#'
#' Rater for valgt variabel for hvert sykehus
#'
#' Konfidensintervallet er basert på Clopper Pearsons "eksakte" metode for binominalfordelt data.
#'
#' @inheritParams FigAndeler
#'
#' @return En figur med tidsutvikling av rate over år
#'
#' @export
#'
NorgastFigAndelerGrVar <- function(RegData=0, valgtVar='', datoFra='2014-01-01', datoTil='2050-12-31',
                                   minald=0, maxald=130, erMann=99, outfile='', hastegrad_hybrid = 99,
                                   preprosess=F, inkl_konf=F, malign=99, Ngrense=10, modGlasgow='',
                                   elektiv=99, BMI='', tilgang='', valgtShus=c(''), minPRS=0, hastegrad = 99,
                                   maxPRS=2.2, ASA='', whoEcog= '', forbehandling='', hentData=0, op_gruppe='',
                                   ncsp='', robotassiastanse=99, kun_ferdigstilte=TRUE, tilgang_utvidet='',
                                   accordion='')
{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- NorgastHentRegData(datoFra = datoFra, datoTil = datoTil)
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- NorgastPreprosess(RegData=RegData)
  }

  if (valgtShus[1] != '') {RegData <- RegData[which(RegData$AvdRESH %in% as.numeric(valgtShus)), ]}


  ## Preparer variabler for fremstilling i figur
  PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
  RegData <- PlotParams$RegData
  PlotParams$RegData <- NA

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NorgastUtvalg <- NorgastUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                                 maxald=maxald, erMann=erMann, elektiv=elektiv, hastegrad = hastegrad,
                                 BMI=BMI, valgtShus=valgtShus, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                                 ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                                 op_gruppe=op_gruppe, ncsp=ncsp, modGlasgow=modGlasgow, hastegrad_hybrid = hastegrad_hybrid,
                                 robotassiastanse=robotassiastanse, kun_ferdigstilte=kun_ferdigstilte,
                                 tilgang_utvidet=tilgang_utvidet, accordion=accordion)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt

  grVar <- 'Sykehusnavn'
  RegData[ ,grVar] <- as.factor(as.character(RegData[ ,grVar]))
  # Ngrense <- 10		#Minste antall registreringer for at ei gruppe skal bli vist

  N <- dim(RegData)[1]
  Nvar <- tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)
  if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}
  AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
  AndelerGr <- round(100*Nvar/Ngr,2)

  KI <- binomkonf(Nvar, Ngr)*100
  KIHele <- binomkonf(sum(RegData$Variabel), N)*100

  indGrUt <- as.numeric(which(Ngr < Ngrense))
  if (length(indGrUt)==0) { indGrUt <- 0}
  AndelerGr[indGrUt] <- -0.001
  KI[, indGrUt] <- 0
  sortInd <- order(as.numeric(AndelerGr), decreasing=FALSE)
  Ngrtxt <- paste('N=', as.character(Ngr), sep='')	#
  Ngrtxt[indGrUt] <- paste0('N<', Ngrense)	#paste(' (<', Ngrense,')',sep='')	#

  AndelerGrSort <- AndelerGr[sortInd]
  KI <- KI[, sortInd]

  AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
  GrNavnSort <- paste0(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd])
  Ngr <- Ngr[sortInd]
  Nvar <- Nvar[sortInd]
  andeltxt <- paste(sprintf('%.1f',AndelerGrSort), '%',sep='') 	#round(as.numeric(AndelerGrSort),1)
  # if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}
  if (length(indGrUt)>0) {andeltxt[1:length(indGrUt)] <- ''}


  if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
    FigTypUt <- rapFigurer::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    if (dim(RegData)[1]>0) {
      tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
    } else {tekst <- 'Ingen registrerte data for dette utvalget'}
    title(main=tittel, cex=0.95)	#line=-8,
    text(0.5, 0.6, tekst, cex=1.2)
    #text(0.5, 0.3, , cex=1.2)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    if ( outfile != '') {dev.off()}
  } else {

    cexShNavn <- 0.85
    smltxt <- 'Hele landet'
    tittel <- PlotParams$tittel

    if (inkl_konf == 1) {
      tittel <- c(tittel, 'inkl. 95% konf. int.')
    }

    FigTypUt <- rapFigurer::figtype(outfile, height=3*800, fargepalett=NorgastUtvalg$fargepalett)
    farger <- FigTypUt$farger
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.7)
    #NB: strwidth oppfører seg ulikt avh. av device...
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    if (inkl_konf == 1) {
      xmax <- min(max(KI)*1.15,100)
    } else {
      xmax <- min(max(AndelerGrSort)*1.15,107)
    }

    pos <- barplot(as.numeric(AndelerGrSort), horiz=T, border=NA, col=farger[3], #main=tittel,
                   xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='Andel (%)', las=1, cex.names=0.7)

    # posKI <- pos[1:AntGr]
    posKI <- pos[(length(pos)-AntGr+1):length(pos)]
    ybunn <- 0.1
    # ytopp <- max(posKI)*1.03	 #min(posKI)
    ytopp <- max(pos)
    if (inkl_konf == 1) {
      polygon( c(rep(KIHele[1],2), rep(KIHele[2],2)), c(ybunn, ytopp, ytopp, ybunn),
               col=farger[4], border=farger[4])
      legend("topright", xpd=TRUE, xjust=0,  yjust=0, pch=c(NA, 15), pt.cex=2, cex=0.9, #y=ytopp+0.5,
             lwd=c(2,NA), col=c(farger[2], farger[4]),
             legend = c(paste0(smltxt, ': ', sprintf('%.1f', AndelHele), ' %'), paste0('95% konf.int., N=', N)),
             bty='o', bg='white', box.col='white')
    } else {
      legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
             legend=paste0(smltxt, ' (', sprintf('%.1f', AndelHele), '%), ', 'N=', N),
             bty='o', bg='white', box.col='white')
    }

    mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
    title(tittel, line=1, font.main=1, cex.main=1.2)

    if (inkl_konf != 1){
      text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
           las=1, cex=0.8, adj=0, col=farger[1])	#Andeler, hvert sykehus
    }

    barplot(as.numeric(AndelerGrSort), horiz=T, border=NA, col=farger[3], xlim=c(0, xmax), add=TRUE,
            font.main=1, las=1)

    lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)

    if (inkl_konf == 1){
      arrows(x0 = KI[1,(length(pos)-AntGr+1):length(pos)], y0 = posKI, x1 = KI[2,(length(pos)-AntGr+1):length(pos)], y1 = posKI,
             length=0.5/max(pos), code=3, angle=90, lwd=1.5, col=farger[1])
    }

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))



    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

    utData <- list(tittel = tittel, utvalgTxt = utvalgTxt, Andeler = AndelerGrSort, AndelHele=AndelHele,
                   KI=KI, KIHele=KIHele, Ngr=Ngr, Nvar=Nvar)
    return(invisible(utData))

  }
}

