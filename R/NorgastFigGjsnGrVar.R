#' Gjennomsnitt av valgt variabel pr grupperingsvariabel
#'
#' Denne funksjonen beregner gjennomsnitt for valgt variabel og plotter de per grupperingsvariabel (vanligvis sykehus).
#' Konfidensintervaler kan inkluderes.
#'
#' Her kan detaljer skrives
#'
#' @inheritParams FigAndeler
#'
#' @return En figur med gjennomsnitt av valgt variabel pr grupperingsvariabel
#'
#' @export
#'
NorgastFigGjsnGrVar <- function(RegData=0, valgtVar='Alder', datoFra='2014-01-01', datoTil='2050-12-31',
                                     minald=0, maxald=130, erMann=99, outfile='',
                                     preprosess=F, malign=99, Ngrense=30,
                                     elektiv=99, BMI='', tilgang='', valgtShus=c(''), minPRS=0,
                                     maxPRS=2.2, ASA='', whoEcog= '', forbehandling='', hentData=0, op_gruppe='', ncsp='')

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


  grVar <- 'Sykehusnavn'
  smltxt <- 'alle sykehus'

  RegData$Variabel <- RegData[, valgtVar]
  RegData <- RegData[!is.na(RegData$Variabel), ]

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NorgastUtvalg <- NorgastUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                                 maxald=maxald, erMann=erMann, elektiv=elektiv,
                                 BMI=BMI, valgtShus=valgtShus, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                                 ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                                 op_gruppe=op_gruppe, ncsp=ncsp)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt

  RegData[ ,grVar] <- as.factor(as.character(RegData[ ,grVar]))
  N <- dim(RegData)[1]
  if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}

  # Ngrense <- 30		#Minste antall registreringer for at ei gruppe skal bli vist

  Ngrtxt <- paste(', N=', as.character(Ngr), sep='')
  indGrUt <- as.numeric(which(Ngr < Ngrense))
  if (length(indGrUt)==0) { indGrUt <- 0}
  Ngrtxt[indGrUt] <- paste(' (<', Ngrense,')',sep='')

  vt <- switch(valgtVar,
               'BMI' = 'BMI',
               'VekttapProsent' = 'vekttap i prosent',
               'ModGlasgowScore' = 'modifisert Glasgow score',
               'Alder' = 'alder',
               'PRSScore' = 'mE-PASS'
  )
  xaksetxt <- switch(valgtVar,
                     'BMI' = 'BMI',
                     'VekttapProsent' = 'Vekttap %',
                     'ModGlasgowScore' = 'Modifisert Glasgow score',
                     'Alder' = 'Alder (år)',
                     'PRSScore' = 'PRSScore'
  )

  tittel <- paste0('Gjennomsnittlig ', vt)

  if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
    FigTypUt <- figtype(outfile)
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
    dummy0 <- -0.001
    Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
    SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
    utresultat <- cbind(Gjsn=Gjsn, KIned=Gjsn-1.96*SE, KIopp=Gjsn+1.96*SE, N=Ngr)
    Gjsn[indGrUt] <- dummy0
    SE[indGrUt] <- 0
    sortInd <- order(Gjsn, decreasing=TRUE)
    Midt <- as.numeric(Gjsn[sortInd])
    KIned <- Gjsn[sortInd] - 1.96*SE[sortInd]
    KIopp <- Gjsn[sortInd] + 1.96*SE[sortInd]
    MidtHele <- round(mean(RegData$Variabel),1)
    KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-1.96,1.96)
    utresultat <- rbind(utresultat, Totalt=c(MidtHele, KIHele[1], KIHele[2], N))


  GrNavnSort <- paste(names(Ngr)[sortInd], Ngrtxt[sortInd], sep='')
  AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))

  #--------------------------FIGUR---------------------------------------------------
  soyletxt <- c(sprintf('%.1f',Midt[1:AntGr]), rep('',length(Ngr)-AntGr))	#	#round(Midt[1:AntGr],1)
  # xmax <-  min(1.1*max(c(Midt, KIned, KIopp)), 1.4*max(Midt))
  xmax <-  1.1*max(c(Midt, KIned, KIopp))
  cexGrNavn <- 0.8
  cexSoyletxt <- 0.75

  # x11()
  FigTypUt <- figtype(outfile, height=3*800, fargepalett=NorgastUtvalg$fargepalett)	#res=96,
  farger <- FigTypUt$farger
  #Tilpasse marger for å kunne skrive utvalgsteksten
  NutvTxt <- length(utvalgTxt)
  vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexGrNavn)*0.7)
  #NB: strwidth oppfører seg ulikt avh. av device...
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

  pos <- barplot(Midt, horiz=T, border=NA, col=farger[3],
                 xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='', las=1, cex.names=cexGrNavn)
  indGrUtPlot <- AntGr+(1:length(indGrUt))
  posKI <- pos[1:AntGr]
  ybunn <- 0
  ytopp <- max(posKI)*1.03	 #min(posKI)
  if (valgtVar=='VekttapProsent') {
    lines(x=rep(MidtHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
  }
  polygon( c(rep(KIHele[1],2), rep(KIHele[2],2)), c(ybunn, ytopp, ytopp, ybunn),
           col=farger[4], border=farger[4])
  lines(x=rep(MidtHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
  legend("topright", xpd=TRUE, xjust=0,  yjust=0, pch=c(NA, 15), pt.cex=2, cex=0.9, #y=ytopp+0.5,
         lwd=c(2,NA), col=c(farger[2], farger[4]),
         legend = c(paste(smltxt, ': ', round(MidtHele, 1), sep=''), paste('95% konf.int., N=', N,sep='' )),
         bty='o', bg='white', box.col='white')

  barplot(Midt, horiz=T, border=NA, col=farger[3], xlim=c(0, xmax), add=TRUE,
          font.main=1, xlab = xaksetxt, las=1) 	#xlim=c(0,ymax), #, cex.names=0.5
  title(tittel, font.main=1)
  title('med 95% konfidensintervall', line=0.5, font.main=1, cex.main=0.95)
  mtext(at=pos+0.1, GrNavnSort, side=2, las=1, cex=cexGrNavn, adj=1, line=0.25)	#Sykehusnavn
  text(x=max(strwidth(soyletxt, units='user', cex=cexSoyletxt)), y=pos+0.1,
       soyletxt, las=1, cex=cexSoyletxt, adj=1, col=farger[4])

  avst <- 0.8
  utvpos <- 3	#Startlinje for teksten
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

  options(warn = -1)	#Unngå melding om KI med lengde 0. Fungerer av en eller annen grunn ikke i pdf.
  arrows(x0=Midt[-indGrUtPlot]*0.999, y0=posKI, x1=KIopp[-indGrUtPlot], y1=posKI,
         length=0.5/max(pos), code=2, angle=90, lwd=1.5, col=farger[1])
  arrows(x0=Midt[-indGrUtPlot]*1.001, y0=posKI, x1=KIned[-indGrUtPlot], y1=posKI,
         length=0.5/max(pos), code=2, angle=90, lwd=1.5, col=farger[1])
  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}

  return(invisible(list(tittel = tittel, utvalgTxt = utvalgTxt, res=utresultat)))
  }

}
