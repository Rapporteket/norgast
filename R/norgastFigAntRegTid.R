#' Plot andeler/andeler i angitt format
#'
#' Denne funksjonen tar som input en dataramme med andeler over 3 år,
#' der radnavn angir grupperingsvariabel og kolonnenavn år. Funksjonen
#' returnerer et søyleplot hvor søylene representerer sist år, fyllt sirkel er året
#' før og åpen sirkel to år før
#'
#' @param AntTilfeller En dataramme med antall i spesifisert form
#' @param outfile Angir filnavn og format på figuren som returneres,
#' @param N En dataramme med nevneren i andelsberegningen
#' @param graaUt En vektor med navn på enheter som skal ha grå søyler
#' @return Et plot av andeler over tre år
#'
#' @export
#'
norgastFigAntRegTid <- function(RegData, tittel='', width=800, height=700, sideTxt='Boområde/opptaksområde',
                                   decreasing=F, terskel=30, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4, legPlass='top',
                                   minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, inkl_konf=F, datoFra='2014-01-01', datoTil='2050-12-31',
                                   minald=0, maxald=130, erMann=99, outfile='', preprosess=F, malign=99, elektiv=99, BMI='',
                                   tilgang=99, minPRS=0, maxPRS=2, ASA='', whoEcog= '', forbehandling=99,
                                   hentData=0, reseksjonsGr='', ncsp='')
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
  # PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
  # RegData <- PlotParams$RegData
  if (tittel[1] == '') {
    tittel <- 'Registrerende avdelinger i NoRGast'
  }
  # PlotParams$RegData <- NA

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NorgastUtvalg <- NorgastLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                                    maxald=maxald, erMann=erMann, elektiv=elektiv,
                                    BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                                    ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                                    reseksjonsGr=reseksjonsGr, ncsp=ncsp)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt
  NutvTxt <- length(utvalgTxt)

  RegData <- RegData[RegData$Aar > max(RegData$Aar)-3, ]

  tmp <- aggregate(RegData$ForlopsID, by=list(aar=RegData$Aar, sh=RegData$Sykehusnavn), length)
  N <- tidyr::spread(tmp, 'aar', 'x')
  rownames(N) <- N$sh
  N <- N[, -1]
  N[is.na(N)] <- 0

  if (decreasing){
    rekkefolge <- order(N[, dim(N)[2]], decreasing = decreasing, na.last = F)
  } else {
    rekkefolge <- order(N[, dim(N)[2]], decreasing = decreasing, na.last = F)
  }
  N <- N[rekkefolge, ]

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], length(N[,dim(N)[2]]))

  windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr

  N <- rbind(N, c(NA,NA,NA))
  rownames(N)[dim(N)[1]] <- ''


  vmarg <- max(0, strwidth(rownames(N), units='figure', cex=cexgr)*0.75)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 4.1, 5.1, 9.1))
  par('oma'=c(0,1,NutvTxt,0))

  xmax <- max(N, na.rm = T)
  ypos <- barplot( t(N[,dim(N)[2]]), beside=T, las=1,
                   #font.main=1, cex.main=1.3,
                   # xlim=c(0,max(andeler, na.rm = T)*1.1),
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(N)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Antall registreringer') # '#96BBE7'

  title(main = tittel)
  ypos <- as.numeric(ypos) #as.vector(ypos)
  yposOver <- max(ypos) + 0.5*diff(ypos)[1]
  axis(1,cex.axis=0.9)
  mtext( rownames(N), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  antAar <- dim(N)[2]
  N_plot <- N
  N_plot[N_plot==0] <- NA

  if (dim(N)[2]==2) {
    # if (!inkl_konf){
      mtext( c(N[1:(dim(N)[1]-1),1], names(N)[1]), side=4, line=2.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( c(N[1:(dim(N)[1]-1),2], names(N)[2]), side=4, line=5.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( 'N', side=4, line=4.0, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    # }
    # else {
    #   mtext( '(N)', side=2, line=0.3, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    # }

    par(xpd=TRUE)
    points(y=ypos, x=N_plot[,1],cex=pktStr, pch= 19)
    par(xpd=FALSE)
    # mtext( 'Boområde/opptaksområde', side=2, line=9.5, las=0, col=1, cex=cexgr)
    if (legPlass=='nede'){
      legend('bottomright', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=names(N), ncol = 1)}
    if (legPlass=='top'){
      legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=names(N), ncol = dim(N)[2])
      # legend(0, yposOver+ diff(ypos)[1], yjust=0, xpd=TRUE, cex=0.9, bty='n', #bg='white', box.col='white',y=max(ypos),
      #        lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
      #        legend=names(N), ncol = dim(andeler)[2])
      } #

  } else {
    # if (!inkl_konf) {
      mtext( c(N[1:(dim(N)[1]-1),1], names(N)[1]), side=4, line=2.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( c(N[1:(dim(N)[1]-1),2], names(N)[2]), side=4, line=5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( c(N[1:(dim(N)[1]-1),3], names(N)[3]), side=4, line=7.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( 'N', side=4, line=5.0, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    # }
    # else {
    #   mtext( '(N)', side=2, line=0.3, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    # }

    par(xpd=TRUE)
    points(y=ypos, x=N_plot[,1],cex=pktStr) #'#4D4D4D'
    points(y=ypos, x=N_plot[,2],cex=pktStr,pch= 19)
    par(xpd=FALSE)
    if (legPlass=='nede'){
      legend(x=82, y=ypos[2]+1 ,xjust=0, cex=cexgr, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
             legend=names(N) )}
    if (legPlass=='top'){
      legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
             legend=names(N), ncol = dim(N)[2])
      # legend(0, yposOver+ diff(ypos)[1], yjust=0, xpd=TRUE, cex=0.9, bty='n', #bg='white', box.col='white',y=max(ypos),
      #        lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
      #        legend=names(N), ncol = dim(andeler)[2]) #
    }
  }
  # mtext(sideTxt, WEST<-2, line=-1, cex=cexgr, outer=TRUE)
  #mtext(sideTxt, line=-1, cex=cexgr, outer=F)#WEST<-2,
  # text(x=0, y=ypos, labels = pst_txt, cex=0.75, pos=4)#

  #Tekst som angir hvilket utvalg som er gjort
  # mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(5+0.8*((length(utvalgTxt)-1):0)))
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=(NutvTxt-1):0, outer=TRUE)

  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)
  par('oma'= oldpar_oma)
  # if (outfile != '') {dev.off()}

  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}

}
