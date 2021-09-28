#' Plot andeler/andeler i angitt format
#'
#' Denne funksjonen tar som input en dataramme med andeler over 3 år,
#' der radnavn angir grupperingsvariabel og kolonnenavn år. Funksjonen
#' returnerer et søyleplot hvor søylene representerer sist år, fyllt sirkel er året
#' før og åpen sirkel to år før
#'
#' @inheritParams FigAndeler
#' @param graaUt En vektor med navn på enheter som skal ha grå søyler
#' @return Et plot av andeler over tre år
#'
#' @export
#'
norgastIndikator_rapporteket <- function(RegData, valgtVar, tittel='', width=800, height=700, sideTxt='Boområde/opptaksområde',
                                   decreasing=F, terskel=10, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4, legPlass='top',
                                   minstekravTxt='Akseptabelt', maalTxt='Mål', graaUt=NA, inkl_konf=F, datoFra='2014-01-01', datoTil='2050-12-31',
                                   minald=0, maxald=130, erMann=99, outfile='', preprosess=F, malign=99, elektiv=99, hastegrad=99, BMI='',
                                   tilgang='', minPRS=0, maxPRS=2.2, ASA='', whoEcog= '', forbehandling='', dagtid =99,
                                   hentData=0, op_gruppe='', ncsp='', maalretn='hoy', lavDG='',
                                 lavDGtekst='Dekningsgrad < 60 %', hastegrad_hybrid=99)
{
  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- NorgastHentRegData(datoFra = datoFra, datoTil = datoTil)
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- NorgastPreprosess(RegData=RegData)
  }

  RegData <- RegData[RegData$Aar > max(RegData$Aar)-3, ] # behold bare siste 3 år

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NorgastUtvalg <- NorgastUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                                    maxald=maxald, erMann=erMann, elektiv=elektiv, hastegrad = hastegrad,
                                    BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS, dagtid = dagtid,
                                    ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                                    op_gruppe=op_gruppe, ncsp=ncsp, hastegrad_hybrid=hastegrad_hybrid)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt
  NutvTxt <- length(utvalgTxt)

  ## Preparer variabler for fremstilling i figur
  PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
  RegData <- PlotParams$RegData
  if (tittel[1] == '') {
    tittel <- paste0('Andel ', PlotParams$VarTxt)
  }
  if (inkl_konf) {tittel <- c(tittel, 'inkl. 95% konf. int.')}
  PlotParams$RegData <- NA

  tmp <- aggregate(RegData$Variabel, by=list(aar=RegData$Aar, sh=RegData$Sykehusnavn), sum)
  AntTilfeller <- tidyr::spread(tmp, 'aar', 'x')
  rownames(AntTilfeller) <- AntTilfeller$sh
  AntTilfeller <- AntTilfeller[, -1]
  AntTilfeller <- rbind(AntTilfeller, 'Norge'=colSums(AntTilfeller, na.rm = T))
  AntTilfeller[,paste0(names(AntTilfeller)[1], '-', names(AntTilfeller)[dim(AntTilfeller)[2]])] <- rowSums(AntTilfeller, na.rm = T)
  Utdata <- list(ant_tilfeller=AntTilfeller, N=NA)
  AntTilfeller <- AntTilfeller[, (dim(AntTilfeller)[2]-1):dim(AntTilfeller)[2]]

  tmp <- aggregate(RegData$Variabel, by=list(aar=RegData$Aar, sh=RegData$Sykehusnavn), length)
  N <- tidyr::spread(tmp, 'aar', 'x')
  rownames(N) <- N$sh
  N <- N[, -1]
  N[is.na(N)] <- 0
  N <- rbind(N, 'Norge'=colSums(N, na.rm = T))
  N[,paste0(names(N)[1], '-', names(N)[dim(N)[2]])] <- rowSums(N, na.rm = T)
  Utdata$N <- N
  N <- N[, (dim(N)[2]-1):dim(N)[2]]

  andeler <- AntTilfeller/N * 100

  # terskel <- 10
  andeler[N < terskel] <- NA
  andeler[rownames(andeler) %in% lavDG, ] <- NA

  if (decreasing){
    rekkefolge <- order(andeler[, dim(andeler)[2]], decreasing = decreasing, na.last = F)
  } else {
    rekkefolge <- order(andeler[, dim(andeler)[2]], decreasing = decreasing, na.last = F)
  }
  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]
  andeler[N[, dim(andeler)[2]]<terskel, 1:2] <- NA
  KI <- binomkonf(AntTilfeller[rekkefolge, dim(andeler)[2]], N[, dim(andeler)[2]])*100
  KI[, is.na(andeler[, dim(andeler)[2]])] <- NA
  pst_txt <- paste0(sprintf('%.0f', andeler[, dim(andeler)[2]]), ' %')
  # pst_txt[is.na(andeler[, dim(andeler)[2]])] <- paste0('N<', terskel, ' eller dekningsgrad mindre en 60 pst.')
  pst_txt[N[, dim(andeler)[2]]<terskel] <- paste0('N<', terskel)
  pst_txt[rownames(andeler) %in% lavDG] <- lavDGtekst
  pst_txt <- c(NA, pst_txt, NA, NA)

  FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], length(andeler[,dim(andeler)[2]]))
  soyleFarger[which(rownames(andeler)=='Norge')] <- farger[4]
  if (!is.na(graaUt[1])) {soyleFarger[which(rownames(andeler) %in% graaUt)] <- 'gray88'}
  soyleFarger <- c(NA, soyleFarger)

  # windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr
  if (inkl_konf) {
    rownames(andeler) <- paste0(rownames(andeler), ' (', N[, dim(N)[2]], ')')
    andeler <- rbind(andeler, c(NA,NA,NA))
    rownames(andeler)[dim(andeler)[1]] <- paste0('(N, ', names(andeler)[dim(andeler)[2]], ')')
    KI <- cbind(c(NA, NA), KI, c(NA, NA))
  } else {
    andeler <- rbind(andeler, c(NA,NA,NA))
    rownames(andeler)[dim(andeler)[1]] <- ''
  }



  vmarg <- max(0, strwidth(rownames(andeler), units='figure', cex=cexgr)*0.75)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 4.1, 5.1, 9.1))
  par('oma'=c(0,1,NutvTxt,0))

  if (inkl_konf) {
    par('mar'=c(5.1, 4.1, 5.1, 2.1))
    xmax <- min(max(KI, max(andeler, na.rm = T), na.rm = T)*1.15,100)
  } else {
    xmax <- min(100, 1.15*max(andeler, na.rm = T))
  }

  # if (dim(andeler)[1] < 15) {
    andeler <- rbind(c(NA,NA), andeler, c(NA,NA))
    rownames(andeler)[dim(andeler)[1]] <- '  '
    rownames(andeler)[1] <- ' '
  # }

  ypos <- barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
                   #font.main=1, cex.main=1.3,
                   # xlim=c(0,max(andeler, na.rm = T)*1.1),
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Andel (%)') # '#96BBE7'

  # fargerMaalNiva <-  c('#4fc63f', '#fbf850','#c6312a')
  fargerMaalNiva <-  c('aquamarine3','#fbf850', 'red')

  if (maal > minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=minstekrav, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (maal < minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=maal, ybottom=1, xright=minstekrav, ytop=max(ypos)-1.6, col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='lav') {
    # rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+0.4, col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='hoy') {
    # rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+0.4, col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}

  barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
          names.arg=rep('',dim(andeler)[1]),
           horiz=T, axes=F, space=c(0,0.3),
           col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)

  # title(main = tittel, outer=T)
  title(main = tittel)
  ypos <- as.numeric(ypos) #as.vector(ypos)
  yposOver <- max(ypos)-2 + 0.5*diff(ypos)[1]
  if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, yposOver), col=fargerMaalNiva[2], lwd=2)
    par(xpd=TRUE)
    text(x=minstekrav, y=yposOver, labels = minstekravTxt,
         pos = 4, cex=cexgr*0.65, srt = 90)
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, yposOver), col=fargerMaalNiva[1], lwd=2)
    barplot( t(andeler[, dim(andeler)[2]]), beside=T, las=1,
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=yposOver, labels = maalTxt, pos = 4, cex=cexgr*0.65, srt = 90) #paste0(maalTxt,maal,'%')
    par(xpd=FALSE)
  }
  if (inkl_konf){
    arrows(x0 = KI[1,], y0 = ypos, x1 = KI[2,], y1 = ypos,
           length=0.5/max(ypos), code=3, angle=90, lwd=1.8, col='gray') #, col=farger[1])
    legend('bottom', cex=0.9*cexgr, bty='n',
           lwd=1.8, lty = 1, pt.cex=1.8, col='gray',
           legend=paste0('Konfidensintervall ', names(N)[dim(N)[2]]))
  }

  axis(1,cex.axis=0.9)
  mtext( rownames(andeler), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  antAar <- dim(andeler)[2]

  if (dim(andeler)[2]==2) {
    if (!inkl_konf){
      mtext( c(N[,1], names(N)[1]), side=4, line=2.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( c(N[,2], names(N)[2]), side=4, line=5.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( 'N', side=4, line=4.0, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    }
    # else {
    #   mtext( '(N)', side=2, line=0.3, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    # }

    par(xpd=TRUE)
    points(y=ypos, x=andeler[,1],cex=pktStr, pch= 19)
    par(xpd=FALSE)
    # mtext( 'Boområde/opptaksområde', side=2, line=9.5, las=0, col=1, cex=cexgr)
    if (legPlass=='nede'){
      legend('bottomright', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=names(N), ncol = 1)}
    if (legPlass=='top'){
      legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=names(N), ncol = dim(andeler)[2])}
    if (legPlass=='topleft'){
    legend('topleft', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
           lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
           legend=names(N), ncol = dim(andeler)[2])}
    if (legPlass=='topright'){
      legend('topright', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=names(N), ncol = dim(andeler)[2])}
      # legend(0, yposOver+ diff(ypos)[1], yjust=0, xpd=TRUE, cex=0.9, bty='n', #bg='white', box.col='white',y=max(ypos),
      #        lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
      #        legend=names(N), ncol = dim(andeler)[2])

  } else {
    if (!inkl_konf) {
      mtext( c(N[,1], names(N)[1]), side=4, line=2.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( c(N[,2], names(N)[2]), side=4, line=5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( c(N[,3], names(N)[3]), side=4, line=7.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( 'N', side=4, line=5.0, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    }
    # else {
    #   mtext( '(N)', side=2, line=0.3, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    # }

    par(xpd=TRUE)
    points(y=ypos, x=andeler[,1],cex=pktStr) #'#4D4D4D'
    points(y=ypos, x=andeler[,2],cex=pktStr,pch= 19)
    par(xpd=FALSE)
    if (legPlass=='nede'){
      legend(x=82, y=ypos[2]+1 ,xjust=0, cex=cexgr, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
             legend=names(N) )}
    if (legPlass=='top'){
      legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
             legend=names(N), ncol = dim(andeler)[2])
      # legend(0, yposOver+ diff(ypos)[1], yjust=0, xpd=TRUE, cex=0.9, bty='n', #bg='white', box.col='white',y=max(ypos),
      #        lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
      #        legend=names(N), ncol = dim(andeler)[2]) #
    }
  }
  # mtext(sideTxt, WEST<-2, line=-1, cex=cexgr, outer=TRUE)
  #mtext(sideTxt, line=-1, cex=cexgr, outer=F)#WEST<-2,
  text(x=0, y=ypos, labels = pst_txt, cex=0.75, pos=4)#

  #Tekst som angir hvilket utvalg som er gjort
  # mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(5+0.8*((length(utvalgTxt)-1):0)))
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=(NutvTxt-1):0, outer=TRUE)

  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)
  par('oma'= oldpar_oma)
  # if (outfile != '') {dev.off()}

  # if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}
  if ( outfile != '') {dev.off()}

  return(invisible(Utdata))
}
