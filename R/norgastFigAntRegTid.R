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
norgastFigAntRegTid <- function(
    RegData, tittel='', width=800, height=700, sideTxt='Boområde/opptaksområde',
    decreasing=F, terskel=30, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
    legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, inkl_konf=F,
    datoFra='2014-01-01', datoTil='2050-12-31', minald=0, maxald=130, erMann=99,
    outfile='', preprosess=F, malign=99, elektiv=99, BMI='', tilgang='', minPRS=0,
    maxPRS=2.2, ASA='', whoEcog= '', forbehandling='', hentData=0, op_gruppe='',
    ncsp='', robotassiastanse=99, kun_ferdigstilte=TRUE, alletider=FALSE,
    accordion='')
{
  if (tittel[1] == '') {
    tittel <- 'Registrerende avdelinger i NORGAST'
  }

  if (alletider) {
    NorgastUtvalg <- NorgastUtvalg(
      RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
      maxald=maxald, erMann=erMann, elektiv=elektiv,
      BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
      ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
      op_gruppe=op_gruppe, ncsp=ncsp, robotassiastanse=robotassiastanse,
      kun_ferdigstilte=kun_ferdigstilte, accordion=accordion)
    RegData <- NorgastUtvalg$RegData
    utvalgTxt <- NorgastUtvalg$utvalgTxt
    NutvTxt <- length(utvalgTxt)

    N <- sort(table(RegData$Sykehusnavn), decreasing = decreasing)
    FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height,
                                    pointsizePDF=11, fargepalett='BlaaOff')
    farger <- FigTypUt$farger
    soyleFarger <- rep(farger[3], length(N))
    oldpar_mar <- par()$mar
    oldpar_fig <- par()$fig
    oldpar_oma <- par()$oma
    cexgr <- skriftStr

    N <- c(N, NA)
    vmarg <- max(0, strwidth(names(N), units='figure', cex=cexgr)*0.75)
    par('fig'=c(vmarg, 1, 0, 1))
    par('mar'=c(5.1, 4.1, 5.1, 5.1))
    par('oma'=c(0,1,NutvTxt,0))
    xmax <- max(N, na.rm = T)

    ypos <- barplot(N,
                    xlim=c(0,xmax),
                    names.arg=rep('', length(N)),
                    horiz=T, axes=F, space=0.2,
                    col=soyleFarger, border=NA, xlab = 'Antall registreringer')
    title(main = tittel)
    ypos <- as.numeric(ypos) #as.vector(ypos)
    yposOver <- max(ypos) + 0.5*diff(ypos)[1]
    axis(1,cex.axis=0.9)
    mtext( names(N), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
    N_plot <- N
    N_plot[N_plot==0] <- NA

    mtext( c(N[1:(length(N)-1)], "N"), side=4, line=5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)

  } else {

    ## Gjør utvalg basert på brukervalg (LibUtvalg)
    NorgastUtvalg <- NorgastUtvalg(
      RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
      maxald=maxald, erMann=erMann, elektiv=elektiv,
      BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
      ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
      op_gruppe=op_gruppe, ncsp=ncsp, robotassiastanse=robotassiastanse,
      kun_ferdigstilte=kun_ferdigstilte, accordion=accordion)
    RegData <- NorgastUtvalg$RegData
    utvalgTxt <- NorgastUtvalg$utvalgTxt
    NutvTxt <- length(utvalgTxt)

    RegData <- RegData[RegData$Aar > max(RegData$Aar)-3, ]

    N <- RegData %>%
      dplyr::group_by(Sykehusnavn, Aar) %>%
      summarise(N=dplyr::n()) %>%
      tidyr::pivot_wider(names_from = Aar, values_from = N,
                         values_fill = 0, id_expand = T) %>%
      as.data.frame()

    # tmp <- aggregate(RegData$ForlopsID, by=list(aar=RegData$Aar, sh=RegData$Sykehusnavn), length)
    # N <- tidyr::spread(tmp, 'aar', 'x')
    # rownames(N) <- N$sh
    rownames(N) <- as.character(N$Sykehusnavn)
    N <- N[, -1]
    N[is.na(N)] <- 0

    rekkefolge <- order(N[[dim(N)[2]]], decreasing = decreasing, na.last = F)
    N <- N[rekkefolge, ]

    FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height,
                                    pointsizePDF=11, fargepalett='BlaaOff')
    farger <- FigTypUt$farger
    soyleFarger <- rep(farger[3], length(N[,dim(N)[2]]))
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
      mtext( c(N[1:(dim(N)[1]-1),1], names(N)[1]), side=4, line=2.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( c(N[1:(dim(N)[1]-1),2], names(N)[2]), side=4, line=5.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( 'N', side=4, line=4.0, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)

      par(xpd=TRUE)
      points(y=ypos, x=N_plot[,1],cex=pktStr, pch= 19)
      par(xpd=FALSE)
      if (legPlass=='nede'){
        legend('bottomright', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',
               lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
               legend=names(N), ncol = 1)}
      if (legPlass=='top'){
        legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
               lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
               legend=names(N), ncol = dim(N)[2])
      }

    } else {
      # if (!inkl_konf) {
      mtext( c(N[1:(dim(N)[1]-1),1], names(N)[1]), side=4, line=2.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( c(N[1:(dim(N)[1]-1),2], names(N)[2]), side=4, line=5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( c(N[1:(dim(N)[1]-1),3], names(N)[3]), side=4, line=7.5, las=1, at=ypos, col=1, cex=cexgr, adj = 1)
      mtext( 'N', side=4, line=5.0, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)

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
      }
    }
  }
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=(NutvTxt-1):0, outer=TRUE)

  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)
  par('oma'= oldpar_oma)

  if (outfile != '') {dev.off()}
}
