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
norgastIndikator_rapporteket <-
  function(RegData, valgtVar, tittel='', width=800, height=700,
           sideTxt='Boområde/opptaksområde', decreasing=F, terskel=10, minstekrav = NA,
           maal = NA, skriftStr=1.3, pktStr=1.4, legPlass='top', minstekravTxt='Akseptabelt',
           maalTxt='Mål', graaUt=NA, inkl_konf=F, datoFra='2014-01-01', datoTil='2050-12-31',
           minald=0, maxald=130, erMann=99, outfile='', preprosess=F, malign=99, elektiv=99,
           hastegrad=99, BMI='', tilgang='', minPRS=0, maxPRS=2.2, ASA='', whoEcog= '',
           forbehandling='', dagtid =99, hentData=0, op_gruppe='', ncsp='', maalretn='hoy',
           lavDG='', lavDGtekst='Dekningsgrad < 60 %', hastegrad_hybrid=99,
           robotassiastanse=99, kun_ferdigstilte=TRUE, prikktall=TRUE, pst_kolonne=FALSE)
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
                                   op_gruppe=op_gruppe, ncsp=ncsp, hastegrad_hybrid=hastegrad_hybrid,
                                   robotassiastanse=robotassiastanse, kun_ferdigstilte=kun_ferdigstilte)
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
    AntTilfeller <- AntTilfeller[, (dim(AntTilfeller)[2]-1):dim(AntTilfeller)[2]]
    Utdata <- list(ant_tilfeller=AntTilfeller, N=NA)

    tmp <- aggregate(RegData$Variabel, by=list(aar=RegData$Aar, sh=RegData$Sykehusnavn), length)
    N <- tidyr::spread(tmp, 'aar', 'x')
    rownames(N) <- N$sh
    N <- N[, -1]
    N[is.na(N)] <- 0
    N <- rbind(N, 'Norge'=colSums(N, na.rm = T))
    N[,paste0(names(N)[1], '-', names(N)[dim(N)[2]])] <- rowSums(N, na.rm = T)
    N <- N[, (dim(N)[2]-1):dim(N)[2]]
    Utdata$N <- N

    andeler <- AntTilfeller/N * 100

    if (!decreasing) {
      andeler[N < terskel] <- -1
    } else {
      andeler[N < terskel] <- max(andeler[, dim(andeler)[2]])+1
    }
    andeler[rownames(andeler) %in% lavDG, ] <- NA
    rekkefolge <- order(andeler[[dim(andeler)[2]]], decreasing = decreasing, na.last = F)

    andeler <- andeler[rekkefolge, ]
    N <- N[rekkefolge, ]
    andeler[N < terskel] <- NA
    andeler[N[, dim(andeler)[2]]<terskel, 1:2] <- NA
    KI <- binomkonf(AntTilfeller[rekkefolge, dim(andeler)[2]], N[, dim(andeler)[2]])*100
    KI[, is.na(andeler[, dim(andeler)[2]])] <- NA
    pst_txt <- paste0(sprintf('%.0f', andeler[, dim(andeler)[2]]), ' %')
    pst_txt[N[, dim(andeler)[2]]<terskel] <- paste0('N<', terskel)
    pst_txt[rownames(andeler) %in% lavDG] <- lavDGtekst
    pst_txt <- c(NA, pst_txt, NA, NA)
    pst_txt_prikk <- paste0(sprintf('%.0f', andeler[, 1]), ' %')
    pst_txt_prikk[N[, 1]<terskel] <- NA
    pst_txt_prikk[rownames(andeler) %in% lavDG] <- NA
    pst_txt_prikk <- c(NA, pst_txt_prikk, NA, NA)


    FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height,
                                    pointsizePDF=11, fargepalett='BlaaOff')
    farger <- FigTypUt$farger
    soyleFarger <- rep(farger[3], length(andeler[,dim(andeler)[2]]))
    soyleFarger[which(rownames(andeler)=='Norge')] <- farger[4]
    if (!is.na(graaUt[1])) {soyleFarger[which(rownames(andeler) %in% graaUt)] <- 'gray88'}
    soyleFarger <- c(NA, soyleFarger)

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
    # par('mar'=c(5.1, 4.1, 5.1, 9.1))
    if (pst_kolonne) {par('mar'=c(5.1, 4.1, 5.1, 9.1)) }
    par('oma'=c(0,1,NutvTxt,0))

    if (inkl_konf) {
      par('mar'=c(5.1, 4.1, 5.1, 2.1))
      if (pst_kolonne) {par('mar'=c(5.1, 4.1, 5.1, 9.1)) }
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
                     xlim=c(0,xmax),
                     names.arg=rep('',dim(andeler)[1]),
                     horiz=T, axes=F, space=c(0,0.3),
                     col=soyleFarger, border=NA, xlab = 'Andel (%)') # '#96BBE7'

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
    title(main = tittel, xpd = TRUE)
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
    grtxt_bold <- rownames(andeler)
    grtxt_bold[which(substr(grtxt_bold, 1, 5) =='Norge')] <-
      paste0("$\\textbf{", grtxt_bold[which(substr(grtxt_bold, 1, 5) =='Norge')], "}")
    mtext(latex2exp::TeX(grtxt_bold), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
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

    if (prikktall) {
      text(x=0, y=ypos, labels = pst_txt, cex=0.75, pos=4)#
      text(x=andeler[,1], y=ypos, labels = pst_txt_prikk, cex=0.75, pos=4, xpd = T)}

    if (pst_kolonne) {
      mtext( pst_txt_prikk, side=4, line=3.5, las=1, at=ypos, col=1, cex=cexgr*0.75, adj = 1)
      mtext( pst_txt, side=4, line=7.5, las=1, at=ypos, col=1, cex=cexgr*0.75, adj = 1)
      mtext( names(N)[1], side=4, line=3.5, las=1, at=max(ypos), col=1, cex=cexgr*0.75, adj = 1, font = 2)
      mtext( names(N)[2], side=4, line=7.5, las=1, at=max(ypos), col=1, cex=cexgr*0.75, adj = 1, font = 2)
    }

    #Tekst som angir hvilket utvalg som er gjort
    # mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(5+0.8*((length(utvalgTxt)-1):0)))
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=(NutvTxt-1):0, outer=TRUE)

    par('mar'= oldpar_mar)
    par('fig'= oldpar_fig)
    par('oma'= oldpar_oma)
    # if (outfile != '') {dev.off()}

    # if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}
    if ( outfile != '') {dev.off()}

    Utdata$utvalgTxt <- utvalgTxt
    Utdata$tittel <- tittel
    return(invisible(Utdata))
  }


#' Plot andeler av valgt variabel per sykehus og per gruppe
#'
#' Plotter andeler av valgt variabel per sykehus og per gruppe
#'
#' @inheritParams FigAndeler
#' @param graaUt En vektor med navn på enheter som skal ha grå søyler
#' @return Et plot av andeler over tre år
#'
#' @export
#'
norgastIndikator_gruppert <-
  function(RegData, valgtVar, tittel='', width=800, height=700,
           decreasing=F, terskel=10, minstekrav = NA,
           maal = NA, skriftStr=1.3, pktStr=1.4, legPlass='top', minstekravTxt='Akseptabelt',
           maalTxt='Mål', graaUt=NA, inkl_konf=F, datoFra='2014-01-01', datoTil='2050-12-31',
           minald=0, maxald=130, erMann=99, outfile='', preprosess=F, malign=99, elektiv=99,
           hastegrad=99, BMI='', tilgang='', minPRS=0, maxPRS=2.2, ASA='', whoEcog= '',
           forbehandling='', dagtid =99, hentData=0, op_gruppe='', ncsp='', maalretn='hoy',
           lavDG='', lavDGtekst='Dekningsgrad < 60 %', hastegrad_hybrid=99, inset = 0,
           robotassiastanse=99, kun_ferdigstilte=TRUE, prikktall=TRUE, inkl_N = FALSE,
           Grvar1 = "Sykehusnavn", Grvar2 = "Malign", ltop=2, lbunn=1,rotermaaltxt=45,
           ny_anastomose=99, pst_kolonne=TRUE)
  {
    # RegData<-aux %>% filter(Op_gr==2); valgtVar<-"Anastomoselekkasje"; tittel=''; width=800; height=700;
    # sideTxt='Boområde/opptaksområde'; decreasing=F; terskel=5; minstekrav = NA;
    # maal = NA; skriftStr=1.3; pktStr=1.4; legPlass='topleft'; minstekravTxt='Moderat \nmåloppnåelse';
    # maalTxt='Høy \nmåloppnåelse'; graaUt=NA; inkl_konf=T; datoFra='2014-01-01'; datoTil='2050-12-31';
    # minald=0; maxald=130; erMann=99; outfile=''; preprosess=F; malign=99; elektiv=99;
    # hastegrad=99; BMI=''; tilgang=''; minPRS=0; maxPRS=2.2; ASA=''; whoEcog= '';
    # forbehandling=''; dagtid =99; hentData=0; op_gruppe=''; ncsp=''; maalretn='hoy';
    # lavDG=''; lavDGtekst='Dekningsgrad < 60 %'; hastegrad_hybrid=99;
    # robotassiastanse=99; kun_ferdigstilte=TRUE; prikktall=TRUE;
    # Grvar1 = "Sykehusnavn"; Grvar2 = "Tilgang_utvidet"; ltop=2; lbunn=1; inset=0; #Grvar2 = "maligndiag";
    # prikktall = FALSE; inkl_N = FALSE; valgtVar = "mortalitet90"; ny_anastomose=99;
    # minstekrav = 8; maal = 5; rotermaaltxt=45; lavDG=graaUt_rektum;pst_kolonne=T
    # RegData$Malign<-factor(RegData$Malign, levels = 0:1, labels = c("Benign", "Malign"))
    # RegData[,Grvar2] <- as.factor(RegData[,Grvar2])

    RegData <- RegData[RegData$Aar > max(RegData$Aar)-3, ] # behold bare siste 3 år
    RegData <- RegData %>% dplyr::filter(!is.na( !! sym(Grvar1 )),
                                         !is.na( !! sym(Grvar2 )))
    RegData[, Grvar2] <- as.factor(RegData[, Grvar2])

    ## Gjør utvalg basert på brukervalg (LibUtvalg)
    NorgastUtvalg <- NorgastUtvalg(
      RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
      maxald=maxald, erMann=erMann, elektiv=elektiv, hastegrad = hastegrad,
      BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS, dagtid = dagtid,
      ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
      op_gruppe=op_gruppe, ncsp=ncsp, hastegrad_hybrid=hastegrad_hybrid,
      robotassiastanse=robotassiastanse, kun_ferdigstilte=kun_ferdigstilte,
      ny_anastomose=ny_anastomose)
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

    if ("Variabel" %in% names(RegData)) {
      Samlet <- RegData %>%
        dplyr::summarise(antall = sum(Variabel, na.rm = T),
                         N = sum(!is.na(Variabel)),
                         andel = antall/N*100,
                         .by = c(!! sym(Grvar1 ), !! sym(Grvar2 ), Aar))
    } else {
      Samlet <- RegData %>%
        dplyr::summarise(antall = sum(!! sym(valgtVar ), na.rm = T),
                         N = sum(!is.na(!! sym(valgtVar ))),
                         andel = antall/N*100,
                         .by = c(!! sym(Grvar1 ), !! sym(Grvar2 ), Aar))}


    aux <- Samlet %>% dplyr::select(-c(N, andel)) %>%
      tidyr::pivot_wider(id_expand = TRUE, names_from = Aar, values_from = antall,
                         values_fill = 0, names_sort = TRUE) %>%
      dplyr::mutate(tot = rowSums(across(-c(1,2))) )
    names(aux)[dim(aux)[2]] <- paste0(names(aux)[3], '-', names(aux)[dim(aux)[2]-1])
    aux <- aux[, c(1,2,dim(aux)[2]-1, dim(aux)[2])]

    AntTilfeller <- dplyr::bind_rows(
      aux,
      aux %>% select(-Sykehusnavn) %>%
        summarise(across(everything(), ~sum(.)), .by = !! sym(Grvar2 )) %>%
        mutate(Sykehusnavn = "Norge"))

    aux <- Samlet %>% dplyr::select(-c(antall, andel)) %>%
      tidyr::pivot_wider(id_expand = TRUE, names_from = Aar, values_from = N,
                         values_fill = 0, names_sort = TRUE) %>%
      dplyr::mutate(tot = rowSums(across(-c(1,2))) )
    names(aux)[dim(aux)[2]] <- paste0(names(aux)[3], '-', names(aux)[dim(aux)[2]-1])
    aux <- aux[, c(1,2,dim(aux)[2]-1, dim(aux)[2])]

    N <- dplyr::bind_rows(
      aux,
      aux %>% select(-Sykehusnavn) %>%
        summarise(across(everything(), ~sum(.)), .by = !! sym(Grvar2 )) %>%
        mutate(Sykehusnavn = "Norge"))

    andeler <- N
    andeler[,3:4] <- AntTilfeller[, 3:4]/N[,3:4]*100

    total <- Samlet %>% summarise(antall = sum(antall, na.rm = T),
                                  N=sum(N, na.rm = T), .by = !! sym(Grvar1 )) %>%
      janitor::adorn_totals(name="Norge") %>% mutate(andel_tot=antall/N*100) %>%
      select(!! sym(Grvar1 ), andel_tot)

    andeler <- left_join(andeler, total, Grvar1) %>%
      left_join(N[, c(1,2,4)], by = c(Grvar1, Grvar2), suffix = c("", "_N"))

    andeler <- andeler %>%
      rename(N=6) %>%
      mutate(Nmax = max(N), .by = !! sym(Grvar1 ))


    andeler[andeler[[Grvar1]] %in% lavDG, 3:5] <- NA
    andeler[N[[4]]<terskel, 3:4] <- NA
    andeler <- andeler %>% mutate(andel_tot = ifelse(Nmax<terskel, NA, andel_tot))
    rekkefolge <- order(andeler$andel_tot, decreasing = decreasing, na.last = F)

    andeler <- andeler[rekkefolge, 1:4]
    N <- N[rekkefolge, ]
    AntTilfeller <- AntTilfeller[rekkefolge, ]
    andeler[, 3:4][N[, 3:4] < terskel] <- NA
    # andeler[N[[4]]<terskel, 3:4] <- NA
    KI <- binomkonf(AntTilfeller[[4]], N[[4]])*100

    andeler$KI_low <- KI[1,]
    andeler$KI_high <- KI[2,]
    andeler[is.na(andeler[[4]]), c("KI_low", "KI_high")] <- NA
    andeler$Sykehusnavn <- factor(andeler$Sykehusnavn, levels = unique(andeler$Sykehusnavn))
    KInew <- andeler %>% select(KI_low, KI_high) %>%  as.matrix()
    ngrvar2level <- nlevels(andeler[[Grvar2]])

    # ggplot(andeler, aes(y = Sykehusnavn, x = `2021-2023`, fill = Malign)) +
    #   geom_col(position = position_dodge(width = 0.9), width = 0.9) +
    #   geom_errorbar(aes(xmin = KI_low, xmax = KI_high),
    #                 position = position_dodge(width = 0.9), width = 0.2) +
    #   scale_fill_brewer(palette = "Blues") +
    #   theme_classic() +
    #   labs(title = tittel, x = "Andel (%)", y = element_blank(), fill = NULL)+
    #   theme(axis.line.y = element_blank(),
    #         plot.title = element_text(hjust = 0.5, size = 22))



    plotdata <- andeler %>% select(1, 2, 4) %>%
      pivot_wider(id_expand = TRUE, names_from = !! sym(Grvar2 ), values_from = 3,
                  values_fill = 0, names_sort = TRUE) #%>% dplyr::arrange(across(ncol(.)))

    tmp <- plotdata[,2:dim(plotdata)[2]] %>% as.matrix() %>% t()
    colnames(tmp) <- plotdata$Sykehusnavn
    plotdata <- tmp
    # plotdata <- bind_rows(tmp2, tmp)
    # pos <- barplot(tmp, beside = TRUE, horiz = TRUE, las=1)
    if (inkl_N) {
      pst_txt <- paste0(sprintf('%.0f', andeler[[4]]), ' %', ', N = ', N[[4]])
    } else {pst_txt <- paste0(sprintf('%.0f', andeler[[4]]), ' %')}
    # pst_txt <- paste0(sprintf('%.0f', andeler[[4]]), ' %', ', N = ', N[[4]])
    pst_txt[N[, 4]<terskel] <- paste0('N<', terskel)
    pst_txt[andeler[[Grvar1]] %in% lavDG] <- lavDGtekst
    # pst_txt <- c(NA, pst_txt, NA, NA)
    pst_txt_prikk <- paste0(sprintf('%.0f', andeler[[3]]), ' %')
    pst_txt_prikk[N[[3]]<terskel] <- NA
    pst_txt_prikk[andeler[[Grvar1]] %in% lavDG] <- NA
    # pst_txt_prikk <- c(NA, pst_txt_prikk, NA, NA)


    FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height,
                                    pointsizePDF=11, fargepalett='BlaaOff')
    farger <- FigTypUt$farger %>% rev()
    soyleFarger <- rep(farger[c(3,4)], dim(andeler)[1]/2)
    soyleFarger <- farger[1:nlevels(andeler[[2]])]
    # soyleFarger[which(rownames(andeler)=='Norge')] <- farger[4]
    # if (!is.na(graaUt[1])) {soyleFarger[which(rownames(andeler) %in% graaUt)] <- 'gray88'}

    oldpar_mar <- par()$mar
    oldpar_fig <- par()$fig
    oldpar_oma <- par()$oma

    cexgr <- skriftStr
    grtxt <- colnames(plotdata)

    vmarg <- max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75)
    par('fig'=c(vmarg, 1, 0, 1))
    par('mar'=c(5.1, 4.1, 5.1, 9.1))
    par('oma'=c(0,1,NutvTxt,0))

    if (inkl_konf) {
      # par('mar'=c(5.1, 4.1, 5.1, 2.1))
      if (!pst_kolonne) {par('mar'=c(5.1, 4.1, 5.1, 2.1)) }
      xmax <- min(max(KInew, max(andeler[, 3:4], na.rm = T), na.rm = T)*1.15,100)
    } else {
      xmax <- min(100, 1.15*max(andeler[, 3:4], na.rm = T))
    }

    plotdata<- cbind(matrix(rep(NA, nlevels(andeler[[2]])),
                            nrow = nlevels(andeler[[2]]), ncol = lbunn),
                     plotdata, matrix(rep(NA, nlevels(andeler[[2]])), nrow = nlevels(andeler[[2]]), ncol = ltop))

    grtxt <- colnames(plotdata)
    # KInew <- rbind(rep(NA, nlevels(andeler[[2]])), rep(NA, nlevels(andeler[[2]])), KInew,
    #                rep(NA, nlevels(andeler[[2]])))
    KInew <- rbind(matrix(c(NA,NA), nrow=nlevels(andeler[[2]])*lbunn, ncol = 2), KInew,
                   matrix(c(NA,NA), nrow=nlevels(andeler[[2]])*ltop, ncol = 2))

    ypos <- barplot( plotdata, beside=T, las=1,
                     xlim=c(0,xmax),
                     names.arg=rep('',dim(plotdata)[2]),
                     horiz=T, axes=F, space=c(0,0.3),
                     col=soyleFarger, border=NA, xlab = 'Andel (%)') # '#96BBE7'

    fargerMaalNiva <-  c('aquamarine3','#fbf850', 'red')
    ymax <- max(ypos[, lbunn+dim(andeler)[1]/ngrvar2level])+1

    if (maal > minstekrav & !is.na(maal) & !is.na(minstekrav)) {
      rect(xleft=minstekrav, ybottom=1, xright=maal, ytop=ymax, col = fargerMaalNiva[2], border = NA)
      rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=ymax, col = fargerMaalNiva[1], border = NA)}
    if (maal < minstekrav & !is.na(maal) & !is.na(minstekrav)) {
      rect(xleft=maal, ybottom=1, xright=minstekrav, ytop=ymax, col = fargerMaalNiva[2], border = NA)
      rect(xleft=0, ybottom=1, xright=maal, ytop=ymax, col = fargerMaalNiva[1], border = NA)}
    if (!is.na(maal) & is.na(minstekrav) & maalretn=='lav') {
      # rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+0.4, col = fargerMaalNiva[2], border = NA)
      rect(xleft=0, ybottom=1, xright=maal, ytop=ymax, col = fargerMaalNiva[1], border = NA)}
    if (!is.na(maal) & is.na(minstekrav) & maalretn=='hoy') {
      # rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+0.4, col = fargerMaalNiva[2], border = NA)
      rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=ymax, col = fargerMaalNiva[1], border = NA)}

    title(main = tittel)
    ypos <- as.numeric(ypos) #as.vector(ypos)
    yposOver <- ymax + 0.25*diff(ypos)[1]
    if (!is.na(minstekrav)) {
      lines(x=rep(minstekrav, 2), y=c(1, yposOver), col=fargerMaalNiva[2], lwd=2)
      par(xpd=TRUE)
      text(x=minstekrav, y=yposOver, labels = minstekravTxt,
           pos = 4, cex=cexgr*0.6, srt = rotermaaltxt)
      par(xpd=FALSE)
    }
    if (!is.na(maal)) {
      lines(x=rep(maal, 2), y=c(1, yposOver), col=fargerMaalNiva[1], lwd=2)
      barplot( plotdata, beside=T, las=1,
               xlim=c(0,xmax),
               names.arg=rep('',dim(plotdata)[2]),
               horiz=T, axes=F, space=c(0,0.3),
               col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
      par(xpd=TRUE)
      text(x=maal, y=yposOver, labels = maalTxt, pos = 4, cex=cexgr*0.6, srt = rotermaaltxt) #paste0(maalTxt,maal,'%')
      par(xpd=FALSE)
    }
    if (inkl_konf){
      arrows(x0 = KInew[,1], y0 = ypos, x1 = KInew[,2], y1 = ypos,
             length=0.5/max(ypos), code=3, angle=90, lwd=1.8, col='gray')
      legend('bottom', cex=0.9*cexgr, bty='n',
             lwd=1.8, lty = 1, pt.cex=1.8, col='gray',
             legend=paste0('Konfidensintervall ', names(N)[dim(N)[2]]))
    }

    axis(1,cex.axis=0.9)
    grtxt_bold <- grtxt
    grtxt_bold[which(substr(grtxt_bold, 1, 5) =='Norge')] <-
      paste0("$\\textbf{", grtxt_bold[which(substr(grtxt_bold, 1, 5) =='Norge')], "}")
    ypos_middel <-
      matrix(ypos,
             ncol = dim(plotdata)[2], nrow = ngrvar2level) %>%
      colMeans()
    mtext(latex2exp::TeX(grtxt_bold), side=2, line=1.7, las=1, at=ypos_middel, col=1, cex=cexgr)

    tmpN <- data.frame(ypos = ypos,
                       tekst = c(rep(NA,lbunn*ngrvar2level),
                                 pst_txt, rep(NA,ltop*ngrvar2level))) %>%
      filter(!is.na(tekst)) %>%
      bind_cols(N[,4])
    mtext(tmpN[[3]], side=2, line=0.4, las=1, at=tmpN[[1]], col=1, cex=cexgr*0.7)
    mtext(paste0("N, ", names(N)[4]), side=2, line=0.4, las=1,
          at=tail(tmpN[[1]], 1)+1, col=1, cex=cexgr*0.7)

    prikker <- c(1, 19, 2,3,4)
    par(xpd=TRUE)
    points(y=ypos, x=c(rep(NA,lbunn*ngrvar2level), andeler[[3]],
                       rep(NA,ltop*ngrvar2level)),
           cex=pktStr, pch= prikker[1:ngrvar2level])
    par(xpd=FALSE)

    legend(legPlass, inset = c(inset, 0), cex=0.7*cexgr, bty='n',
           lwd=rep(NA, 2*ngrvar2level),
           pch=c(prikker[1:ngrvar2level],rep(15, ngrvar2level)),
           pt.cex=c(rep(1.2, ngrvar2level), rep(1.8, ngrvar2level)),
           col=c(rep('black', ngrvar2level),soyleFarger),
           legend=paste(rep(names(andeler)[3:4], each=ngrvar2level),
                        levels(andeler[[Grvar2]]), sep=', '), ncol = 2, xpd=TRUE)

    aux <- data.frame(ypos = ypos,
                      tekst = c(rep(NA,lbunn*ngrvar2level),
                                pst_txt, rep(NA,ltop*ngrvar2level))) %>%
      filter(!is.na(tekst),
             tekst != dg_tekst)
    aux2 <- data.frame(ypos = (ypos + c(tail(ypos,-1), head(ypos, 1)))/2,
                       tekst = c(rep(NA,lbunn*ngrvar2level),
                                 pst_txt, rep(NA,ltop*ngrvar2level))) %>%
      filter(!is.na(tekst)) %>%
      bind_cols(andeler[,"Sykehusnavn"]) %>%
      filter(ypos == min(ypos), .by = c(tekst, Sykehusnavn)) %>%
      filter(tekst == dg_tekst) %>% select(ypos, tekst) %>%
      bind_rows(aux)

    if (pst_kolonne) {
      mtext( aux2$tekst, side=4, line=7.5, las=1, at=aux2$ypos, col=1, cex=cexgr*0.75, adj = 1)
      mtext( c(rep(NA,lbunn*ngrvar2level),
               pst_txt_prikk, rep(NA,ltop*ngrvar2level)), side=4, line=3.5, las=1,
             at=ypos, col=1, cex=cexgr*0.75, adj = 1)
      mtext( names(andeler)[3], side=4, line=3.5, las=1, at=max(ypos), col=1, cex=cexgr*0.75, adj = 1, font = 2)
      mtext( names(andeler)[4], side=4, line=7.5, las=1, at=max(ypos), col=1, cex=cexgr*0.75, adj = 1, font = 2)
    } else {
      text(x=0, y=aux2$ypos, labels = aux2$tekst, cex=0.75, pos=4)
      if (prikktall) {text(x=c(rep(NA,lbunn*ngrvar2level), andeler[[3]],
                               rep(NA,ltop*ngrvar2level)), y=ypos,
                           labels = c(rep(NA,lbunn*ngrvar2level),
                                      pst_txt_prikk, rep(NA,ltop*ngrvar2level)),
                           cex=0.75, pos=4, xpd = T)}
    }

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[4], line=(NutvTxt-1):0, outer=TRUE)

    par('mar'= oldpar_mar)
    par('fig'= oldpar_fig)
    par('oma'= oldpar_oma)
    # if (outfile != '') {dev.off()}

    # if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}
    if ( outfile != '') {dev.off()}

    # Utdata$utvalgTxt <- utvalgTxt
    # Utdata$tittel <- tittel
    # return(invisible(Utdata))
  }


