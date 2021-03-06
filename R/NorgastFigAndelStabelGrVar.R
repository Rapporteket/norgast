#' Stablet andel av valgt variabel pr grupperingsvariabel
#'
#' Denne funksjonen andeler for valgt variabel og plotter de stablet per grupperingsvariabel (vanligvis sykehus).
#'
#' Her kan detaljer skrives
#'
#' @inheritParams FigAndeler
#'
#' @return En figur med gjennomsnitt av valgt variabel pr grupperingsvariabel
#'
#' @export
#'

NorgastFigAndelStabelGrVar <- function(RegData=0, valgtVar='ModGlasgowScore', datoFra='2014-01-01', datoTil='2050-12-31',
                                       minald=0, maxald=130, erMann=99, outfile='', hastegrad_hybrid=99,
                                       preprosess=F, malign=99, Ngrense=30, lavDG='',
                                       lavDGtekst='Dekningsgrad < 60 %', hastegrad = 99,
                                       elektiv=99, BMI='', tilgang='', valgtShus=c(''), minPRS=0, modGlasgow='',
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

  if (valgtVar == 'AccordionGrad_drenasje') {
    RegData$AccordionGrad_drenasje <- RegData$AccordionGrad
    RegData$AccordionGrad_drenasje[which(RegData$AccordionGrad_drenasje==3 & RegData$KunDrenasje ==1)] <- 2
  }

  RegData$Variabel <- RegData[, valgtVar]
  RegData <- RegData[!is.na(RegData$Variabel), ]
  # if (valgtVar == 'ThoraxTilgang') {RegData <- RegData[RegData$ThoraxTilgang %in% 4:6, ]}  ## MIDLERTIDIG, MÅ FINNE LABEL FOR NY KATEGORI!!!!!!!
  RegData$Variabel <- as.factor(RegData$Variabel)

  if (valgtVar == 'Tilgang') {RegData <- RegData[which(RegData$Tilgang %in% 1:3), ]}

  NorgastUtvalg <- NorgastUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                                 maxald=maxald, erMann=erMann, elektiv=elektiv, hastegrad = hastegrad, hastegrad_hybrid=hastegrad_hybrid,
                                 BMI=BMI, valgtShus=valgtShus, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS, modGlasgow=modGlasgow,
                                 ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign, op_gruppe=op_gruppe, ncsp=ncsp)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt

  RegData[ ,grVar] <- as.factor(as.character(RegData[ ,grVar]))
  N <- dim(RegData)[1]
  if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}

  if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
    FigTypUt <- rapFigurer::figtype(outfile, fargepalett = 'BlaaOffAlle')
    farger <- FigTypUt$farger
    plot.new()
    if (dim(RegData)[1]>0) {
      tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
    } else {tekst <- 'Ingen registrerte data for dette utvalget'}
    title(main='For få registreringer', cex=0.95)	#line=-8,
    text(0.5, 0.6, tekst, cex=1.2)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    if ( outfile != '') {dev.off()}
  } else {
    tittel <- switch (valgtVar,
                      'ModGlasgowScore' = 'Modified Glasgow score',
                      'AccordionGrad' = 'Komplikasjoner (Accordion score)',
                      'AccordionGrad_drenasje' = 'Komplikasjoner (Accordion score)',
                      'Tilgang' = 'Tilgang i abdomen',
                      'ThoraxTilgang' = 'Tilgang i thorax',
                      'AvstandAnalVerge_kat' = 'Avstand tumors nedre margin til analkanten '
    )
    legendTxt <- switch (valgtVar,
                         'ModGlasgowScore' = c('0','1', '2'),
                         'AccordionGrad' = c('3','4', '5', '6'),
                         'AccordionGrad_drenasje' = c('3 (kun drenasje av \n pleuravæske/ascites)', '3 (resten)','4', '5', '6'),
                         'Tilgang' = c('Åpen', 'Laparoskopi', 'Konvertert'),
                         'ThoraxTilgang' = c('Thoracotomi', 'Thorakoskopi', 'Ingen (transhiatal)', 'Konvertert til åpen'),
                         'AvstandAnalVerge_kat' = levels(RegData$AvstandAnalVerge_kat)
    )
    legendTitle <- switch (valgtVar,
                           'ModGlasgowScore' = NULL,
                           'AccordionGrad' = 'Accordiongrad',
                           'AccordionGrad_drenasje' = 'Accordiongrad',
                           'Tilgang' = NULL,
                           'ThoraxTilgang' = NULL,
                           'AvstandAnalVerge_kat' = NULL
    )


    # N_kat <- length(unique(RegData[,valgtVar]))
    # N_kat <- length(legendTxt)
    N_kat <- nlevels(RegData$Variabel)
    AndelerGr <- ftable(RegData[ ,c(grVar, 'Variabel')])/rep(Ngr, N_kat)*100
    utdata_antall <- RegData[ ,c(grVar, 'Variabel')] %>% table() %>%
      addmargins(2) %>% as_tibble() %>% spread(key = Variabel, value = n)
    if (!(valgtVar %in% c('AccordionGrad', 'AccordionGrad_drenasje'))) {
      names(utdata_antall)[2:(N_kat+1)] <- legendTxt} else {names(utdata_antall)[2] <- '<3'}
    # names(utdata_antall)[2:(N_kat+1)] <- legendTxt
    utdata_andel <- utdata_antall
    utdata_andel <- utdata_andel %>% mutate_at(2:(N_kat+1), funs(. / Sum * 100))
    AndelerGr[which(Ngr<Ngrense),] <- NA
    AndelerGr[unlist(attr(AndelerGr, "row.vars")) %in% lavDG,] <- NA

    dataAlle <- table(RegData$Variabel)/N*100

    if (valgtVar %in% c('AccordionGrad', 'AccordionGrad_drenasje')) {
      AndelerGr <- AndelerGr[, -1]
      N_kat <- N_kat -1
      dataAlle <- table(RegData$Variabel)[-1]/N*100
    }

    AndelerGr <- rbind(AndelerGr, as.numeric(dataAlle))
    Ngr <- c(Ngr, Norge=sum(Ngr))
    grtxt <- paste0(names(Ngr), ' (', Ngr, ')')
    Ngrtxt <- rep(NA, length(Ngr))    #paste0('N=', Ngr)
    Ngrtxt[Ngr<Ngrense] <- paste0('N<', Ngrense)
    Ngrtxt[names(Ngr) %in% lavDG] <- lavDGtekst

    if (N_kat==3 & valgtVar != 'AvstandAnalVerge_kat'){
      sortInd <- order(AndelerGr[,2], decreasing = F, na.last = F)
    } else {
      sortInd <- order(AndelerGr[,1], decreasing = F, na.last = F)
    }

    if (valgtVar %in% c('AccordionGrad', 'AccordionGrad_drenasje')) {
      sortInd <- order(rowSums(AndelerGr), decreasing = F, na.last = F)
    }

    AndelerGr <- AndelerGr[sortInd, ]
    AndelerGr <- rbind(AndelerGr, rep(NA, N_kat))
    grtxt <- c(grtxt[sortInd], '(N)')
    Ngrtxt <- c(Ngrtxt[sortInd], NA)

    xmax <- max(rowSums(AndelerGr), na.rm = T)
    ymax <- length(grtxt)*1.3

    FigTypUt <- rapFigurer::figtype(outfile, height=3*800, fargepalett=NorgastUtvalg$fargepalett)	#res=96,
    farger <- FigTypUt$farger
    if (length(legendTxt)==5) {farger <- c('#4D4D4D' ,farger)}

    landet <- AndelerGr
    landet[-which(substr(grtxt, 1, 5) =='Norge'), ] <- NA
    AndelerGr[which(substr(grtxt, 1, 5) =='Norge'), ] <- NA
    ## Function for desaturating colors by specified proportion
    desat <- function(cols, sat=0.5) {
      X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
      hsv(X[1,], X[2,], X[3,])
    }
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(grtxt, units='figure', cex=0.9))
    #NB: strwidth oppfører seg ulikt avh. av device...
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    pos <- barplot(t(AndelerGr), horiz=T, beside=FALSE, border=NA, col=farger[1:N_kat], main='', font.main=1,
                   xlab='', xlim=c(0, min(1.1*xmax, 100)), las=1, ylim=c(0, ymax))#, cex.names=xkr ) #ylim=c(ymin, 1.05*ymax+2),
    barplot(t(landet), horiz=T, beside=FALSE, border=NA, col=desat(farger[1:N_kat], 0.5), main='', font.main=1,
            xlab='', xlim=c(0, min(1.1*xmax, 100)), las=1, ylim=c(0, ymax), add=TRUE)
      legend('top', legendTxt, ncol=2, fill=farger[1:N_kat], border=farger[1:N_kat],
             bty='n', cex=0.7, xpd = T, title = legendTitle)

    mtext(at=pos, grtxt, side=2, las=1, cex=1, adj=1, line=0.25)	#Sykehusnavn
    text(x=0.005*xmax, y=pos, Ngrtxt, las=1, cex=0.8, adj=0, lwd=3)	#, col=farger[4]	c(Ngrtxt[sortInd],''),
    x_pos_landet <- cumsum(c(0, landet[which(substr(grtxt, 1, 5) =='Norge'), ])[1:N_kat]) +
      landet[which(substr(grtxt, 1, 5) =='Norge'), ]/2
    text(x=x_pos_landet, y=pos[which(substr(grtxt, 1, 5) =='Norge')],
         paste0(round(landet[which(substr(grtxt, 1, 5) =='Norge'), ]), '%'), las=1, cex=0.8, adj=0.5, lwd=3)

    mtext('Prosent (%)', las=1, side=1, line=2)
    title(tittel, line=1.5, font.main=1, cex.main=1.5)
    # mtext('(Tall på søylene angir antall registreringer)', las=1, side=1, line=3)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

    return(invisible(list(andeler = utdata_andel, antall = utdata_antall, utvalgTxt=utvalgTxt, tittel=tittel)))
  }
}

