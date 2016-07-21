#' Lag søylediagram eller stabelplott som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram eller stabelplot som viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel skal plottes
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren).
#' @param minald Alder, fra og med (Default: 0)
#' @param maxald Alder, til og med (Default: 130)
#' @param erMann kjønn
#'                 1: menn
#'                 0: kvinner
#'                 99: begge (alt annet enn 0 og 1) (Default)
#' @param op_gruppe Operasjonsgruppe
#'                 0: Alle grupper (Default)
#'                 1: Kolonreseksjoner
#'                 2: Rektumreseksjoner
#'                 3: Øsofagusreseksjoner
#'                 4: Ventrikkelreseksjoner
#'                 5: Leverreseksjoner
#'                 6: Whipple's operasjon
#'                 9: Annet
#' @param outfile Navn på fil figuren skrives til. Default: '' (Figur skrives
#'    til systemets default output device (som regel skjerm))
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Lag figur for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Default)
#'                 2: Egen enhet
#' @param stabel Stabelplot
#'                 0: Stabel
#'                 1: Søyle (Default)
#' @param preprosess Preprosesser data
#'                 FALSE: Nei (Default)
#'                 TRUE: Ja
#' @param hentData Gjør spørring mot database
#'                 FALSE: Nei, RegData gis som input til funksjonen (Default)
#'                 TRUE: Ja
#' @param elektiv Elektiv eller øyeblikkelig hjelp
#'                 0: Øyeblikkelig hjelp
#'                 1: Elektiv
#'                 99: Begge deler (Default)
#' @param BMI BMI-klasse, flervalg hvor (Default alle)
#'                1: Alvorlig undervekt
#'                2: Undervekt
#'                3: Mild undervekt
#'                4: Normal
#'                5: Overvekt
#'                6: Moderat fedme, klasse I
#'                7: Fedme, klasse II
#'                8: Fedme, klasse III
#' @param valgtShus Vektor med AvdResh over hvilke sykehus man genererer rapporten for.
#'                  Denne overstyrer reshID og er bare tilgjengelig for SC-bruker.
#' @param tilgang Tilgang i abdomen
#'                1: Åpen eller konvertert
#'                2: Lapaoskopisk
#'                99: Alle (Default)
#' @param minPRS  Minimum PRS (Default 0?)
#' @param maxPRS  Maksimum PRS (Default 2?)
#' @param ASA ASA-grad, flervalg hvor (Default alle)
#'                1: Ingen organisk, fysiologisk, biokjemisk eller psykisk forstyrrelse.
#'                Den aktuelle lidelsen er lokalisert og gir ikke generelle systemforstyrrelser.
#'                2: Moderat sykdom eller forstyrrelser som ikke forårsaker funksjonelle begrensninger.
#'                3: Alvorlig sykdom eller forstyrrelse som gir definerte funksjonelle begrensninger.
#'                4: Livstruende organisk sykdom som ikke behøver å være knyttet til den aktuelle
#'                kirurgiske lidelsen eller som ikke alltid bedres ved det planlagte kirurgiske inngrepet.
#'                5: Døende pasient som ikke forventes å overleve 24 timer uten kirurgi.
#' @param whoEcog WHO WCOG score, flervalg hvor (Default alle)
#'                0: Fullt aktiv
#'                1: Lett husarbeid og sittende arbeid
#'                2: Oppe > 50% av dagen, selvstelt
#'                3: Oppe < 50% av dagen, delvis selvstelt
#'                4: Kun i stol/seng, hjelp til alt stell
#'                9: Ukjent
#' @param forbehandling Onkologisk forbehandling
#'                1: Cytostatika
#'                2: Stråleterapi
#'                3: Komb. kjemo/radioterapi
#'                4: Ingen
#'                99: Alle
#'
#' @return En figur med søylediagram eller et stabelplot av ønsket variabel
#'
#' @export


FigAndeler  <- function(RegData=0, valgtVar='Alder', datoFra='2014-01-01', datoTil='2050-12-31',
                        minald=0, maxald=130, erMann=99, op_gruppe=0, outfile='',
                        reshID, enhetsUtvalg=1, stabel=F, preprosess=F,
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
  PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar, enhetsUtvalg=enhetsUtvalg)
  RegData <- PlotParams$RegData
  PlotParams$RegData <- NA

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NorgastUtvalg <- NorgastLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                                    maxald=maxald, erMann=erMann, op_gruppe=op_gruppe, elektiv=elektiv,
                                    BMI=BMI, valgtShus=valgtShus, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                                    ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt

  # For variabler som går på person, ikke per operasjon
  if (valgtVar %in% c('Avdod', 'OpDoedTid')) {
    RegData <- RegData[order(RegData$OperasjonsDato, decreasing = T), ]   # Sorter slik at man velger nyeste operasjon når flere
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
  }


  if (valgtShus[1]!='') {
    valgtShus <- as.numeric(valgtShus)
    if (length(valgtShus)==1) {reshID<-valgtShus[1]}
  }

  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$Sykehusnavn[match(reshID, RegData$AvdRESH)])
  }

  if (enhetsUtvalg!=0 & length(valgtShus)>1) {
    RegData$AvdRESH[RegData$AvdRESH %in% valgtShus] <- 99
    shtxt <- 'Ditt utvalg'
    reshID <- 99
  }

  #Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}	#{indHovedUt <- which(RegData$AvdRESH != reshID)}

################
#   utvalg <- c('Hoved', 'Rest')
#   Andeler <- list(Hoved = 0, Rest =0)
#
#   indHoved <-which(RegData$AvdRESH == reshID)
#   indRest <- which(RegData$AvdRESH != reshID)
#   RegDataLand <- RegData
#   ind <- list(Hoved=indHoved, Rest=indRest)
#   Nrest <- 0
#
#   for (teller in 1:2) {
#     if (teller==2 & enhetsUtvalg != 1) {break}
#
#     if (enhetsUtvalg == 1) {RegData <- RegDataLand[switch(utvalg[teller], Hoved = ind$Hoved, Rest=ind$Rest), ]}
#
#     #Variablene kjøres to ganger for sammenligning med Resten.
#
#     if (teller == 1) {Andeler$Hoved <- round(table(RegData$VariabelGr)/length(RegData$VariabelGr)*100,2)
#     NHoved <- dim(RegData)[1]}
#     if (teller == 2) {Andeler$Rest <- round(table(RegData$VariabelGr)/length(RegData$VariabelGr)*100,2)
#     Nrest <- dim(RegData)[1]}
#   }
####################
  # Initialiserer nødvendige størrelser
  Andeler <- list(Hoved = 0, Rest =0)
  ind <- list(Hoved=which(RegData$AvdRESH == reshID), Rest=which(RegData$AvdRESH != reshID))
  Nrest <- 0

  if (enhetsUtvalg==1) {
    AntHoved <- table(RegData$VariabelGr[ind$Hoved])
    NHoved <- sum(AntHoved)
    Andeler$Hoved <- 100*AntHoved/NHoved
    AntRest <- table(RegData$VariabelGr[ind$Rest])
    Nrest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
    Andeler$Rest <- 100*AntRest/Nrest
  } else {
    AntHoved <- table(RegData$VariabelGr)
    NHoved <- sum(AntHoved)
    Andeler$Hoved <- 100*AntHoved/NHoved
  }

####################

  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; grtxt2 <- PlotParams$grtxt2;
  stabel <- PlotParams$stabel; subtxt <- PlotParams$subtxt; incl_N <- PlotParams$incl_N;
  incl_pst <- PlotParams$incl_pst; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
  FigTypUt <- figtype(outfile=outfile, fargepalett=NorgastUtvalg$fargepalett, pointsizePDF=12)

  #Hvis for få observasjoner..
  if (NHoved < 5 | (Nrest<5 & enhetsUtvalg==1)) {
    #-----------Figur---------------------------------------
    NutvTxt <- length(utvalgTxt)
    par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt-1)))  #Har alltid datoutvalg med
    plot.new()
    text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)


  } else {

    #Plottspesifikke parametre:

    farger <- FigTypUt$farger
    NutvTxt <- length(utvalgTxt)
    grtxtpst <- rev(grtxt)
    if (incl_pst) {grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f', Andeler$Hoved)), '%)', sep='')}
    if (incl_N) {grtxtpst <- paste(rev(grtxt), ' (n=', rev(sprintf('%.0f', Andeler$Hoved*NHoved/100)), ')', sep='')}
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))  #Har alltid datoutvalg med
    if (grtxt2 == '') {grtxt2 <- paste(sprintf('%.1f',Andeler$Hoved), '%', sep='')}


    if (stabel) {

      N <- c(NHoved, Nrest)
      Andel<- as.data.frame(cbind('Egen avdeling'=Andeler$Hoved, 'Landet forøvrig'= Andeler$Rest))
      names(Andel)[1]<-shtxt; Andel <- as.matrix(Andel)
      tLeg <- 'Kategorier'
      if (length(grtxt)==2 ) {farger<-farger[c(2,4)]}
      if (length(grtxt)==3 ) {farger<-farger[c(1,3,4)]}

      koord <- barplot(Andel, beside=F, las=1,
                       cex.names=cexgr, col=rev(farger), ylab="Andel (%)", ylim=c(0,114), xlim = c(0,3.7), border=NA,
                       cex.axis=cexgr, cex.lab=cexgr, space=.25)
      legend(2.65, 99, rev(grtxt), bty='n', ncol=1, cex=cexgr, xjust=0, fill=farger,
             border=farger, title=tLeg)
      text(koord, 102.6, paste('N=', N, sep=''), cex=cexgr)
      AndelTab <- apply(matrix(paste(sprintf('%.1f', Andel),'%',sep=''), dim(Andel)), 2, rev)#[length(grtxt):1,]
      row.names(AndelTab) <- rev(grtxt)
      colnames(AndelTab) <- c('Egen', 'Andre')
      plotrix::addtable2plot(x = 2.65, y=15, AndelTab, cex=.8*cexgr, display.rownames=TRUE, bg=farger[2], xpad = .25)
    }
    else {

      fargeHoved <- farger[1]
      fargeRest <- farger[3]
      antGr <- length(grtxt)
      lwdRest <- 3	#tykkelse på linja som repr. landet

      if (retn == 'V' ) {
        #Vertikale søyler
        ymax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
        ylabel <- "Andel pasienter"
        pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab=ylabel,  #main=tittel,
                       sub=subtxt, cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, #names.arg=grtxt, cex.names=cexgr,
                       col=fargeHoved, border='white', ylim=c(0, ymax))	#farger[c(1,3)]
        mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
        mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
        if (enhetsUtvalg == 1) {
          points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
          legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
                 border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
                 lwd=lwdRest, ncol=1, cex=cexgr)
        } else {
          legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
                 border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexgr)
        }
      }


      if (retn == 'H') {
        #Horisontale søyler
        ymax <- antGr*1.4
        xmax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
        xlabel <- "Andel pasienter (%)"

        pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab=xlabel, #main=tittel,
                       col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
        mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

        if (enhetsUtvalg == 1) {
          points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
          legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
                 border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
                 lwd=lwdRest,	lty=NA, ncol=1, cex=cexgr)
        } else {
          legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
                 border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexgr)
        }
      }
    }
  }
  krymp <- .9
  title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
  mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

  par('fig'=c(0, 1, 0, 1))

  if ( outfile != '') {dev.off()}



}

