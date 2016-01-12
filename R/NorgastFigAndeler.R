#' Lag søylediagram eller stabelplott som viser andeler av ulike variabler
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
#' @param andel Andel
#'                 0: andeler
#'                 1: antall
#' @param preprosess Preprosesser data
#' @param hentData Gjør spørring mot database
#'                 0: Nei, RegData gis som input til funksjonen (Default)
#'                 1: Ja
#'
#' @return En figur med søylediagram eller et stabelplot av ønsket variabel
#'
#' @export


FigAndeler  <- function(RegData=0, valgtVar, datoFra='2014-01-01', datoTil='2050-12-31',
                        minald=0, maxald=130, erMann=99, op_gruppe=0, outfile='',
                        reshID, enhetsUtvalg=1, stabel=F, andel=T, preprosess=T, hentData=0)
{
  # Trenger funksjonene LibFigFilType.R og NorgastLibUtvalg.R
#   source(paste(libkat, 'LibFigFilType.R', sep=''), encoding="UTF-8")
#   source(paste(libkat, 'NorgastLibUtvalg.R', sep=''), encoding="UTF-8")

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData==1){
    RegData <- NSLoadRegData(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    # source(paste(libkat, 'NorgastLibRensOgDefinerVariabler.R', sep=''), encoding="UTF-8")
    Data <- NorgastLibRensOgDefinerVariabler(RegData=RegData, reshID=reshID)
    RegData <- Data$RegData
    rm(Data)
  }

  #------------Gjøre utvalg-------------------------

  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'}
  else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
    }	#'Eget sykehus' #

  RegData$Variabel <- NA
  if (valgtVar %in% c('Alder', 'Vektendring', 'DIABETES','WHO_ECOG_SCORE', 'ASA', 'MODIFIED_GLASGOW_SCORE', 'Forbehandling',
                      'BMI_kodet', 'Op_gr', 'Hastegrad', 'ABDOMINAL_ACCESS', 'THORAX_ACCESS', 'ACCORDION_SCORE', 'RELAPAROTOMY',
                      'AvlastendeStomiRektum', 'PermanentStomiColorektal', 'RegMnd', 'ROBOTASSISTANCE', 'erMann', 'PRS_SCORE',
                      'ANASTOMOSIS','Anastomoselekkasje')) {
    RegData$Variabel <- RegData[ ,valgtVar]
  }

  #Tar ut de med manglende registrering av valgt variabel og gjør utvalg
  NorgastUtvalg <- NorgastLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                                    op_gruppe=op_gruppe)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt

  #Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}	#{indShUt <- which(RegData$AvdRESH != reshID)}

  #Hvis for få observasjoner..
  if (dim(RegData)[1] < 5 | (length(which(RegData$AvdRESH == reshID))<5 & enhetsUtvalg==1)
      | (length(which(RegData$AvdRESH != reshID))<5 & enhetsUtvalg==1)) {
    #-----------Figur---------------------------------------
    figtype(outfile)
    plot.new()
    title(main=valgtVar, line=-1)
    text(0.5, 0.7, 'Færre enn 5 registreringer ved egen avdeling',cex=1.3)
    text(0.5, 0.6, 'eller landet forøvrig for dette utvalget.',cex=1.3)
    if ( outfile != '') {dev.off()}
  } else {

    #-----------Må ha en del som er registerspesifikk, så må selve plottet være i pakken, dvs. funksjoner.
    cexgr <- 1.0
    skalerGrTxt <- 1
    retn <- 'V'
    grtxt <- ''
    grtxt2 <- ''
    subtxt <- ''
    utvalg <- c('Sh', 'Rest')
    Andeler <- list(Sh = 0, Rest =0)
    vmarg <- 0.1
    incl_pst <- FALSE
    incl_N <- FALSE

{    indSh <-which(RegData$AvdRESH == reshID)
    indRest <- which(RegData$AvdRESH != reshID)
    RegDataLand <- RegData
    ind <- list(Sh=indSh, Rest=indRest)

    for (teller in 1:2) {
      if (teller==2 & enhetsUtvalg != 1) {break}

      if (enhetsUtvalg == 1) {RegData <- RegDataLand[switch(utvalg[teller], Sh = ind$Sh, Rest=ind$Rest), ]}

      #Variablene kjøres to ganger for sammenligning med Resten.

      if (valgtVar=='Vektendring') {
        tittel <- 'Fra premorbid til preoperativ vektendring'
        RegData$Variabel <- as.numeric(RegData$Variabel)
        gr <- c(-100, -10, -5, -2, 2, 5, 10, 200)
        RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=T)
        grtxt <- c('<-10','[-10,-5)', '[-5,-2)', '[-2,2)', '[2,5)', '[5,10)','>=15')
        subtxt <- 'Vektendring %'
      }

      if (valgtVar=='Op_gr') {
        tittel <- 'Operasjonsgrupper'
        gr <- c(1:6, 9)
        grtxt <- c('Kolonreseksjoner','Rektumreseksjoner','Øsofagusreseksjoner','Ventrikkelreseksjoner',
                   'Leverreseksjoner',"Whipples operasjon",'Annet')
        RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
        subtxt <- 'Operasjonsgrupper'
        incl_N <- T
        retn <- 'H'
#         skalerGrTxt <-.9
      }

      if (valgtVar=='Alder') {
        tittel <- 'Aldersfordeling'
        gr <- c(0, seq(45, 85, 10), 120)  #c(0,16,31,46,61,76,200)
        RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
        grtxt <- c('<45','45-54','55-64','65-74','75-84','85+')
        subtxt <- 'Aldersgrupper'
      }


      if (valgtVar=='PRS_SCORE') {
        tittel <- 'mE-PASS'
        gr <- seq(0, 2, .4)
        RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
        grtxt <- names(table(RegData$VariabelGr))
        subtxt <- 'PRS-score'
      }


      if (valgtVar=='WHO_ECOG_SCORE') {
        tittel <- 'WHO-ECOG'
        grtxt <- c('0: Fullt aktiv', '1: Lett husarbeid og sittende arbeid', '2: Oppe > 50% av dagen, selvstelt',
          '3: Oppe < 50% av dagen, delvis selvstelt', '4: Kun i stol/seng, hjelp til alt stell', 'Ukjent')
        RegData <- RegData[which(RegData$Variabel %in% c(0:4,9)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0:4,9), labels = grtxt)
#         vmarg <- 0.15
#         skalerGrTxt <- .72
        retn <- 'H'
#         incl_pst <- FALSE
      }

      if (valgtVar=='BMI_kodet') {
        tittel <- 'BMI'
        subtxt <- expression(BMI (kg/m^2))
        grtxt <- c('Alvorlig undervekt (<16)','Undervekt (16-17)','Mild undervekt (17-17.5)','Normal (18.5-25)','Overvekt (25-30)',
                   'Moderat fedme, klasse I (30-35)','Fedme, klasse II (35-40)','Fedme, klasse III (40-50)')
        RegData <- RegData[which(RegData$Variabel %in% c(1:8)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:8), labels = grtxt)
#         vmarg <- 0.15
#         skalerGrTxt <-.95
        retn <- 'H'
      }

      if (valgtVar=='ASA') {
        tittel <- 'ASA-score'
        subtxt <- 'ASA-score gruppe'
        grtxt <- c('1', '2', '3', '4', '5')
        RegData <- RegData[which(RegData$Variabel %in% c(1:5)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:5), labels = grtxt)
      }

      if (valgtVar=='erMann') {
        tittel <- 'Kjønn'
#         subtxt <- 'ASA-score gruppe'
        grtxt <- c('Kvinne', 'Mann')
        RegData <- RegData[which(RegData$Variabel %in% c(0,1)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0,1), labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='MODIFIED_GLASGOW_SCORE') {
        tittel <- 'Modified Glasgow Prognostic Score'
        grtxt <- c('0', '1', '2')
        RegData <- RegData[which(RegData$Variabel %in% c(0:2)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0:2), labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='Forbehandling') {
        tittel <- 'Neoadjuvant behandling siste 3 mnd.'
        grtxt <- c('Cytostatika', 'Stråleterapi', 'Komb. kjemo/radioterapi', 'Ingen')
        RegData <- RegData[which(RegData$Variabel %in% c(1:3,9)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:3,9), labels = grtxt)
#         vmarg <- 0.15
#         skalerGrTxt <-.95
        retn <- 'H'
        incl_pst <- T
      }

      if (valgtVar=='Hastegrad') {
        tittel <- 'Elektiv kirurgi'
        grtxt <- c('Øyeblikkelig', 'Elektiv')
        RegData <- RegData[which(RegData$Variabel %in% 0:1), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=0:1, labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='ABDOMINAL_ACCESS') {
        tittel <- 'Tilgang i abdomen'
        grtxt <- c('Åpen', 'Laparoskopisk', 'Konvertert')
        RegData <- RegData[which(RegData$Variabel %in% 1:3), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=1:3, labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='THORAX_ACCESS') {
        tittel <- 'Tilgang i thorax v/ øsofaguskirurgi'
        grtxt <- c('Thoracotomi', 'Thorakoskopi', 'Ingen (transhiatal)')
        RegData <- RegData[which(RegData$Variabel %in% 4:6), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=4:6, labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='ACCORDION_SCORE') {
        tittel <- 'Komplikasjoner'
        grtxt <- c('<3', '3', '4', '5', '6')
        subtxt <- 'Accordion score'
#         RegData$Variabel <- as.character(RegData$Variabel)
#         RegData$Variabel[which(RegData$Variabel=='Mindre enn 3')] <- '0'
#         RegData$Variabel <- as.numeric(RegData$Variabel)
        RegData <- RegData[which(RegData$Variabel %in% c(1, 3:6)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1, 3:6), labels = grtxt)
      }

      if (valgtVar=='DIABETES') {
        tittel <- 'Medisinert mot diabetes'
        grtxt <- c('Nei','Ja')
        RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='Anastomoselekkasje') {
        tittel <- 'Anastomoselekkasje, ny anastomose'
        grtxt <- c('Nei','Ja')
        RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='ROBOTASSISTANCE') {
        tittel <- 'Robotassistert laparoskopi'
        grtxt <- c('Nei','Ja')
        RegData <- RegData[which(RegData$ABDOMINAL_ACCESS %in% c(2,3)), ]
        RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='RELAPAROTOMY') {
        tittel <- 'Relaparotomi'
        grtxt <- c('Nei','Ja')
        RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='ANASTOMOSIS') {
        tittel <- 'Ny anastomose'
        grtxt <- c('Nei','Ja')
        RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='AvlastendeStomiRektum') {
        tittel <- 'Avlastende stomi ved rektumreseksjon'
        grtxt <- c('Nei','Ja')
        RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='PermanentStomiColorektal') {
        tittel <- 'Permanent stomi'
        grtxt <- c('Nei','Ja')
        RegData <- RegData[which(RegData$Variabel %in% c(0, 1)), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0, 1), labels = grtxt)
        if (enhetsUtvalg==1) {stabel=T}
      }

      if (valgtVar=='RegMnd') {
        tittel <- 'Antall operasjoner per måned'
        grtxt <- c('jan','feb','mar','apr','mai','jun','jul','aug','sep','okt','nov','des')
        RegData <- RegData[which(RegData$Variabel %in% 1:12), ]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=1:12, labels = grtxt)
        andel <- F
      }

      if (andel){
        if (teller == 1) {Andeler$Sh <- round(table(RegData$VariabelGr)/length(RegData$VariabelGr)*100,2)
                          Nsh <- dim(RegData)[1]}
        if (teller == 2) {Andeler$Rest <- round(table(RegData$VariabelGr)/length(RegData$VariabelGr)*100,2)
                          Nrest <- dim(RegData)[1]}}
      else {
        if (teller == 1) {Andeler$Sh <- table(RegData$VariabelGr)
                              Nsh <- dim(RegData)[1]}
        if (teller == 2) {Andeler$Rest <- table(RegData$VariabelGr)
                              Nrest <- dim(RegData)[1]}
      }
    }

}
    #-----------Figur---------------------------------------

    #Plottspesifikke parametre:

    FigTypUt <- figtype(outfile=outfile, fargepalett=NorgastUtvalg$fargepalett, pointsizePDF=12)
    farger <- FigTypUt$farger
    NutvTxt <- length(utvalgTxt)
    grtxtpst <- rev(grtxt)
    if (incl_pst) {grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f', Andeler$Sh)), '%)', sep='')}
    if (incl_N) {grtxtpst <- paste(rev(grtxt), ' (n=', rev(sprintf('%.0f', Andeler$Sh*Nsh/100)), ')', sep='')}
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))  #Har alltid datoutvalg med
    if (grtxt2 == '') {grtxt2 <- paste(sprintf('%.1f',Andeler$Sh), '%', sep='')}


  if (stabel) {

    N <- c(Nsh, Nrest)
    Andel<- as.data.frame(cbind('Egen avdeling'=Andeler$Sh, 'Landet forøvrig'= Andeler$Rest))
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
    devtools::use_package('grid')
    devtools::use_package('gridExtra')
    pushViewport(viewport(x = 0.83, y=0.32))
    grid.draw(tableGrob(AndelTab,  gp=gpar(cex=.8*cexgr), core.just='right'))
    popViewport()

  }
  else {

    fargeSh <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	#tykkelse på linja som repr. landet

    if (retn == 'V' ) {
      #Vertikale søyler eller linje
#       par('fig'=c(0, 1, 0, 0.9))
      if (andel){
        ymax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 100)
        ylabel <- "Andel pasienter"
      }
      else {
        ymax <- max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25
        ylabel <- "Antall"
      }
      pos <- barplot(as.numeric(Andeler$Sh), beside=TRUE, las=1, ylab=ylabel,  #main=tittel,
                     sub=subtxt, cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, #names.arg=grtxt, cex.names=cexgr,
                     col=fargeSh, border='white', ylim=c(0, ymax))	#farger[c(1,3)]
      mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
      if (enhetsUtvalg == 1) {
        points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
               border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
               lwd=lwdRest, ncol=1, cex=cexgr)
      } else {
        legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''),
               border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexgr)
      }
    }


    if (retn == 'H') {
      #Horisontale søyler
      ymax <- antGr*1.4
      if (andel){
        xmax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 100)
        xlabel <- "Andel pasienter (%)"}
      else {
        xmax <- max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25
        xlabel <- "Antall"}

      pos <- barplot(rev(as.numeric(Andeler$Sh)), horiz=TRUE, beside=TRUE, las=1, xlab=xlabel, #main=tittel,
                     col=fargeSh, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
      mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

      if (enhetsUtvalg == 1) {
        points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
               border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
               lwd=lwdRest,	lty=NA, ncol=1, cex=cexgr)
      } else {
        legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''),
               border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexgr)
      }
    }
  }
  krymp <- .9
  title(tittel, line=1, font.main=1, cex.main=1.3*cexgr)
mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

  par('fig'=c(0, 1, 0, 1))

  if ( outfile != '') {dev.off()}

  }
}
