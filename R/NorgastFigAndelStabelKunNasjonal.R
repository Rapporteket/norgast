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

NorgastFigAndelStabelKunNasjonal <- function(RegData=0, valgtVar='ModGlasgowScore', datoFra='2014-01-01', datoTil='2050-12-31',
                                             minald=0, maxald=130, erMann=99, outfile='',
                                             reshID, enhetsUtvalg=1, stabel=F, preprosess=F,
                                             elektiv=99, BMI='', tilgang=99, valgtShus=c(''), minPRS=0,
                                             maxPRS=2, ASA='', whoEcog= '', forbehandling=99, hentData=F, reseksjonsGr='', ncsp='')

{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- NorgastHentRegData(datoFra = datoFra, datoTil = datoTil)
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- NorgastPreprosess(RegData=RegData)
  }


  grVar <- 'Sykehusnavn'

  RegData$Variabel <- RegData[, valgtVar]
  RegData <- RegData[!is.na(RegData$Variabel), ]

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  NorgastUtvalg <- NorgastLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                                    maxald=maxald, erMann=erMann, elektiv=elektiv,
                                    BMI=BMI, valgtShus=valgtShus, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                                    ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, reseksjonsGr=reseksjonsGr, ncsp=ncsp)
  RegData <- NorgastUtvalg$RegData
  utvalgTxt <- NorgastUtvalg$utvalgTxt

  N <- dim(RegData)[1]
  tittel <- switch (valgtVar,
                    'ModGlasgowScore' = 'Modified Glasgow score',
                    'AccordionGrad' = 'Komplikasjoner',
                    'Tilgang' = 'Tilgang i abdomen'
  )
  legendTxt <- switch (valgtVar,
                       'ModGlasgowScore' = c('0','1', '2'),
                       'AccordionGrad' = c('3','4', '5', '6'),
                       'Tilgang' = c('Åpen', 'Laparoskopisk', 'Konvertert')
  )
  legendTitle <- switch (valgtVar,
                         'ModGlasgowScore' = NULL,
                         'AccordionGrad' = 'Accordiongrad',
                         'Tilgang' = NULL
  )

  # if (fullSoyle == 1) {
  xkr <- 1
  cexGrNavn <- 1.2
  FigTypUt <- figtype(outfile, height=1.75*800, fargepalett=NorgastUtvalg$fargepalett)	#res=96,
  farger <- FigTypUt$farger
  #Tilpasse marger for å kunne skrive utvalgsteksten
  NutvTxt <- length(utvalgTxt)
  vmarg <- max(0, strwidth('Nasjonalt', units='figure', cex=cexGrNavn)*0.7)
  #NB: strwidth oppfører seg ulikt avh. av device...
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
  N_kat <- length(unique(RegData[,valgtVar]))

  if (valgtVar == 'AccordionGrad') {
    # AndelerGr <- AndelerGr[, -1]
    N_kat <- N_kat -1
  }

  if (valgtVar == 'AccordionGrad') {
    dataAlle <- table(RegData$Variabel)[-1]/N*100
  } else {
    dataAlle <- table(RegData$Variabel)/N*100
  }
  xmax<- sum(dataAlle)

  #Legger til resultat for hele gruppa. Og legger til en tom etter for å få plass til legend
  pos <- barplot(cbind(as.numeric(dataAlle), rep(0,N_kat)), horiz=T, beside=FALSE,
                 border=NA, col=farger[1:N_kat], main='', font.main=1, xlab='',
                 ylim=c(0, 2), xlim=c(0, min(1.2*xmax, 100)),
                 las=1, cex.names=xkr )
  GrNavnSort <- c('Alle sykehus', '')
  NgrtxtSort<- c(paste0('N=', N), '')

  legend(x= 'top', legendTxt, xjust=0.5, yjust=0.5,	#inset=0.01,# max(pos)*1.01 x=50, y=ymax,
         fill=farger[1:N_kat], border=farger[1:N_kat], ncol=2, bty='n', title = legendTitle) #, ncol=3
  if (valgtVar == 'ModGlasgowScore') {mtext('(sortert på modified Glasgow scale = 1)', line=0.5, cex=1)}

  mtext(at=pos, GrNavnSort, side=2, las=1, cex=cexGrNavn*xkr, adj=1, line=0.25)	#Sykehusnavn
  Nfarge <-  farger[4]
  text(x=0.005*xmax, y=pos, NgrtxtSort, las=1, cex=xkr, adj=0, lwd=3, col=Nfarge)	#, col=farger[4]	c(Ngrtxt[sortInd],''),
  mtext('Prosent (%)', las=1, side=1, cex=xkr, line=2.2*xkr)
  title(tittel, line=1.5, font.main=1, cex.main=1.5)
  mtext('(Tall på søylene angir antall registreringer)', las=1, side=1, cex=xkr, line=3.2*xkr)

  #Tekst som angir hvilket utvalg som er gjort
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


  par('fig'=c(0, 1, 0, 1))
  #savePlot(outfile, type=filtype)
  if ( outfile != '') {dev.off()}

}

