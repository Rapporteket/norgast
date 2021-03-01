#' Tidstrend (år/kvartal/mnd) av rate/andel for en gitt variabel for to utvalg
#'
#' Årlige eller månedlige rater for valgt variabel.
#' Konfidensintervall kan inkluderes hvis ønskelig.
#'
#' Konfidensintervallet er basert på Clopper Pearsons "eksakte" metode for binominalfordelt data.
#'
#' @inheritParams FigAndeler
#' @param tidsenhet Plot figur med år eller måned som tidsenhet
#'                  'Aar' (Default)
#'                  'Kvartal'
#'                  'Halvaar'
#'                  'Mnd'
#'
#' @return En figur med tidsutvikling av rate over år for to utvalg
#'
#' @export
#'
NorgastFigAndelTid_SammenligUtvalg <- function(plotdata=0, outfile='', tidsenhet='Aar', inkl_konf=FALSE,
                                               datoFra="2014-01-01", datoTil=today(), fra0 = FALSE, inkl_tall=FALSE) {

  # outfile=''; tidsenhet='Aar'; inkl_konf=FALSE
  # datoFra="2014-01-01"; datoTil=today(); fra0 = FALSE
  # RegData$Utvalg <- 1; inkl_tall <- F
  # RegData$Utvalg <- sample(c(1,2), dim(RegData), replace = T)
  # RegData$Variabel <- sample(c(0,1), dim(RegData), replace = T)

  RegData <- req(plotdata$Samlet)
  RegData$TidsEnhet <- switch(tidsenhet,
                              Aar = factor(format(RegData$HovedDato, format='%Y'),
                                           levels = format(seq(as.Date(datoFra), as.Date(datoTil), by="year"), "%Y")),
                              Mnd = factor(format(RegData$HovedDato, format='%b-%y'),
                                           levels = format(seq(as.Date(datoFra), as.Date(datoTil), by="month"), "%b-%y")),
                              Kvartal = factor(as.character(tsibble::yearquarter(RegData$HovedDato)),
                                               levels = as.character(tsibble::yearquarter(seq(as.Date(datoFra),
                                                                                              as.Date(datoTil), by="1 quarter")))),
                              Halvaar = factor(tsibble::yearquarter(lubridate::floor_date(RegData$HovedDato, unit = "halfyear")) %>%
                                                 stringr::str_replace("Q1", "H1") %>% stringr::str_replace("Q3", "H2"),
                                               levels = tsibble::yearquarter(seq(as.Date(datoFra), as.Date(datoTil), by="2 quarter")) %>%
                                                 stringr::str_replace("Q1", "H1") %>% stringr::str_replace("Q3", "H2"))
  )

  VarTxt <- plotdata$PlotParams$VarTxt

  oppsum <- RegData %>% group_by(TidsEnhet, Utvalg) %>%
    summarise(antall = sum(Variabel),
              N = n(),
              andel = antall/N*100)

  andeler <- pivot_wider(oppsum, id_cols = TidsEnhet, names_from = Utvalg, values_from = andel)
  antall <- pivot_wider(oppsum, id_cols = TidsEnhet, names_from = Utvalg, values_from = antall)
  N <- pivot_wider(oppsum, id_cols = TidsEnhet, names_from = Utvalg, values_from = N)

  Konf <- list(Utvalg1=binomkonf(antall[["1"]], N[["1"]])*100,
               Utvalg2=if ("2" %in% names(antall)) {binomkonf(antall[["2"]], N[["2"]])*100} else NA)

  Tidtxt <- andeler$TidsEnhet
  Ant_tidpkt <- length(Tidtxt)
  xmin <- 0.9
  xmax <- Ant_tidpkt

  cexgr <- 0.9
  if (fra0) {
    ymin <- 0
    if (inkl_konf) {
      ymax <- 1.1*max(Konf$Utvalg1, Konf$Utvalg2, na.rm=TRUE)
    } else {
      ymax <- 1.1*max(andeler[,-1], na.rm=TRUE)
    }
  } else {
    if (inkl_konf) {
      ymin <- 0.9*min(Konf$Utvalg1, Konf$Utvalg2, na.rm=TRUE)
      ymax <- 1.1*max(Konf$Utvalg1, Konf$Utvalg2, na.rm=TRUE)
    } else {
      ymin <- 0.9*min(andeler[,-1], na.rm=TRUE)
      ymax <- 1.1*max(andeler[,-1], na.rm=TRUE)
    }
  }
  xskala <- 1:Ant_tidpkt
  FigTypUt <- rapFigurer::figtype(outfile=outfile)
  farger <- FigTypUt$farger
  plot(xskala, andeler[["1"]], xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='l', frame.plot=FALSE,
       ylab=c(paste0('Andel ', VarTxt),if (inkl_konf) {'inkl. 95% konfidensintervall'} else {NULL}),
       xlab=switch(tidsenhet, Aar='Operasjonsår', Mnd='Operasjonsår og -måned',
                   Kvartal='Operasjonsår og -kvartal', Halvaar='Operasjonsår og -halvår'),
       xaxt='n', col=farger[1], lwd=2,
       sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
  axis(side=1, at = xskala, labels = Tidtxt)

  if ("2" %in% names(andeler)) {
    lines(xskala, andeler[["2"]], col="orangered", lwd=2)
  }

  if (inkl_konf) {
    polygon( c(xskala, xskala[Ant_tidpkt:1]), c(Konf$Utvalg1[1,], Konf$Utvalg1[2,Ant_tidpkt:1]),
             col=adjustcolor(farger[1], alpha.f = 0.1), border=NA)
    if ("2" %in% names(andeler)) {
      polygon( c(xskala, xskala[Ant_tidpkt:1]), c(Konf$Utvalg2[1,], Konf$Utvalg2[2,Ant_tidpkt:1]),
               col=adjustcolor("orangered", alpha.f = 0.1), border=NA)
    }
  }

  if (inkl_tall) {
    text(xskala, andeler[["1"]], pos=3, N[["1"]], cex=0.9, col=farger[1])
    if ("2" %in% names(andeler)) {
      text(xskala, andeler[["2"]], pos=3, N[["2"]], cex=0.9, col="orangered")
    }
  }

  title(main=plotdata$PlotParams$tittel, font.main=1.2, line=1)
  legend('top', cex=0.9, bty='o', bg='white', box.col='white', lty = c(1,1),
         lwd=c(2,2), pch=c(NA,NA), pt.cex=c(1,1), col=c(farger[1],"orangered"),
         legend = c(paste0('Utvalg 1, N = ', sum(N[["1"]])),
                    paste0('Utvalg 2, N = ', sum(N[["2"]]))) )

  if ( outfile != '') {dev.off()}

  return(invisible(list(andeler=andeler, antall=antall, N=N, Konf=Konf)))
}

