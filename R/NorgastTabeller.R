#' Lag en tabell over de vanligste operasjonstypene samt en tabell med de komplikasjonsrater for de ulike
#' komplikasjonstypene for de obligatoriske operasjonsgruppene.
#'
#' @param Terskel - Minste antall registreringer av operasjonstype for at den skal telles med i lista
#' over operasjonstyper
#' @inheritParams FigAndeler
#'
#' @return Tabell En tabell med de vanligste operasjonstypene
#'         Tabell2 En tabell med komplikasjonsrater for de ulike komplikasjonstypene
#'         Terskel Minste antall
#'
#' @export
NorgastTabeller <- function(
    RegData=RegData, datoFra='2014-01-01', datoTil='2050-12-31',
    minald=0, maxald=130, erMann=99, enhetsUtvalg=0, Terskel=15,
    reshID=reshID, elektiv=99, BMI='', valgtShus='')
{

  if (enhetsUtvalg==2){RegData <- RegData[which(RegData$AvdRESH==reshID),]}

  NorgastUtvalg <- NorgastUtvalg(
    RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
    maxald=maxald, erMann=erMann, elektiv=elektiv, BMI=BMI, valgtShus=valgtShus)
  RegData <- NorgastUtvalg$RegData

  ###### Lag tabell over alle operasjoner (noen gruppert)  ##################
  RegData <- RegData %>%
    dplyr::mutate(Hovedoperasjon = paste0(substr(Hovedoperasjon, 7, 100),
                                          ' (', substr(Hovedoperasjon, 1, 5), ')'),
                  Hovedoperasjon = ifelse(Op_gr == 99, Hovedoperasjon, Operasjonsgrupper),
                  Hovedoperasjon = gsub("[\r\n]", "", Hovedoperasjon))

  Tabell <- RegData %>%
    dplyr::count(Hovedoperasjon) %>%
    dplyr::arrange(-n)

  if (dim(Tabell)[1] > 14) {
    Tabell$Hovedoperasjon[15] <- "Andre"
    Tabell$n[15] <- sum(Tabell$n[15:dim(Tabell)[1]])
    Tabell <- Tabell[1:15, ] %>%
      dplyr::mutate(Andel = n/sum(n)*100) %>%
      dplyr::rename(Operasjonsgruppe = Hovedoperasjon,
                    Antall = n)
  }

  ###  Lag tabell over reoperasjonsrater sammen med årsak til reoperasjon
  ###  splittet på operasjonsgrupper                               ######

  Tabell2 <- RegData %>%
    dplyr::filter(ReLapNarkose %in% c(0, 1)) %>%
    dplyr::summarise(
      N = dplyr::n(),
      Reoperasjonsrate = sum(ReLapNarkose)/N*100,
      Anastomoselekkasje = sum(ViktigsteFunn == 1, na.rm = TRUE)/N*100,
      # Anastomoselekkasje_v2 = sum(ViktigsteFunn == 1 & NyAnastomose==1, na.rm = TRUE)/
      #   sum(NyAnastomose==1, na.rm = TRUE)*100,
      DypInfUtenLekkasje = sum(ViktigsteFunn == 2, na.rm = TRUE)/N*100,
      Bloedning = sum(ViktigsteFunn == 3, na.rm = TRUE)/N*100,
      Saarruptur = sum(ViktigsteFunn == 4, na.rm = TRUE)/N*100,
      Annet = sum(ViktigsteFunn == 5, na.rm = TRUE)/N*100,
      Ingen = sum(ViktigsteFunn == 6, na.rm = TRUE)/N*100,
      .by = Operasjonsgrupper
    ) %>%
    dplyr::mutate(
      Operasjonsgrupper =
        factor(Operasjonsgrupper,
               levels = RegData$Operasjonsgrupper[match(sort(unique(RegData$Op_gr)),
                                                        RegData$Op_gr)])
    ) %>%
    dplyr::arrange(Operasjonsgrupper)

  Data <- list(Tabell=Tabell, Tabell2=Tabell2, Terskel=Terskel)

  return(invisible(Data))
}

