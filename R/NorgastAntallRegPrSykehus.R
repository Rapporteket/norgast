#' Antall pasienter registrert per sykehus
#'
#' Denne funksjonen lager en liste over hvor mange pasienter som er registrert per sykehus.
#'
#' @inheritParams FigAndeler
#'
#' @return Tabell En data frame over antall registreringer per sykehus, fordelt på ferdige og
#' uferdige hovedskjema og oppfølgingsskjema.
#'
#' @export


NorgastAntallRegPrSykehus  <- function(RegData=RegData, datoFra='2014-01-01', datoTil='2050-12-31')

{
RegData$OperasjonsDato <- as.POSIXlt(RegData$OPERATION_DATE, format="%Y-%m-%d")
RegData <- RegData[which(RegData$OperasjonsDato >= as.POSIXlt(datoFra) & RegData$OperasjonsDato <= as.POSIXlt(datoTil)),]

lik0 <- function(x) {
  ut <- sum(x==0, na.rm=T)
  return(invisible(ut))
}
lik1 <- function(x) {
  ut <- sum(x==1, na.rm=T)
  return(invisible(ut))
}

tmp1 <-  tapply(RegData$STATUS, RegData$Sykehusnavn, lik1)
tmp2 <-  tapply(RegData$STATUS, RegData$Sykehusnavn, lik0)
tmp3 <-  tapply(RegData$READMISSION_STATUS, RegData$Sykehusnavn, lik1)
tmp4 <-  tapply(RegData$READMISSION_STATUS, RegData$Sykehusnavn, lik0)

Tabell <- as.data.frame(cbind(tmp1,tmp3,tmp2,tmp4, tmp1+tmp2))
names(Tabell) <- c('Hovedskjema', 'Oppfolging', 'HovedKladd', 'OppfolgKladd', 'Totalt')
Tabell <- Tabell[order(-Tabell$Totalt),]

Tabell <- rbind(Tabell, data.frame(Hovedskjema=sum(Tabell$Hovedskjema),
                                     Oppfolging=sum(Tabell$Oppfolging), HovedKladd=sum(Tabell$HovedKladd),
                                     OppfolgKladd=sum(Tabell$OppfolgKladd), Totalt=sum(Tabell$Totalt)))
row.names(Tabell)[length(Tabell$Hovedskjema)] <- 'Totalt'

return(invisible(Tabell))
}

