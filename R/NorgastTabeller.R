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


NorgastTabeller <- function(RegData=RegData, datoFra='2014-01-01', datoTil='2050-12-31',
                            minald=0, maxald=130, erMann=99, enhetsUtvalg=0, Terskel=15,
                            reshID=reshID, elektiv=99, BMI='', valgtShus='')
{

if (enhetsUtvalg==2){RegData <- RegData[which(RegData$AvdRESH==reshID),]}

NorgastUtvalg <- NorgastLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                                  erMann=erMann, op_gruppe=0, elektiv=elektiv, BMI=BMI, valgtShus=valgtShus)
RegData <- NorgastUtvalg$RegData

####################    Lag tabell over alle operasjoner (noen gruppert)  ###########################################
RegData$Hovedoperasjon <- paste0(substr(RegData$Hovedoperasjon, 7, 100), ' (', substr(RegData$Hovedoperasjon, 1, 5), ')')

N_opgr <- length(unique(RegData$Operasjonsgrupper))  # Antall distikte operasjonsgrupper (inkludert Ukjent)
RegData$Hovedoperasjon[RegData$Op_gr %in% 1:(N_opgr-1)] <- RegData$Operasjonsgrupper[RegData$Op_gr %in% 1:(N_opgr-1)]


RegData$Hovedoperasjon <- gsub("[\r\n]", "", RegData$Hovedoperasjon)
res <- sort(table(RegData$Hovedoperasjon), decreasing=T)
Tabell <- data.frame('Operasjonsgruppe'=names(res[res>=Terskel]), 'Antall'=as.numeric(res[res>=Terskel]))

# Tabellen skal inneholde maks 14 operasjonskoder
while(length(Tabell$Antall)>14){
  Terskel <- Terskel+1
  Tabell <- Tabell[Tabell$Antall>=Terskel, ]
}

Tabell <- data.frame('Operasjonsgruppe'=c(names(res[res>=Terskel]), 'Andre'),
                     'Antall'=c(as.numeric(res[res>=Terskel]), sum(as.numeric(res[res<Terskel]))))
Tabell$Andel <- Tabell$Antall/sum(Tabell$Antall)*100



###  Lag tabell over reoperasjonsrater sammen med årsak til reoperasjon splittet på operasjonsgrupper ######################

grtxt <- c('Nei','Ja')
RegData <- RegData[which(RegData$ReLapNarkose %in% c(0, 1)), ] # Ekskluderer de som ikke har registrert om reoperasjon er utført
RegData$VariabelGr <- factor(RegData$ReLapNarkose, levels=c(0, 1), labels = grtxt)

Tabell2 <- data.frame(Operasjonsgruppe=RegData$Operasjonsgrupper[match(c(1:(N_opgr-1),99), RegData$Op_gr)],
                     N=numeric(N_opgr), Reoperasjonsrate=numeric(N_opgr), Anastomoselekkasje=numeric(N_opgr),
                     DypInfUtenLekkasje=numeric(N_opgr),Bloedning=numeric(N_opgr),Saarruptur=numeric(N_opgr),
                     Annet=numeric(N_opgr))


RegData$ViktigsteFunn[which(RegData$ViktigsteFunn==6)]<-5    # Nytt alternativ "Ingen funn, kun diagnostisk" må tas høyde for.

grtxt <- c('Anastomoselekkasje', 'DypInfUtenLekkasje', 'Bloedning', 'Saarruptur', 'Annet')

for (p in  1:N_opgr){
  Subset <- RegData[RegData$Op_gr==p, ]
  if (p==N_opgr) Subset <- RegData[RegData$Op_gr==99, ]
  Tabell2$Reoperasjonsrate[p] <- round(table(Subset$VariabelGr)/length(Subset$VariabelGr)*100,2)[2]
  Tabell2$N[p] <- dim(Subset)[1]
  Subset <- Subset[Subset$ReLapNarkose==1,]
  Subset$VariabelGr <- factor(Subset$ViktigsteFunn, levels=1:5, labels = grtxt)
#   Tabell2[p,grtxt]<- round(table(Subset$VariabelGr)/length(Subset$VariabelGr)*100,2)
  Tabell2[p,grtxt]<- round(table(Subset$VariabelGr)/Tabell2$N[p]*100,2)
}

Data <- list(Tabell=Tabell, Tabell2=Tabell2, Terskel=Terskel)

return(invisible(Data))
}
