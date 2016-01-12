

NorgastTabeller <- function(RegData=RegData, datoFra='2014-01-01', datoTil='2050-12-31',
                            minald=0, maxald=130, erMann=99, enhetsUtvalg=0, Terskel=15,
                            libkat=libkat, reshID=reshID, skrivCSV = F)
{
  #Inngangsdata:
  #	  RegData - ei dataramme med alle variabler fra registeret
  #	  libkat - sti til bibliotekkatalog
  #	  reshID - avdelingsid for egen avdeling, standard: 0-hele landet

# Trenger funksjonen NorgastLibUtvalg.R
# source(paste(libkat, 'NorgastLibUtvalg.R', sep=''), encoding="UTF-8")

if (enhetsUtvalg==2){RegData <- RegData[which(RegData$AvdRESH==reshID),]}

NorgastUtvalg <- NorgastLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                                  erMann=erMann, op_gruppe=0)
RegData <- NorgastUtvalg$RegData

####################     Tabell over alle operasjoner (noen gruppert)   ###########################################
RegData$NCSP <- paste0(substr(RegData$NCSP, 7, 100), ' (', substr(RegData$NCSP, 1, 5), ')')

RegData$NCSP[which(RegData$Operasjonsgrupper == 'Kolonreseksjoner')] <- 'Kolonreseksjoner'
RegData$NCSP[which(RegData$Operasjonsgrupper == "Rektumreseksjoner")] <- 'Rektumreseksjoner'
RegData$NCSP[which(RegData$Operasjonsgrupper == "Øsofagusreseksjoner")] <- 'Øsofagusreseksjoner'
RegData$NCSP[which(RegData$Operasjonsgrupper == "Ventrikkelreseksjoner")] <- 'Ventrikkelreseksjoner'
RegData$NCSP[which(RegData$Operasjonsgrupper == "Leverreseksjoner")] <- 'Leverreseksjoner'
RegData$NCSP[which(RegData$Operasjonsgrupper == "Whipples operasjon")] <- 'Whipples operasjon'

RegData$NCSP <- gsub("[\r\n]", "", RegData$NCSP)
res <- sort(table(RegData$NCSP), decreasing=T)
Tabell <- data.frame('Operasjonsgruppe'=names(res[res>=Terskel]), 'Antall'=as.numeric(res[res>=Terskel]))

while(length(Tabell$Antall)>14){
  Terskel <- Terskel+1
  Tabell <- Tabell[Tabell$Antall>=Terskel, ]
}

Tabell <- data.frame('Operasjonsgruppe'=c(names(res[res>=Terskel]), 'Andre'),
                     'Antall'=c(as.numeric(res[res>=Terskel]), sum(as.numeric(res[res<Terskel]))))
Tabell$Andel <- Tabell$Antall/sum(Tabell$Antall)*100

grtxt <- c('Nei','Ja')

RegData <- RegData[which(RegData$RELAPAROTOMY %in% c(0, 1)), ]
RegData$VariabelGr <- factor(RegData$RELAPAROTOMY, levels=c(0, 1), labels = grtxt)

Tabell2 <- data.frame(Operasjonsgruppe=c('Kolonreseksjoner', "Rektumreseksjoner","Øsofagusreseksjoner",
                                        "Ventrikkelreseksjoner","Leverreseksjoner",'Whipples operasjon', 'Annet'),
                     N=numeric(7), Reoperasjonsrate=numeric(7), Anastomoselekkasje=numeric(7),DypInfUtenLekkasje=numeric(7),
                     Bloedning=numeric(7),Saarruptur=numeric(7),Annet=numeric(7))

RegData$RELAPAROTOMY_YES[which(RegData$RELAPAROTOMY_YES==6)]<-5    # Nytt alternativ "Ingen funn, kun diagnostisk" må tas høyde for.

grtxt <- c('Anastomoselekkasje', 'DypInfUtenLekkasje', 'Bloedning', 'Saarruptur', 'Annet')

for (p in  1:7){
  Subset <- RegData[RegData$Op_gr==p, ]
  if (p==7) Subset <- RegData[RegData$Op_gr==9, ]
  Tabell2$Reoperasjonsrate[p] <- round(table(Subset$VariabelGr)/length(Subset$VariabelGr)*100,2)[2]
  Tabell2$N[p] <- dim(Subset)[1]
  Subset <- Subset[Subset$RELAPAROTOMY==1,]
  Subset$VariabelGr <- factor(Subset$RELAPAROTOMY_YES, levels=1:5, labels = grtxt)
#   Tabell2[p,grtxt]<- round(table(Subset$VariabelGr)/length(Subset$VariabelGr)*100,2)
  Tabell2[p,grtxt]<- round(table(Subset$VariabelGr)/Tabell2$N[p]*100,2)
}

Data <- list(Tabell=Tabell, Tabell2=Tabell2, Terskel=Terskel)

return(invisible(Data))
}
