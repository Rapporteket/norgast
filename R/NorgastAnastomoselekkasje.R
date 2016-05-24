#' Lag tabell over anastomoselekkasjerate
#'
#' Denne funksjonen lager tre tabeller for bruk i samlerapport
#'
#' @inheritParams FigAndeler
#'
#' @return Tabell En list med tre tabeller over anastomoselekkasjerater
#'
#' @export


NorgastAnastomoselekkasje <- function(RegData=RegData, datoFra='2014-01-01', datoTil='2050-12-31',
                                      minald=0, maxald=130, erMann=99, reshID=601225, outfile='', elektiv=99,
                                      BMI='', valgtShus='')

{

RegData <- RegData[which(RegData$Op_gr2 != 9), ]
RegData$Op_gr2 <- factor(RegData$Op_gr2)

grtxt <- c('Kolonreseksjoner, ny anastomose', 'Kolonreseksjoner, øvrige', "Rektumreseksjoner, ny anastomose",
           "Rektumreseksjoner, øvrige", 'Øsofagusreseksjoner', 'Ventrikkelreseksjoner, ny anastomose',
           'Ventrikkelreseksjoner, øvrige','Whipples operasjon')

RegData$variabel <- 0
RegData$variabel[RegData$RELAPAROTOMY_YES==1] <- 1

NorgastUtvalg <- NorgastLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                                  erMann=erMann, op_gruppe=0, elektiv=elektiv, BMI=BMI, valgtShus=valgtShus)
RegData <- NorgastUtvalg$RegData
utvalgTxt <- NorgastUtvalg$utvalgTxt

indSh <-which(RegData$AvdRESH == reshID)
indRest <- which(RegData$AvdRESH != reshID)
RegDataSh <- RegData[indSh,]
RegDataRest <- RegData[indRest,]

###  Lage tabeller #####################################################################
Tabell1 <- data.frame(Operasjonsgruppe=grtxt, N_lokal=numeric(8), RateAnastomoselekkasje_lokal=numeric(8),
                     N_ovrig=numeric(8), RateAnastomoselekkasje_ovrig=numeric(8))

Tabell1$N_lokal <- tapply(RegDataSh$variabel, RegDataSh$Op_gr2, length)[1:8]
Tabell1$RateAnastomoselekkasje_lokal <- round(tapply(RegDataSh$variabel, RegDataSh$Op_gr2, sum)/
                                               tapply(RegDataSh$variabel, RegDataSh$Op_gr2, length)*100, 2)[1:8]
Tabell1$N_ovrig <- tapply(RegDataRest$variabel, RegDataRest$Op_gr2, length)[1:8]
Tabell1$RateAnastomoselekkasje_ovrig <- round(tapply(RegDataRest$variabel, RegDataRest$Op_gr2, sum)/
                                               tapply(RegDataRest$variabel, RegDataRest$Op_gr2, length)*100, 2)[1:8]

Tabell1$RateAnastomoselekkasje_lokal[2]<-NA
Tabell1$RateAnastomoselekkasje_ovrig[2]<-NA
Tabell1$RateAnastomoselekkasje_lokal[4]<-NA
Tabell1$RateAnastomoselekkasje_ovrig[4]<-NA
Tabell1$RateAnastomoselekkasje_lokal[7]<-NA
Tabell1$RateAnastomoselekkasje_ovrig[7]<-NA

### Begrenset til rektum ##################################
regdata <- RegData                      # Beholde det fulle datasettet
RegData <- RegData[RegData$Op_gr2==3,]  # Velg bare rektum
indSh <-which(RegData$AvdRESH == reshID)
indRest <- which(RegData$AvdRESH != reshID)
RegDataSh <- RegData[indSh,]
RegDataRest <- RegData[indRest,]

Tabell2 <- data.frame(Operasjonsgruppe=c('Rektumreseksjoner, ny anastomose', '\\quad Uten avlastende stomi', '\\quad Med avlastende stomi'),
                      N_lokal=numeric(3), RateAnastomoselekkasje_lokal=numeric(3),
                     N_ovrig=numeric(3), RateAnastomoselekkasje_ovrig=numeric(3))

Tabell2$N_lokal[2:3] <- tapply(RegDataSh$variabel, RegDataSh$AvlastendeStomiRektum, length)[1:2]
Tabell2$RateAnastomoselekkasje_lokal[2:3] <- round(tapply(RegDataSh$variabel, RegDataSh$AvlastendeStomiRektum, sum)/
                                               tapply(RegDataSh$variabel, RegDataSh$AvlastendeStomiRektum, length)*100, 2)[1:2]
Tabell2$N_ovrig[2:3] <- tapply(RegDataRest$variabel, RegDataRest$AvlastendeStomiRektum, length)[1:2]
Tabell2$RateAnastomoselekkasje_ovrig[2:3] <- round(tapply(RegDataRest$variabel, RegDataRest$AvlastendeStomiRektum, sum)/
                                               tapply(RegDataRest$variabel, RegDataRest$AvlastendeStomiRektum, length)*100, 2)[1:2]
Tabell2[1,2:5] <- NA


### Onkologisk forbehandling ##################################

regdata$ForbehandlingBinaer <- NA
regdata$ForbehandlingBinaer[regdata$Forbehandling %in% c(1,2,3)] <- 1
regdata$ForbehandlingBinaer[regdata$Forbehandling == 4] <- 0
RegData <- regdata

Tabell3 <- data.frame(Operasjonsgruppe=c('Rektumreseksjon, ny anastomose', '\\quad Ingen forbehandling', '\\quad Enhver forbehandling',
                                         'Ventrikkelreseksjon, ny anastomose', '\\quad Ingen forbehandling', '\\quad Enhver forbehandling',
                                         'Øsofagusreseksjon', '\\quad Ingen forbehandling', '\\quad Enhver forbehandling'),
                      N_lokal=numeric(9), RateAnastomoselekkasje_lokal=numeric(9),
                      N_ovrig=numeric(9), RateAnastomoselekkasje_ovrig=numeric(9))


RegData <- RegData[RegData$Op_gr2==3,]
indSh <-which(RegData$AvdRESH == reshID)
indRest <- which(RegData$AvdRESH != reshID)
RegDataSh <- RegData[indSh,]
RegDataRest <- RegData[indRest,]

Tabell3$N_lokal[2:3] <- tapply(RegDataSh$variabel, RegDataSh$ForbehandlingBinaer, length)[1:2]
Tabell3$RateAnastomoselekkasje_lokal[2:3] <- round(tapply(RegDataSh$variabel, RegDataSh$ForbehandlingBinaer, sum)/
                                                     tapply(RegDataSh$variabel, RegDataSh$ForbehandlingBinaer, length)*100, 2)[1:2]
Tabell3$N_ovrig[2:3] <- tapply(RegDataRest$variabel, RegDataRest$ForbehandlingBinaer, length)[1:2]
Tabell3$RateAnastomoselekkasje_ovrig[2:3] <- round(tapply(RegDataRest$variabel, RegDataRest$ForbehandlingBinaer, sum)/
                                                     tapply(RegDataRest$variabel, RegDataRest$ForbehandlingBinaer, length)*100, 2)[1:2]

RegData <- regdata
RegData <- RegData[RegData$Op_gr2==6,]
indSh <-which(RegData$AvdRESH == reshID)
indRest <- which(RegData$AvdRESH != reshID)
RegDataSh <- RegData[indSh,]
RegDataRest <- RegData[indRest,]

Tabell3$N_lokal[5:6] <- tapply(RegDataSh$variabel, RegDataSh$ForbehandlingBinaer, length)[1:2]
Tabell3$RateAnastomoselekkasje_lokal[5:6] <- round(tapply(RegDataSh$variabel, RegDataSh$ForbehandlingBinaer, sum)/
                                                     tapply(RegDataSh$variabel, RegDataSh$ForbehandlingBinaer, length)*100, 2)[1:2]
Tabell3$N_ovrig[5:6] <- tapply(RegDataRest$variabel, RegDataRest$ForbehandlingBinaer, length)[1:2]
Tabell3$RateAnastomoselekkasje_ovrig[5:6] <- round(tapply(RegDataRest$variabel, RegDataRest$ForbehandlingBinaer, sum)/
                                                     tapply(RegDataRest$variabel, RegDataRest$ForbehandlingBinaer, length)*100, 2)[1:2]

RegData <- regdata
RegData <- RegData[RegData$Op_gr2==5,]
indSh <-which(RegData$AvdRESH == reshID)
indRest <- which(RegData$AvdRESH != reshID)
RegDataSh <- RegData[indSh,]
RegDataRest <- RegData[indRest,]

Tabell3$N_lokal[8:9] <- tapply(RegDataSh$variabel, RegDataSh$ForbehandlingBinaer, length)[1:2]
Tabell3$RateAnastomoselekkasje_lokal[8:9] <- round(tapply(RegDataSh$variabel, RegDataSh$ForbehandlingBinaer, sum)/
                                                     tapply(RegDataSh$variabel, RegDataSh$ForbehandlingBinaer, length)*100, 2)[1:2]
Tabell3$N_ovrig[8:9] <- tapply(RegDataRest$variabel, RegDataRest$ForbehandlingBinaer, length)[1:2]
Tabell3$RateAnastomoselekkasje_ovrig[8:9] <- round(tapply(RegDataRest$variabel, RegDataRest$ForbehandlingBinaer, sum)/
                                                     tapply(RegDataRest$variabel, RegDataRest$ForbehandlingBinaer, length)*100, 2)[1:2]

Tabell3[c(1,4,7),2:5] <- NA

Tabell <- list(Tabell1=Tabell1, Tabell2=Tabell2, Tabell3=Tabell3)

return(invisible(Tabell))

}
