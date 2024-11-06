library(norgast)
library(tidyverse)
rm(list = ls())

RegData <- read.table('I:/norgast/allevarnum2020-03-11 14-52-26.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- read.table('I:/norgast/forlopsoversikt2020-03-11 14-52-26.txt', header=TRUE, sep=";", encoding = 'UTF-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
# RegData <- RegData[RegData$Aar <= 2018, ]

RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
RegDataOblig <- RegData[RegData$Op_gr %in% 1:7, ]

enhetsliste <- RegDataOblig[match(unique(RegDataOblig$AvdRESH), RegDataOblig$AvdRESH), c("AvdRESH", "Sykehusnavn")]
write.csv2(enhetsliste, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/enhetsliste.csv', row.names = F)


minald=0
maxald=130
erMann <- 99
datoFra <- '2014-01-01'
datoTil <- '2018-12-31'
BMI=''
minPRS=0
maxPRS=2.2
ASA=''
whoEcog= ''
ncsp=''
forbehandling=''
valgtShus=c('')
op_gruppe <- ''
malign <- 99

elektiv=1
tilgang = c('1', '3')
valgtVar <- 'Saarruptur'

NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel")]
indikator$Nevner <- 1
names(indikator)[1:3] <- c('Resh', 'Aar', 'Teller')

write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind1_sårruptur_NorGast.csv', row.names = F)

elektiv=99
tilgang = ''
op_gruppe <- 3
valgtVar <- 'mortalitet90'

NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel")]
indikator$Nevner <- 1
names(indikator)[1:3] <- c('Resh', 'Aar', 'Teller')

write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind2_andelavdøde_spiserør_NorGast.csv', row.names = F)


op_gruppe <- 4
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel")]
indikator$Nevner <- 1
names(indikator)[1:3] <- c('Resh', 'Aar', 'Teller')

write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind3_andelavdøde_magesekk_NorGast.csv', row.names = F)


op_gruppe <- 6
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel")]
indikator$Nevner <- 1
names(indikator)[1:3] <- c('Resh', 'Aar', 'Teller')

write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind4_andelavdøde_bykspytt_tolv_NorGast.csv', row.names = F)


op_gruppe <- 5
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel")]
indikator$Nevner <- 1
names(indikator)[1:3] <- c('Resh', 'Aar', 'Teller')

write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind5_andelavdøde_lever_NorGast.csv', row.names = F)


op_gruppe <- 1
valgtVar <- 'Anastomoselekkasje'
whoEcog= c('0', '1')
malign <- 1
elektiv=1

NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel")]
indikator$Nevner <- 1
names(indikator)[1:3] <- c('Resh', 'Aar', 'Teller')

write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind6_lekkasje_tykktarm_NorGast.csv', row.names = F)


op_gruppe <- 2
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel")]
indikator$Nevner <- 1
names(indikator)[1:3] <- c('Resh', 'Aar', 'Teller')

write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind7_lekkasje_endetarm_NorGast.csv', row.names = F)


valgtVar <- 'LapTilgang2'
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel")]
indikator$Nevner <- 1
names(indikator)[1:3] <- c('Resh', 'Aar', 'Teller')

write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind10_kikkhullsteknikk_endetarm_NorGast.csv', row.names = F)


op_gruppe <- 1
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel")]
indikator$Nevner <- 1
names(indikator)[1:3] <- c('Resh', 'Aar', 'Teller')

write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind9_kikkhullsteknikk_tykktarm_NorGast.csv', row.names = F)


op_gruppe <- 5
whoEcog= ''
malign <- 99
elektiv=99
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel")]
indikator$Nevner <- 1
names(indikator)[1:3] <- c('Resh', 'Aar', 'Teller')

write.csv2(indikator, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind8_kikkhullsteknikk_ lever_NorGast.csv',
           row.names = F)


############## Nøkkeltall  ###########################

nokkeltall <- RegDataOblig %>% group_by(Aar) %>% summarise("Antall operasjoner" = n(),
                                             "Antall avdelinger" = length(unique(AvdRESH)),
                                             "Gjennomsnittsalder" = mean(Alder),
                                             "Medianalder" = median(Alder),
                                             "Andel sårruptur" = sum(Saarruptur==1 & LapTilgang == 0 & Hastegrad_tid == 1, na.rm = T)/
                                               sum(LapTilgang == 0 & Hastegrad_tid == 1, na.rm = T)*100,
                                             "Andel reopererte" = sum(ReLapNarkose & Hastegrad_tid == 1)/sum(Hastegrad_tid)*100,
                                             "Overlevelse 90 dager" = sum((OpDoedTid>=90 | is.na(OpDoedTid)) & Hastegrad_tid == 1)/sum(Hastegrad_tid)*100)
nokkeltall$Dekningsgrad <- c(13.5, 21.3, 46.6, 65, 67.7, NA, NA)

write.csv2(nokkeltall, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/nokkeltall.csv', row.names = F)




