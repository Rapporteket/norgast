library(norgast)
library(tidyverse)
rm(list = ls())

RegData <- read.table('I:/norgast/AlleVarNum2020-08-31 09-48-25.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2020-08-31 09-49-48.txt', header=TRUE, sep=";", encoding = 'UTF-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad",
                      "Rekonstruksjon", "Rekonstruksjonstype", "EndoInterLekkasje", "EndoInterBlod", "PerkDrenasje",
                      "HoyAmylaseKons", "AvstandAnalVerge", "KunDrenasje", "TelefonKontroll", "FysiskKontroll")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID',
                            'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
# RegData <- RegData[RegData$Aar <= 2018, ]

# RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
RegDataOblig <- RegData[RegData$Op_gr %in% 1:7, ]

enhetsliste <- RegDataOblig[match(unique(RegDataOblig$AvdRESH), RegDataOblig$AvdRESH), c("AvdRESH", "Sykehusnavn")]
# write.csv2(enhetsliste, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/enhetsliste.csv', row.names = F)

# OBS, OUS er foreløpig registrert under en felles resh. Her benytter vi Rikshospitalet. AHUS mappes til AHUS NORDBYHAGEN SOMATIKK?

map_resh_orgnr <- data.frame(orgnr_sh = c(974733013, 974631407, 974557746, 974632535, 974795787, 974705788, 974633574, 974795639,
                                          974724960, 974795361, 874716782, 974631326, 974749025, 974706490, 974703300, 974633752,
                                          874632562, 974743272, 974795515, 974116804, 974747138, 974745569, 974795833, 974633191,
                                          974724774, 974631091, 974795477, 974329506, 974316285, 974753898),
                             resh = c(100353,4204126, 700922, 108355, 601225, 103091, 100100, 601231, 108354, 706264, 700413,
                                      4204082, 107440, 108162, 114271,4209222, 108357, 102939, 102141, 107505, 708761,4204500,
                                      101823, 102037, 701402, 100354, 102145,4211928, 100170,4212917))

# # tmp <- xlsx::read.xlsx('C:/GIT/qmongrdata/data-raw/SykehusNavnStruktur.xlsx', sheetIndex = 1)
# tmp <- read.csv2('C:/GIT/qmongrdata/data-raw/SykehusNavnStruktur.csv', fileEncoding = 'UTF-8')
# mapping_npr <- merge(enhetsliste, map_resh_orgnr, by.x = 'AvdRESH', by.y = 'resh')
# mapping_npr <- merge(mapping_npr, tmp[,c("OrgNrShus", "OrgNavnEnhetsreg")], by.x = 'orgnr_sh', by.y = 'OrgNrShus')
# write.csv2(mapping_npr, 'C:/GIT/norgast/doc/map_orgnr_resh_norgast.csv', row.names = F)

minald=0
maxald=130
erMann <- 99
datoFra <- '2014-01-01'
datoTil <- '2099-12-31'
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

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast1'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator1 <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


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

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast2'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator2 <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


op_gruppe <- 4
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast3'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator3 <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


op_gruppe <- 6
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast4'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator4 <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


op_gruppe <- 5
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast5'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator5 <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


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

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast6'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator6 <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


op_gruppe <- 2
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast7'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator7 <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


valgtVar <- 'LapTilgang2'
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast10'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator10 <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


op_gruppe <- 2
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast9'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator9 <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


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

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast8'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator8 <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]

indikator <- bind_rows(indikator1, indikator2, indikator3, indikator4, indikator5, indikator6, indikator7, indikator8, indikator9, indikator10)
write.csv2(indikator, "C:/GIT/qmongrdata/data-raw/norgastdata.csv", row.names = F, fileEncoding = 'UTF-8')

############## Nøkkeltall  ###########################

# nokkeltall <- RegDataOblig %>% group_by(Aar) %>% summarise("Antall operasjoner" = n(),
#                                              "Antall avdelinger" = length(unique(AvdRESH)),
#                                              "Gjennomsnittsalder" = mean(Alder),
#                                              "Medianalder" = median(Alder),
#                                              "Andel sårruptur" = sum(Saarruptur==1 & LapTilgang == 0 & Hastegrad_tid == 1, na.rm = T)/
#                                                sum(LapTilgang == 0 & Hastegrad_tid == 1, na.rm = T)*100,
#                                              "Andel reopererte" = sum(ReLapNarkose & Hastegrad_tid == 1)/sum(Hastegrad_tid)*100,
#                                              "Overlevelse 90 dager" = sum((OpDoedTid>=90 | is.na(OpDoedTid)) & Hastegrad_tid == 1)/sum(Hastegrad_tid)*100)
# nokkeltall$Dekningsgrad <- c(13.5, 21.3, 46.6, 65, 67.7)
#
# write.csv2(nokkeltall, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/nokkeltall.csv', row.names = F)


### Tilpass det nye formatet til Resultatportalen

aux <- indikator1[, c(1,5,3)]
names(aux) <- c("Aar", "Teller Ind1", "ReshId")
aux[, "Nevner Ind1"] <- 1
aux$AarID <- paste0(aux$Aar, aux$ReshId)
aux$Indikator <- "Ind1"
aux <- aux[, c(1,6,4,2,3,5)]
write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind1_sårruptur_NorGast.csv', row.names = F)

aux <- indikator2[, c(1,5,3)]
names(aux) <- c("Aar", "Teller Ind2", "ReshId")
aux[, "Nevner Ind2"] <- 1
aux$AarID <- paste0(aux$Aar, aux$ReshId)
aux$Indikator <- "Ind2"
aux <- aux[, c(1,6,4,2,3,5)]
write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind2_andelavdøde_spiserør_NorGast.csv', row.names = F)

aux <- indikator3[, c(1,5,3)]
names(aux) <- c("Aar", "Teller Ind3", "ReshId")
aux[, "Nevner Ind3"] <- 1
aux$AarID <- paste0(aux$Aar, aux$ReshId)
aux$Indikator <- "Ind3"
aux <- aux[, c(1,6,4,2,3,5)]
write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind3_andelavdøde_magesekk_NorGast.csv', row.names = F)

aux <- indikator4[, c(1,5,3)]
names(aux) <- c("Aar", "Teller Ind4", "ReshId")
aux[, "Nevner Ind4"] <- 1
aux$AarID <- paste0(aux$Aar, aux$ReshId)
aux$Indikator <- "Ind4"
aux <- aux[, c(1,6,4,2,3,5)]
write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind4_andelavdøde_bykspytt_tolv_NorGast.csv', row.names = F)

aux <- indikator5[, c(1,5,3)]
names(aux) <- c("Aar", "Teller Ind5", "ReshId")
aux[, "Nevner Ind5"] <- 1
aux$AarID <- paste0(aux$Aar, aux$ReshId)
aux$Indikator <- "Ind5"
aux <- aux[, c(1,6,4,2,3,5)]
write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind5_andelavdøde_lever_NorGast.csv', row.names = F)

aux <- indikator6[, c(1,5,3)]
names(aux) <- c("Aar", "Teller Ind6", "ReshId")
aux[, "Nevner Ind6"] <- 1
aux$AarID <- paste0(aux$Aar, aux$ReshId)
aux$Indikator <- "Ind6"
aux <- aux[, c(1,6,4,2,3,5)]
write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind6_lekkasje_tykktarm_NorGast.csv', row.names = F)

aux <- indikator7[, c(1,5,3)]
names(aux) <- c("Aar", "Teller Ind7", "ReshId")
aux[, "Nevner Ind7"] <- 1
aux$AarID <- paste0(aux$Aar, aux$ReshId)
aux$Indikator <- "Ind7"
aux <- aux[, c(1,6,4,2,3,5)]
write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind7_lekkasje_endetarm_NorGast.csv', row.names = F)

aux <- indikator8[, c(1,5,3)]
names(aux) <- c("Aar", "Teller Ind8", "ReshId")
aux[, "Nevner Ind8"] <- 1
aux$AarID <- paste0(aux$Aar, aux$ReshId)
aux$Indikator <- "Ind8"
aux <- aux[, c(1,6,4,2,3,5)]
write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind8_kikkhullsteknikk_ lever_NorGast.csv', row.names = F)

aux <- indikator9[, c(1,5,3)]
names(aux) <- c("Aar", "Teller Ind9", "ReshId")
aux[, "Nevner Ind9"] <- 1
aux$AarID <- paste0(aux$Aar, aux$ReshId)
aux$Indikator <- "Ind9"
aux <- aux[, c(1,6,4,2,3,5)]
write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind9_kikkhullsteknikk_tykktarm_NorGast.csv', row.names = F)

aux <- indikator10[, c(1,5,3)]
names(aux) <- c("Aar", "Teller Ind10", "ReshId")
aux[, "Nevner Ind10"] <- 1
aux$AarID <- paste0(aux$Aar, aux$ReshId)
aux$Indikator <- "Ind10"
aux <- aux[, c(1,6,4,2,3,5)]
write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind10_kikkhullsteknikk_endetarm_NorGast.csv', row.names = F)


############## Nøkkeltall  ###########################

nokkeltall <- RegDataOblig %>% group_by(Aar) %>% summarise("Antall operasjoner" = n(),
                                                           "Antall avdelinger" = length(unique(AvdRESH)),
                                                           "Gjennomsnittsalder" = mean(Alder),
                                                           "Medianalder" = median(Alder),
                                                           "Andel sårruptur" = sum(Saarruptur==1 & LapTilgang == 0 & Hastegrad_tid == 1, na.rm = T)/
                                                             sum(LapTilgang == 0 & Hastegrad_tid == 1, na.rm = T),
                                                           "Andel reopererte" = sum(ReLapNarkose & Hastegrad_tid == 1)/sum(Hastegrad_tid),
                                                           "Overlevelse 90 dager" = sum((OpDoedTid>=90 | is.na(OpDoedTid)) & Hastegrad_tid == 1)/sum(Hastegrad_tid))
nokkeltall$Dekningsgrad <- c(13.5, 21.3, 46.6, 65, 67.7, NA, NA)/100

write.csv2(nokkeltall, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/nokkeltall.csv', row.names = F)








