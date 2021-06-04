library(norgast)
library(tidyverse)
rm(list = ls())

rap_aar <- 2020

RegData <- read.table('I:/norgast/AlleVarNum2021-06-02 08-20-32.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2021-06-02 08-20-32.txt', header=TRUE, sep=";", encoding = 'UTF-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad",
                      "Rekonstruksjon", "Rekonstruksjonstype", "EndoInterLekkasje", "EndoInterBlod", "PerkDrenasje",
                      "HoyAmylaseKons", "AvstandAnalVerge", "KunDrenasje", "TelefonKontroll", "FysiskKontroll")]
names(ForlopData)[match(c("SykehusNavn", "erMann"), names(ForlopData))] <- c("Sykehusnavn", "ErMann")
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID',
                            'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
RegData <- RegData[RegData$Aar <= rap_aar, ]

# RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
RegDataOblig <- RegData[RegData$Op_gr %in% 1:7, ]

enhetsliste <- RegDataOblig[match(unique(RegDataOblig$AvdRESH), RegDataOblig$AvdRESH), c("AvdRESH", "Sykehusnavn")]
# write.csv2(enhetsliste, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/enhetsliste.csv', row.names = F)

# OBS, OUS er foreløpig registrert under en felles resh. Her benytter vi Rikshospitalet. AHUS mappes til AHUS NORDBYHAGEN SOMATIKK?

map_resh_orgnr <- data.frame(orgnr_sh = c(974733013, 974631407, 974557746, 974632535, 974795787, 974705788, 974633574, 974795639,
                                          974724960, 974795361, 993467049, 974631326, 974749025, 974706490, 974703300, 974633752,
                                          874632562, 974743272, 974795515, 974116804, 974747138, 974745569, 974795833, 974633191,
                                          974724774, 974631091, 974795477, 974329506, 974316285, 974753898, 974631407, 974795558,
                                          974795574, 874716782, 974707152, 974589095, 974754118),
                             resh = c(100353,4204126, 700922, 108355, 601225, 103091, 100100, 601231, 108354, 706264, 700413,
                                      4204082, 107440, 108162, 114271,4209222, 108357, 102939, 102141, 107505, 708761,4204500,
                                      101823, 102037, 701402, 100354, 102145,4211928, 100170,4212917, 4204084, 700840, 700841,
                                      103312, 4205289, 106168, 4207594))
# map_resh_orgnr[map_resh_orgnr$resh%in% setdiff(map_resh_orgnr$resh, enhetsliste$AvdRESH), ]
# enhetsliste[enhetsliste$AvdRESH %in% setdiff(enhetsliste$AvdRESH, map_resh_orgnr$resh), ]
# # tmp <- xlsx::read.xlsx('C:/GIT/qmongrdata/data-raw/SykehusNavnStruktur.xlsx', sheetIndex = 1)
tmp <- read.csv2('C:/GIT/qmongrdata/data-raw/SykehusNavnStruktur.csv', fileEncoding = 'UTF-8')
mapping_npr <- merge(enhetsliste, map_resh_orgnr, by.x = 'AvdRESH', by.y = 'resh')
mapping_npr <- merge(mapping_npr, tmp[,c("OrgNrShus", "OrgNavnEnhetsreg")], by.x = 'orgnr_sh', by.y = 'OrgNrShus')
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


op_gruppe <- 1
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
indikator$denominator <- 1
indikator <- indikator[, c(4,1,5,7,6)]
names(indikator) <- c("orgnr",	"year",	"var",	"denominator",	"ind_id")
indikator$ind_id[indikator$ind_id == "norgast1"] <- "norgast_saarruptur"
indikator$ind_id[indikator$ind_id == "norgast2"] <- "norgast_avdoede_spiseroer"
indikator$ind_id[indikator$ind_id == "norgast3"] <- "norgast_avdoede_magesekk"
indikator$ind_id[indikator$ind_id == "norgast4"] <- "norgast_avdoede_bukspytt_tolv"
indikator$ind_id[indikator$ind_id == "norgast5"] <- "norgast_avdoede_lever"
indikator$ind_id[indikator$ind_id == "norgast6"] <- "norgast_lekkasje_tykktarm"
indikator$ind_id[indikator$ind_id == "norgast7"] <- "norgast_lekkasje_endetarm"
indikator$ind_id[indikator$ind_id == "norgast8"] <- "norgast_kikkhullsteknikk_lever"
indikator$ind_id[indikator$ind_id == "norgast9"] <- "norgast_kikkhullsteknikk_tykktarm"
indikator$ind_id[indikator$ind_id == "norgast10"] <- "norgast_kikkhullsteknikk_endetarm"
indikator$context <- "caregiver"

write.csv2(indikator, "I:/norgast/norgast_indikator_2021_06_04.csv", row.names = F, fileEncoding = 'UTF-8')
# write.csv2(indikator, "C:/GIT/qmongrdata/data-raw/norgastdata.csv", row.names = F, fileEncoding = 'UTF-8')

### Tilbered dekningsgrad for sykehusviser

dg_kobl_resh_orgnr <- data.frame(orgnr_sh = c(974733013, 974631407, 974557746, 974632535, 974795787, 974705788, 974633574, 974795639,
                                              974724960, 974795361, 993467049, 974631326, 974749025, 974706490, 974703300, 974633752,
                                              874632562, 974743272, 974795515, 974116804, 974747138, 974745569, 974795833, 974633191,
                                              974724774, 974631091, 974795477, 974329506, 974316285, 974631407, 974795558,
                                              974795574, 874716782, 974707152, 974631776, 974744570, 974747545, 974753898, 974795558,
                                              974795574, 974754118),
                                 resh = c(100353,4204126, 700922, 108355, 601225, 103091, 100100, 601231, 108354, 706264, 700413,
                                          4204082, 107440, 108162, 114271,4209222, 108357, 102939, 102141, 107505, 708761,4204500,
                                          101823, 102037, 701402, 100354, 102145,4211928, 100170, 4204084, 700840, 700841,
                                          103312,4205289, 974631776, 974744570, 974747545, 974753898, 974795558, 974795574, 4212917))

dg <- readxl::read_excel("Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Sykehusviser/Dekningsgrader/DG_NorGast.xlsx",
                         sheet = "Total DG per SH")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[which(dg$Sykehus=="Levanger")] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_total"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- dg


dg <- readxl::read_excel("Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Sykehusviser/Dekningsgrader/DG_NorGast.xlsx",
                         sheet = "DG Tykktarm")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_tykktarm"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

dg <- readxl::read_excel("Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Sykehusviser/Dekningsgrader/DG_NorGast.xlsx",
                         sheet = "DG_Lever")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_lever"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

dg <- readxl::read_excel("Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Sykehusviser/Dekningsgrader/DG_NorGast.xlsx",
                         sheet = "DG_Pankreas")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_pankreas"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

dg <- readxl::read_excel("Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Sykehusviser/Dekningsgrader/DG_NorGast.xlsx",
                         sheet = "DG_Endetarm")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_endetarm"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

dg <- readxl::read_excel("Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Sykehusviser/Dekningsgrader/DG_NorGast.xlsx",
                         sheet = "DG_Magesekk")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_magesekk"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

dg <- readxl::read_excel("Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Sykehusviser/Dekningsgrader/DG_NorGast.xlsx",
                         sheet = "DG_Spiseroer")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_spiseroer"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

# tmp <- RegDataOblig[substr(RegDataOblig$Sykehusnavn, 1, 3) %in% "OUS", ]

data.frame(resh=unique(RegDataOblig$AvdRESH),
           shus = RegDataOblig$Sykehusnavn[match(unique(RegDataOblig$AvdRESH),RegDataOblig$AvdRESH)])

feilsok <- indikator
feilsok$shus <- NA
feilsok$shus[feilsok$orgnr %in% qmongrdata::SykehusNavnStruktur$OrgNrShus] <-
  qmongrdata::SykehusNavnStruktur$SykehusNavn[match(feilsok$orgnr[feilsok$orgnr %in% qmongrdata::SykehusNavnStruktur$OrgNrShus],
                                                    qmongrdata::SykehusNavnStruktur$OrgNrShus)]
feilsok$shus[feilsok$orgnr %in% qmongrdata::SykehusNavnStruktur$OrgNrHF] <-
  qmongrdata::SykehusNavnStruktur$HF[match(feilsok$orgnr[feilsok$orgnr %in% qmongrdata::SykehusNavnStruktur$OrgNrHF],
                                                    qmongrdata::SykehusNavnStruktur$OrgNrHF)]

# indikator <- bind_rows(indikator, dg_samlet)

write.csv2(indikator[indikator$year <= rap_aar, ], "C:/GIT/qmongrdata/data-raw/norgastdata.csv", row.names = F, fileEncoding = 'UTF-8')

dg_samlet2 <-dg_samlet

tmp <- merge(dg_samlet2[dg_samlet2$year == 2014, ], dg_samlet2[dg_samlet2$year == 2016, c("orgnr", "ind_id", "denominator")],
             by = c("orgnr", "ind_id"), all.x = T, suffixes = c('', '_2016')) %>%
  merge(dg_samlet2[dg_samlet2$year == 2018, c("orgnr", "ind_id", "denominator")],
        by = c("orgnr", "ind_id"), all.x = T, suffixes = c('', '_2018')) %>%
  merge(dg_samlet2[dg_samlet2$year == 2019, c("orgnr", "ind_id", "denominator")],
        by = c("orgnr", "ind_id"), all.x = T, suffixes = c('', '_2019'))

write.csv2(dg_samlet[dg_samlet$year <= rap_aar, ], "C:/GIT/qmongrdata/data-raw/norgast_dg.csv", row.names = F, fileEncoding = 'UTF-8')

# write.csv2(indikator[which(indikator$year < 2020 & indikator$ind_id == "norgast_avdoede_bukspytt_tolv"), ],
#            "C:/GIT/qmongrdata/data-raw/norgastdata_pankread.csv", row.names = F, fileEncoding = 'UTF-8')

# dg_tot <- xlsx::read.xlsx2("Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Sykehusviser/Dekningsgrader/DG_NorGast.xlsx",
#                  sheetName = "Total DG per SH", as.data.frame = T, header = T, colClasses = "character")
# dg_tot[match(unique(dg_tot$ReshID), dg_tot$ReshID),]
# dg_tot <- dg_tot[, c(1,2,5)]
#
# write.csv2(dg_tot, "C:/GIT/norgast/doc/dg.csv", row.names = F)
# write.csv2(dg2, "C:/GIT/norgast/doc/dg2.csv", row.names = F)

# # Hent orgnr fra koblingstabell
# dg_tot$orgnr <- map_resh_orgnr$orgnr_sh[match(dg_tot$ReshID, map_resh_orgnr$resh)]
#
# i_regdata_mangler_i_dgtabell <- data.frame(resh= unique(RegDataOblig$AvdRESH[!(RegDataOblig$AvdRESH %in% dg_tot$ReshID)]),
#                                   shus = RegDataOblig$Sykehusnavn[match(unique(RegDataOblig$AvdRESH[!(RegDataOblig$AvdRESH %in% dg_tot$ReshID)]),
#                                                                         RegDataOblig$AvdRESH)])
# i_regdata_mangler_i_dgtabell$orgnr <- map_resh_orgnr$orgnr_sh[match(i_regdata_mangler_i_dgtabell$resh, map_resh_orgnr$resh)]
#
# tmp <- dg_tot[is.na(dg_tot$orgnr), ]
# i_dgtabell_mangler_i_regdata <- tmp[match(unique(tmp$ReshID), tmp$ReshID), c("Sykehus", "ReshID")]


### Tilpass det nye formatet til Resultatportalen

# aux <- indikator1[, c(1,5,3)]
# names(aux) <- c("Aar", "Teller Ind1", "ReshId")
# aux[, "Nevner Ind1"] <- 1
# aux$AarID <- paste0(aux$Aar, aux$ReshId)
# aux$Indikator <- "Ind1"
# aux <- aux[, c(1,6,4,2,3,5)]
# write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind1_sårruptur_NorGast.csv', row.names = F)
#
# aux <- indikator2[, c(1,5,3)]
# names(aux) <- c("Aar", "Teller Ind2", "ReshId")
# aux[, "Nevner Ind2"] <- 1
# aux$AarID <- paste0(aux$Aar, aux$ReshId)
# aux$Indikator <- "Ind2"
# aux <- aux[, c(1,6,4,2,3,5)]
# write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind2_andelavdøde_spiserør_NorGast.csv', row.names = F)
#
# aux <- indikator3[, c(1,5,3)]
# names(aux) <- c("Aar", "Teller Ind3", "ReshId")
# aux[, "Nevner Ind3"] <- 1
# aux$AarID <- paste0(aux$Aar, aux$ReshId)
# aux$Indikator <- "Ind3"
# aux <- aux[, c(1,6,4,2,3,5)]
# write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind3_andelavdøde_magesekk_NorGast.csv', row.names = F)
#
# aux <- indikator4[, c(1,5,3)]
# names(aux) <- c("Aar", "Teller Ind4", "ReshId")
# aux[, "Nevner Ind4"] <- 1
# aux$AarID <- paste0(aux$Aar, aux$ReshId)
# aux$Indikator <- "Ind4"
# aux <- aux[, c(1,6,4,2,3,5)]
# write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind4_andelavdøde_bykspytt_tolv_NorGast.csv', row.names = F)
#
# aux <- indikator5[, c(1,5,3)]
# names(aux) <- c("Aar", "Teller Ind5", "ReshId")
# aux[, "Nevner Ind5"] <- 1
# aux$AarID <- paste0(aux$Aar, aux$ReshId)
# aux$Indikator <- "Ind5"
# aux <- aux[, c(1,6,4,2,3,5)]
# write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind5_andelavdøde_lever_NorGast.csv', row.names = F)
#
# aux <- indikator6[, c(1,5,3)]
# names(aux) <- c("Aar", "Teller Ind6", "ReshId")
# aux[, "Nevner Ind6"] <- 1
# aux$AarID <- paste0(aux$Aar, aux$ReshId)
# aux$Indikator <- "Ind6"
# aux <- aux[, c(1,6,4,2,3,5)]
# write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind6_lekkasje_tykktarm_NorGast.csv', row.names = F)
#
# aux <- indikator7[, c(1,5,3)]
# names(aux) <- c("Aar", "Teller Ind7", "ReshId")
# aux[, "Nevner Ind7"] <- 1
# aux$AarID <- paste0(aux$Aar, aux$ReshId)
# aux$Indikator <- "Ind7"
# aux <- aux[, c(1,6,4,2,3,5)]
# write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind7_lekkasje_endetarm_NorGast.csv', row.names = F)
#
# aux <- indikator8[, c(1,5,3)]
# names(aux) <- c("Aar", "Teller Ind8", "ReshId")
# aux[, "Nevner Ind8"] <- 1
# aux$AarID <- paste0(aux$Aar, aux$ReshId)
# aux$Indikator <- "Ind8"
# aux <- aux[, c(1,6,4,2,3,5)]
# write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind8_kikkhullsteknikk_ lever_NorGast.csv', row.names = F)
#
# aux <- indikator9[, c(1,5,3)]
# names(aux) <- c("Aar", "Teller Ind9", "ReshId")
# aux[, "Nevner Ind9"] <- 1
# aux$AarID <- paste0(aux$Aar, aux$ReshId)
# aux$Indikator <- "Ind9"
# aux <- aux[, c(1,6,4,2,3,5)]
# write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind9_kikkhullsteknikk_tykktarm_NorGast.csv', row.names = F)
#
# aux <- indikator10[, c(1,5,3)]
# names(aux) <- c("Aar", "Teller Ind10", "ReshId")
# aux[, "Nevner Ind10"] <- 1
# aux$AarID <- paste0(aux$Aar, aux$ReshId)
# aux$Indikator <- "Ind10"
# aux <- aux[, c(1,6,4,2,3,5)]
# write.csv2(aux, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/5. NorGast/Indikatorer/ind10_kikkhullsteknikk_endetarm_NorGast.csv', row.names = F)


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



########## Legge til teller i data - UNDER UTVIKLING ################################

RegData <- read.table('I:/norgast/AlleVarNum2021-06-02 08-20-32.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2021-06-02 08-20-32.txt', header=TRUE, sep=";",
                         encoding = 'UTF-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]

ForlopData <- ForlopData[,c('erMann', 'AvdRESH', 'SykehusNavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
names(ForlopData)[match(c("SykehusNavn", "erMann"), names(ForlopData))] <- c("Sykehusnavn", "ErMann")
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
# RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
# RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
RegData$op_gr_npr <- 'Annet'
RegData$ncsp_lowercase <- substr(tolower(RegData$Hovedoperasjon), 1, 5)
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jfh")] <- "Kolonreseksjoner"
RegData$op_gr_npr[intersect(which(substr(RegData$ncsp_lowercase,1,3)=="jfb"),
                            which(as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% 20:64))] <- "Kolonreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jgb")] <- "Rektumreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jcc")] <- "Øsofagusreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jdc")] <- "Ventrikkelreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jdd")] <- "Ventrikkelreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jjb")] <- "Leverreseksjoner"
RegData$op_gr_npr[intersect(which(substr(RegData$ncsp_lowercase,1,3)=="jlc"),
                            which(as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% c(0:40, 96)))] <- "Pankreasreseksjoner"
RegData <- RegData[which(RegData$op_gr_npr != 'Annet'), ]
RegData$Aar <- format(as.Date(RegData$HovedDato), '%Y')
RegData <- RegData[RegData$Aar %in% 2014:2019,  ]
RegData <- RegData[RegData$BasisRegStatus == 1, ]

library(tidyverse)
tmp <- RegData %>% group_by(PasientID, HovedDato, Hovedoperasjon) %>% summarise(antall = n(),
                                                                                ForlopsID = ForlopsID[1])
tmp <- tmp[tmp$antall>1, ]
RegData <- RegData[!(RegData$ForlopsID %in% tmp$ForlopsID), ] # fjern dobbelreg
RegData <- RegData[, c("PasientID", "ForlopsID", "HovedDato", "Hovedoperasjon", "op_gr_npr", "AvdRESH", "Sykehusnavn")]

RegData$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(RegData$AvdRESH, dg_kobl_resh_orgnr$resh)]
RegData$year <- as.numeric(substr(RegData$HovedDato, 1,4))

RegData$ind_id <- NA
RegData$ind_id[RegData$op_gr_npr == "Kolonreseksjoner"] <- "norgast_dg_tykktarm"
RegData$ind_id[RegData$op_gr_npr == "Rektumreseksjoner"] <- "norgast_dg_endetarm"
RegData$ind_id[RegData$op_gr_npr == "Øsofagusreseksjoner"] <- "norgast_dg_spiseroer"
RegData$ind_id[RegData$op_gr_npr == "Ventrikkelreseksjoner"] <- "norgast_dg_magesekk"
RegData$ind_id[RegData$op_gr_npr == "Leverreseksjoner"] <- "norgast_dg_lever"
RegData$ind_id[RegData$op_gr_npr == "Pankreasreseksjoner"] <- "norgast_dg_pankreas"


sammenstill <- RegData %>% group_by(orgnr, year, ind_id) %>% summarise(teller = n())
samlet <- RegData %>% group_by(orgnr, year) %>% summarise(teller = n())
samlet$ind_id <- "norgast_dg_total"

sammenstill <- bind_rows(sammenstill, samlet)

tmp <- merge(dg_samlet, sammenstill, by = c("orgnr", "year", "ind_id"), all.x = T)
tmp2 <- tmp[tmp$year==2017, ]
tmp2$nevner[tmp2$var!=0] <- round(tmp2$teller[tmp2$var!=0]*100/tmp2$var[tmp2$var!=0])
# tmp2$nevner[tmp2$var==0] <- 0

gjsn.2017 <- merge(dg_samlet[dg_samlet$year==2016, -3], dg_samlet[dg_samlet$year==2018, -3],
                   by = c("orgnr", "ind_id"), all = T, suffixes = c("_2016", "_2018"))



nevner <- dg_samlet[dg_samlet$year==2014, -c(2)] %>%
  merge(dg_samlet[dg_samlet$year==2015, -c(2)], by = c("orgnr", "ind_id"),
        all = T, suffixes = c("_2014", "_2015")) %>%
  merge(dg_samlet[dg_samlet$year==2016, -c(2)], by = c("orgnr", "ind_id"),
        all = T, suffixes = c("", "_2016")) %>%
  merge(dg_samlet[dg_samlet$year==2017, -c(2)], by = c("orgnr", "ind_id"),
        all = T, suffixes = c("", "_2017")) %>%
  merge(dg_samlet[dg_samlet$year==2018, -c(2)], by = c("orgnr", "ind_id"),
        all = T, suffixes = c("", "_2018")) %>%
  merge(dg_samlet[dg_samlet$year==2019, -c(2)], by = c("orgnr", "ind_id"),
        all = T, suffixes = c("", "_2019"))


mapping_npr <- mapping_npr[mapping_npr$AvdRESH != 4204084, ] # fjerner én av Ringerikeoppføringene

nevner <- merge(nevner, mapping_npr[,c("orgnr_sh", "Sykehusnavn")], by.x = "orgnr", by.y = "orgnr_sh", all.x = T)

# nevner[is.na(nevner$Sykehusnavn), ]




