library(norgast)
library(tidyverse)
rm(list = ls())

rap_aar <- 2021

RegData <-  norgast::NorgastHentRegData()
skjemaoversikt <- norgast::NorgastHentSkjemaOversikt()
skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
RegData <- norgast::NorgastPreprosess(RegData, behold_kladd = TRUE)
skjemaoversikt <- merge(skjemaoversikt, RegData[,c("ForlopsID", "Op_gr", "Hovedoperasjon")], by = "ForlopsID", all.x = T)
RegData <- RegData[which(RegData$RegistreringStatus==1),]
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
RegData <- RegData[RegData$Aar <= rap_aar, ]

RegDataOblig <- RegData[RegData$Op_gr %in% 1:7, ]

enhetsliste <- RegDataOblig[match(unique(RegDataOblig$AvdRESH), RegDataOblig$AvdRESH), c("AvdRESH", "Sykehusnavn")]

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

hastegrad_hybrid=1
tilgang = c('1', '3')
valgtVar <- 'Saarruptur'

NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
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

#########
tilgang = ''
valgtVar <- 'AktivKontroll_v2'
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig,
                               datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast_aktivkontroll'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator_aktivkontroll <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


valgtVar <- 'Vekttap_registrert'
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig,
                               datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData

indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
indikator$KvalIndID <- 'norgast_vekt_reg'
indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
indikator_vekt_reg <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]

#########

hastegrad_hybrid=99
tilgang = ''
op_gruppe <- 3
valgtVar <- 'mortalitet90'

NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
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


# valgtVar <- "Anastomoselekk_osofagus"
# NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
#                                maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
#                                BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
#                                ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
#                                op_gruppe=op_gruppe, ncsp=ncsp)
# RegData <- NorgastUtvalg$RegData
# PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
# RegData <- PlotParams$RegData
#
# indikator <- RegData[, c("AvdRESH", "Aar", "Variabel", "Sykehusnavn")]
# names(indikator) <- c('ReshId', 'Aar', 'Variabel', 'ShNavn')
# indikator$KvalIndID <- 'norgast2'
# indikator$OrgNrShus <- map_resh_orgnr$orgnr_sh[match(indikator$ReshId, map_resh_orgnr$resh)]
# indikator_anastomelekk_osofagus <- indikator[, c("Aar", "ShNavn", "ReshId", "OrgNrShus", "Variabel", "KvalIndID")]


op_gruppe <- 4
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
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
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
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
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
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
hastegrad_hybrid=1

NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
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
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
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
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
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
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
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
hastegrad_hybrid=99
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, hastegrad_hybrid=hastegrad_hybrid,
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



indikator <- bind_rows(indikator1, indikator2, indikator3, indikator4, indikator5,
                       indikator6, indikator7, indikator8, indikator9, indikator10,
                       indikator_aktivkontroll, indikator_vekt_reg)
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

# write.csv2(indikator, "~/.ssh/norgast/norgast_indikator_2022_08_04.csv", row.names = F, fileEncoding = 'UTF-8')


### Tilbered dekningsgrad for sykehusviser

dg_kobl_resh_orgnr <- data.frame(orgnr_sh = c(974733013, 974631407, 974557746, 974632535, 974795787, 974705788, 974633574, 974795639,
                                              974724960, 974795361, 993467049, 974631326, 974749025, 974706490, 974703300, 974633752,
                                              874632562, 974743272, 974795515, 974116804, 974747138, 974745569, 974795833, 974633191,
                                              974724774, 974631091, 974795477, 974329506, 974316285, 974631407, 974795558,
                                              974795574, 874716782, 974707152, 974631776, 974744570, 974747545, 974753898, 974795558,
                                              974795574, 974754118, 974589095, 974754118),
                                 resh = c(100353,4204126, 700922, 108355, 601225, 103091, 100100, 601231, 108354, 706264, 700413,
                                          4204082, 107440, 108162, 114271,4209222, 108357, 102939, 102141, 107505, 708761,4204500,
                                          101823, 102037, 701402, 100354, 102145,4211928, 100170, 4204084, 700840, 700841,
                                          103312,4205289, 974631776, 974744570, 974747545, 974753898, 974795558, 974795574, 4212917,
                                          106168, 4207594))

dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
                         sheet = "Total DG per SH")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[which(dg$Sykehus=="Levanger")] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_total"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- dg


dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
                         sheet = "DG Tykktarm")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_tykktarm"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
                         sheet = "DG_Lever")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_lever"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
                         sheet = "DG_Pankreas")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_pankreas"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
                         sheet = "DG_Endetarm")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_endetarm"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
                         sheet = "DG_Magesekk")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_magesekk"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
                         sheet = "DG_Spiseroer")
dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
dg <- dg[,c(10,6,2,3)]
dg <- dg[!is.na(dg$orgnr), ]
dg$ind_id <- "norgast_dg_spiseroer"
names(dg)[2:4] <- c("year",	"var", "denominator")
dg_samlet <- bind_rows(dg_samlet, dg)

# Legg til 2021
mapping_npr <- read.csv2('~/.ssh/Sykehus/Koblingstabell_AvdRESH_sh_standard.csv', fileEncoding = "Latin1")
DG <- read.csv2('~/.ssh/Sykehus/Alle_sh.csv', fileEncoding = "Latin1")
DG$ind_id <- "norgast_dg_total"
DG_samlet <- DG

DG <- read.csv2('~/.ssh/Sykehus/Kolon_sh.csv', fileEncoding = "Latin1")
DG$ind_id <- "norgast_dg_tykktarm"
DG_samlet <- bind_rows(DG_samlet, DG)

DG <- read.csv2('~/.ssh/Sykehus/Rektum_sh.csv', fileEncoding = "Latin1")
DG$ind_id <- "norgast_dg_endetarm"
DG_samlet <- bind_rows(DG_samlet, DG)

DG <- read.csv2('~/.ssh/Sykehus/Lever_sh.csv', fileEncoding = "Latin1")
DG$ind_id <- "norgast_dg_lever"
DG_samlet <- bind_rows(DG_samlet, DG)

DG <- read.csv2('~/.ssh/Sykehus/Ventrikkel_sh.csv', fileEncoding = "Latin1")
DG$ind_id <- "norgast_dg_magesekk"
DG_samlet <- bind_rows(DG_samlet, DG)

DG <- read.csv2('~/.ssh/Sykehus/Whipple_sh.csv', fileEncoding = "Latin1")
DG$ind_id <- "norgast_dg_pankreas"
DG_samlet <- bind_rows(DG_samlet, DG)

DG <- read.csv2('~/.ssh/Sykehus/Øsofagus_sh.csv', fileEncoding = "Latin1")
DG$ind_id <- "norgast_dg_spiseroer"
DG_samlet <- bind_rows(DG_samlet, DG)

DG_samlet$AvdRESH <- mapping_npr$AvdRESH[match(DG_samlet$sh_standard, mapping_npr$sh_standard)]
DG_samlet <- DG_samlet[!is.na(DG_samlet$AvdRESH), ]
DG_samlet$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(DG_samlet$AvdRESH, dg_kobl_resh_orgnr$resh)]
DG_samlet$year <- 2021
DG_samlet$var <- DG_samlet$Begge + DG_samlet$Kun_norgast
DG_samlet$denominator <- DG_samlet$Total

DG_samlet <- DG_samlet[,c("orgnr", "year", "var", "denominator", "ind_id")]
dg_samlet <- bind_rows(DG_samlet, dg_samlet)
dg_samlet$context <- "caregiver"
dg_samlet$var <- round(dg_samlet$var)

indikator <- bind_rows(indikator, dg_samlet)

write.csv2(indikator, "~/.ssh/norgast/norgast_indikator_2022_09_06.csv", row.names = F, fileEncoding = 'UTF-8')

slett_resident <- indikator[match(unique(indikator$ind_id), indikator$ind_id), ]
slett_resident$context <- "resident"
slett_resident$var <- 0
slett_resident$denominator <- 1

write.csv2(slett_resident, "~/.ssh/norgast/norgast_slett_resident_2022_09_06.csv", row.names = F, fileEncoding = 'UTF-8')

# write.csv2(dg_samlet[dg_samlet$year <= rap_aar, ], "~/.ssh/norgast/norgast_dg.csv", row.names = F, fileEncoding = 'UTF-8')
#
# ind_info <- readxl::read_xlsx("~/.ssh/norgast/Indikatorbeskrivelse publisering SKDE_KH.xlsx", sheet = 2)

