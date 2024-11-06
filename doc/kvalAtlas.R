library(norgast)
library(tidyverse)
rm(list = ls())

RegData <- read.table('I:/norgast/allevarnum2020-10-16 10-02-12.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- read.table('I:/norgast/forlopsoversikt2020-10-16 10-02-12.txt', header=TRUE, sep=";", encoding = 'UTF-8')
# Adresser1 <- read.table('I:/norgast/Adresser ved operasjonsdato.csv', header=TRUE, sep=",", encoding = 'UTF-8', colClasses = "character")
Adresser <- read.table('I:/norgast/Dagens_adresser_i_NoRGast_pr_15102020.csv', header=TRUE, sep=",", encoding = 'UTF-8', colClasses = "character")
Adresser$bydel <- paste0(Adresser$MUNICIPALITY_NUMBER, substr(Adresser$DISTRICTCODE, 3, 4))
kobling_bosted_hf <- read.table('I:/kommuner_bydel_2020.csv', header=TRUE, sep=";", encoding = 'Latin1', colClasses = "character")
kobling_bosted_hf$bydel2 <- kobling_bosted_hf$bydel
kobling_bosted_hf$bydel2[nchar(kobling_bosted_hf$bydel2)==5] <- paste0("0", kobling_bosted_hf$bydel2[nchar(kobling_bosted_hf$bydel2)==5])
kobling_bosted_hf$bydel2[nchar(kobling_bosted_hf$bydel2)==0] <- kobling_bosted_hf$komnr[nchar(kobling_bosted_hf$bydel2)==0]
kobling_bosted_hf$bydel2[nchar(kobling_bosted_hf$bydel2)==3] <- paste0("0", kobling_bosted_hf$bydel2[nchar(kobling_bosted_hf$bydel2)==3])
kobling_bosted_hf <- kobling_bosted_hf[order(kobling_bosted_hf$bydel2, decreasing = T), ]

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

RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
RegDataOblig <- RegData[RegData$Op_gr %in% 1:7, ]

RegDataOblig <- merge(RegDataOblig, Adresser, by.x = "ForlopsID", by.y = "MCEID")
RegDataOblig <- merge(RegDataOblig, kobling_bosted_hf, by.x = "bydel", by.y = "bydel2", all.x = T)

RegDataOblig$bydel2 <- RegDataOblig$bydel
RegDataOblig[RegDataOblig$bydel %in% setdiff(RegDataOblig$bydel, kobling_bosted_hf$bydel2) & RegDataOblig$bydel != "" , names(kobling_bosted_hf)[-3]] <-
  kobling_bosted_hf[match(substr(RegDataOblig$bydel[RegDataOblig$bydel %in% setdiff(RegDataOblig$bydel,
                                                                                    kobling_bosted_hf$bydel2) & RegDataOblig$bydel != ""], 1, 4),
                          substr(kobling_bosted_hf$bydel2, 1, 4)), names(kobling_bosted_hf)[-3]]

# improvisert_maping <- RegDataOblig[RegDataOblig$bydel %in% setdiff(RegDataOblig$bydel, kobling_bosted_hf$bydel2) & RegDataOblig$bydel != "" ,
#                         c(names(kobling_bosted_hf)[-3], "bydel", "MUNICIPALITY_NUMBER", "DISTRICTCODE")]
# improvisert_maping <- improvisert_maping[match(setdiff(improvisert_maping$bydel, improvisert_maping$bydel2), improvisert_maping$bydel), ]
#
# tomme <- RegDataOblig[RegDataOblig$bydel == "", c("Sykehusnavn", "PasientID", "ForlopsID", "Avdod", "AvdodDato", "HovedDato")]
# tomme <- tomme[order(tomme$PasientID), ]
#
# oppsum <- tomme %>% group_by(PasientID) %>% summarise(ant_forlop = n())
# oppsum <- oppsum[order(oppsum$ant_forlop, decreasing = T), ]
#
# tmp <- merge(tomme, RegDataOblig[!(RegDataOblig$ForlopsID %in% tomme$ForlopsID), c("PasientID", "ForlopsID", "HovedDato", "bydel")], by = "PasientID", all.x = T)

# testdata <- RegDataOblig[RegDataOblig$MUNICIPALITY_NUMBER == "", c("PasientID", "ForlopsID", "Avdod", "AvdodDato", "HovedDato", "Aar", "Sykehusnavn")]
# write.csv2(testdata, "doc/mangler_bosted.csv", row.names = F, fileEncoding = "Latin1")
#
# personinfo <- read.table('I:/norgast/NORGAST-334_AllenorgastPasienter_2020-03-11.csv', header=TRUE, sep=",",
#                       encoding = 'UTF-8', stringsAsFactors = F, colClasses = c('integer', 'character'))
#
# length(unique(testdata$PasientID))
# length(intersect(testdata$PasientID, personinfo$PID))
#
# testdata[testdata$PasientID %in% setdiff(testdata$PasientID, personinfo$PID), ]
#
# testdata_agg <- testdata %>% group_by(Aar, Sykehusnavn) %>% summarise(Ant_dode = sum(Avdod),
#                                                       N = n())
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
tilgang = ''

hastegrad_hybrid <- 1
tilgang = c('1', '3')
valgtVar <- 'Saarruptur'

NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, #elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp, hastegrad_hybrid=hastegrad_hybrid)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData
aldersgr <- sort(RegData$Alder)[round(seq(1, dim(RegData)[1], length.out = 4))]
RegData$aldersgruppe <- cut(RegData$Alder, breaks = aldersgr, include.lowest = T)
ind3 <- RegData[ , c("Aar", "opptaksomr", "erMann", "Variabel", "aldersgruppe")] %>%
  group_by(Aar, opptaksomr, erMann, aldersgruppe) %>% summarise(teller = sum(Variabel),
                                                                nevner = n())

op_gruppe <- 1
tilgang <- ''
valgtVar <- 'Anastomoselekkasje'
whoEcog= c('0', '1')
malign <- 1
# elektiv=1
hastegrad_hybrid <- 1

NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, #elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp, hastegrad_hybrid=hastegrad_hybrid)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData
aldersgr <- sort(RegData$Alder)[round(seq(1, dim(RegData)[1], length.out = 4))]
RegData$aldersgruppe <- cut(RegData$Alder, breaks = aldersgr, include.lowest = T)
ind1 <- RegData[ , c("Aar", "opptaksomr", "erMann", "Variabel", "aldersgruppe")] %>%
  group_by(Aar, opptaksomr, erMann, aldersgruppe) %>% summarise(teller = sum(Variabel),
                                                                nevner = n())

op_gruppe <- 2
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, #elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp, hastegrad_hybrid=hastegrad_hybrid)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData
aldersgr <- sort(RegData$Alder)[round(seq(1, dim(RegData)[1], length.out = 4))]
RegData$aldersgruppe <- cut(RegData$Alder, breaks = aldersgr, include.lowest = T)
ind2 <- RegData[ , c("Aar", "opptaksomr", "erMann", "Variabel", "aldersgruppe")] %>%
  group_by(Aar, opptaksomr, erMann, aldersgruppe) %>% summarise(teller = sum(Variabel),
                                                                nevner = n())


op_gruppe <- c(1,2)
tilgang = ''
malign <- 99
whoEcog=''
valgtVar <- 'LapTilgang2'
NorgastUtvalg <- NorgastUtvalg(RegData=RegDataOblig, datoFra=datoFra, datoTil=datoTil, minald=minald,
                               maxald=maxald, erMann=erMann, #elektiv=elektiv,
                               BMI=BMI, tilgang=tilgang, minPRS=minPRS, maxPRS=maxPRS,
                               ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
                               op_gruppe=op_gruppe, ncsp=ncsp, hastegrad_hybrid=hastegrad_hybrid)
RegData <- NorgastUtvalg$RegData
PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=valgtVar)
RegData <- PlotParams$RegData
aldersgr <- sort(RegData$Alder)[round(seq(1, dim(RegData)[1], length.out = 4))]
RegData$aldersgruppe <- cut(RegData$Alder, breaks = aldersgr, include.lowest = T)
ind4 <- RegData[ , c("Aar", "opptaksomr", "erMann", "Variabel", "aldersgruppe")] %>%
  group_by(Aar, opptaksomr, erMann, aldersgruppe) %>% summarise(teller = sum(Variabel),
                                                                nevner = n())

kobling_bosted_hf_v2 <- kobling_bosted_hf[match(unique(kobling_bosted_hf$opptaksomr), kobling_bosted_hf$opptaksomr), ]

ind1 <- merge(ind1, kobling_bosted_hf_v2[, c("opptaksomr", "opptaksomr_navn")], by = "opptaksomr", all.x = T)
ind1 <- ind1[, c(2,7,1,3,4,5,6)]
names(ind1) <- c("aar",	"bohf",	"bohfnr",	"ermann",	"aldersgruppe",	"teller",	"nevner")
ind2 <- merge(ind2, kobling_bosted_hf_v2[, c("opptaksomr", "opptaksomr_navn")], by = "opptaksomr", all.x = T)
ind2 <- ind2[, c(2,7,1,3,4,5,6)]
names(ind2) <- c("aar",	"bohf",	"bohfnr",	"ermann",	"aldersgruppe",	"teller",	"nevner")
ind3 <- merge(ind3, kobling_bosted_hf_v2[, c("opptaksomr", "opptaksomr_navn")], by = "opptaksomr", all.x = T)
ind3 <- ind3[, c(2,7,1,3,4,5,6)]
names(ind3) <- c("aar",	"bohf",	"bohfnr",	"ermann",	"aldersgruppe",	"teller",	"nevner")
ind4 <- merge(ind4, kobling_bosted_hf_v2[, c("opptaksomr", "opptaksomr_navn")], by = "opptaksomr", all.x = T)
ind4 <- ind4[, c(2,7,1,3,4,5,6)]
names(ind4) <- c("aar",	"bohf",	"bohfnr",	"ermann",	"aldersgruppe",	"teller",	"nevner")

write.csv2(ind1, "I:/norgast/kvalatlas_norgast_ind1.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(ind2, "I:/norgast/kvalatlas_norgast_ind2.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(ind3, "I:/norgast/kvalatlas_norgast_ind3.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(ind4, "I:/norgast/kvalatlas_norgast_ind4.csv", row.names = F, fileEncoding = "Latin1")



