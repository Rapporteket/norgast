setwd('C:/GIT/norgast/doc/')
library(norgast)
rm(list=ls())


### Forespørsel fra Lone 31.10.2017 ######################################

# Les inn data
RegData <- read.table('P:/MinData/norgast/AlleVariablerNum2017-10-31 12-12-24.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('P:/MinData/norgast/ForlopsOversikt2017-10-31 12-12-22.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori', 'BMI', 'VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'PostopLiggedogn', "Hoveddiagnose")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]

RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData=RegData)


RegData <- RegData[RegData$HovedDato >= '2016-01-01', ] # Fra 2016
RegData <- RegData[substr(RegData$ncsp_lowercase, 1, 5) %in% c('jlc10', 'jlc11'), ] # Distale pankreas
RegData <- RegData[RegData$Sykehusnavn == 'St. Olavs Hospital HF', ]


table(RegData$Aar, RegData$Kvartal, useNA = 'ifany')












