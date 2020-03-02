setwd('C:/GIT/norgast/doc/')
library(norgast)
library(survival)
library(survminer)
library(dplyr)
rm(list=ls())

# Les inn data
RegData <- read.table('I:/norgast/AlleVarNum2020-01-13 13-43-56.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# RegData2 <- read.table('C:/SVN/jasper/norgast/data/AlleVar2016-10-11 09-34-46.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast//ForlopsOversikt2020-01-13 13-44-23.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori', 'BMI', 'VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'PostopLiggedogn', "Hoveddiagnose")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]

RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData=RegData)
RegData <- RegData[-which(RegData$OpDoedTid<0), ]



dummyData <- RegData[!(RegData$Avdod == 1 & is.na(RegData$OpDoedTid)), ]
dummyData <- dummyData[order(dummyData$HovedDato, decreasing = T), ]
dummyData <- dummyData[match(unique(dummyData$PasientID), dummyData$PasientID), ]

# dummyData$OpDoedTid[dummyData$Avdod==1]
dummyData$overlev <- difftime(as.Date(Sys.Date()), dummyData$OperasjonsDato, units = 'days')
dummyData$overlev[dummyData$Avdod==1] <- dummyData$OpDoedTid[dummyData$Avdod==1]
dummyData$overlev <- as.numeric(dummyData$overlev)
dummyData$SurvObj <- with(dummyData, Surv(overlev, Avdod == 1))

km.as.one <- survfit(SurvObj ~ 1, data = dummyData, conf.type = "log-log")


fit1 <- survfit(SurvObj ~ erMann, data = dummyData)
x11()
ggsurvplot(fit1, data = dummyData, title='test', pval = TRUE, conf.int = F)


