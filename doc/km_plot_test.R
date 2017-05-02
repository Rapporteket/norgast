setwd('C:/GIT/norgast/doc/')
library(norgast)
rm(list=ls())

# Les inn data
RegData <- read.table('C:/SVN/jasper/norgast/data/AlleVariablerNum2016-11-30 08-20-39.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# RegData2 <- read.table('C:/SVN/jasper/norgast/data/AlleVar2016-10-11 09-34-46.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('C:/SVN/jasper/norgast/data/ForlopsOversikt2016-11-30 08-20-42.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori', 'BMI', 'VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'PostopLiggedogn', "Hoveddiagnose")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]

RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData=RegData)


dummyData <- RegData[!(RegData$Avdod == 1 & is.na(RegData$OpDoedTid)), ]
dummyData <- dummyData[order(dummyData$HovedDato), ]
dummyData <- dummyData[match(unique(dummyData$PasientID), dummyData$PasientID), ]

# dummyData$OpDoedTid[dummyData$Avdod==1]
dummyData$overlev <- difftime(as.POSIXlt(Sys.Date()), dummyData$OperasjonsDato, units = 'days')
dummyData$overlev[dummyData$Avdod==1] <- dummyData$OpDoedTid[dummyData$Avdod==1]
dummyData$overlev <- as.numeric(dummyData$overlev)

dummyData$SurvObj <- with(dummyData, Surv(overlev, Avdod == 1))

km.as.one <- survfit(SurvObj ~ 1, data = dummyData, conf.type = "log-log")


x11()
plot(km.as.one)

x11()
plot(km.as.one$time[1:200], km.as.one$surv[1:200])




