setwd('C:/GIT/norgast/doc/')
library(norgast)
rm(list=ls())

### Linn Nymo/Kristin Woll - 07.05.2018 ###############################################################
library(norgast)
rm(list = ls())

RegData <- read.table('I:/norgast/AlleVariablerNum2018-04-24 12-12-33.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-04-24 12-12-43.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)

rap_aar <- 2017 # Året rapporten skal kjøres for
datoFra= paste0(rap_aar, '-01-01')
datoTil= paste0(rap_aar, '-12-31')

RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData <- RegData[RegData$Aar==rap_aar, ]
RegData <- RegData[RegData$Sykehusnavn=='UNN-Tromsø' & RegData$Op_gr==7, ]

write.csv2(RegData[, c("PasientID", "ForlopsID", "OpDato", "Hovedoperasjon")], 'AndrePankreasUNN2017.csv', row.names = F)

### Linn Nymo, avdøde Skien og Gjøvik - 07.05.2018 ###############################################################
library(norgast)
rm(list = ls())

RegData <- read.table('I:/norgast/AlleVariablerNum2018-04-24 12-12-33.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-04-24 12-12-43.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)

rap_aar <- 2017 # Året rapporten skal kjøres for
datoFra= paste0(rap_aar, '-01-01')
datoTil= paste0(rap_aar, '-12-31')

RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData <- RegData[RegData$Aar==rap_aar, ]
RegData <- RegData[RegData$Sykehusnavn %in% c('Skien', 'SI-Gjøvik') & RegData$Op_gr==1, ]
RegData <- RegData[RegData$Hastegrad == 1 & RegData$Malign==0, ]


### Stig Norderval - 13.04.2018 ###############################################################
library(norgast)
rm(list = ls())

RegData <- read.table('I:/norgast/AlleVariablerNum2018-04-13 08-48-49.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-04-13 08-48-58.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', "Hoveddiagnose")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)

robot <- RegData[RegData$Op_gr==2 & RegData$Malign == 1 & RegData$Tilgang %in% c(2,3) & RegData$Robotassistanse==1, ]
table(robot$Tilgang)


### Hent tall til nasjonal rapport over dekningsgrad og indikatorer ########################################
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
rm(ForlopData)
RegData <- NorgastPreprosess(RegData=RegData)
RegData <- RegData[RegData$Aar == 2016, ]
RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, 'UTF-8', '')


kolon <- data.frame(AvdRESH=c(4204082, 108357, 700922,114271, 4204500, 102141, 601231, 601225),
                    Sykehusnavn=c('Drammen sykehus', 'Sykehuset Innlandet Lillehamme', 'Haukeland Universitetssykehus',
                                  'Helse Stavanger HF', 'Molde sjukehus', 'Rana sykehus', 'UNN Harstad', 'UNN Tromsø'))

lever <- data.frame(AvdRESH=c(700413, 700922, 114271, 107440, 601225),
                    Sykehusnavn=c('Oslo universitetssykehus HF', 'Haukeland Universitetssykehus', 'Helse Stavanger HF',
                                  'St. Olavs Hospital HF', 'UNN Tromsø'))

pankreas <- lever

RegData$Op_gr[RegData$Op_gr==7] <- 6
RegData <- RegData[RegData$Op_gr %in% c(1, 5, 6), ]

x11()
tmp <- NorgastFigAndelStabelGrVar(RegData=RegData[RegData$Op_gr==1, ], valgtVar='AccordionGrad', elektiv=1, outfile='', reshID=601225, Ngrense=10)
aux<-substr(tmp$N, 3,6)
aux[substr(aux,1,1)=='<']<-''
aux<-as.numeric(aux)

kolondata <- data.frame(shus=tmp$shus, andel=c(colSums(tmp$andeler),NA), N=aux)
kolondata$AvdRESH <- RegData$AvdRESH[match(kolondata$shus, RegData$Sykehusnavn)]
kolondata <- kolondata[kolondata$AvdRESH %in% kolon$AvdRESH, ]
kolondata$andel <- round(kolondata$andel, 1)

x11()
tmp <- NorgastFigAndelStabelGrVar(RegData=RegData[RegData$Op_gr==5, ], valgtVar='AccordionGrad', outfile='', reshID=601225, Ngrense=10)
aux<-substr(tmp$N, 3,6)
aux[substr(aux,1,1)=='<']<-''
aux<-as.numeric(aux)

leverdata <- data.frame(shus=tmp$shus, andel=c(colSums(tmp$andeler),NA), N=aux)
leverdata$AvdRESH <- RegData$AvdRESH[match(leverdata$shus, RegData$Sykehusnavn)]
leverdata <- leverdata[leverdata$AvdRESH %in% lever$AvdRESH, ]
leverdata$andel <- round(leverdata$andel, 1)

x11()
tmp <- NorgastFigAndelStabelGrVar(RegData=RegData[RegData$Op_gr==6, ], valgtVar='AccordionGrad', outfile='', reshID=601225, Ngrense=10)
aux<-substr(tmp$N, 3,6)
aux[substr(aux,1,1)=='<']<-''
aux<-as.numeric(aux)

pankreasdata <- data.frame(shus=tmp$shus, andel=c(colSums(tmp$andeler),NA), N=aux)
pankreasdata$AvdRESH <- RegData$AvdRESH[match(pankreasdata$shus, RegData$Sykehusnavn)]
pankreasdata <- pankreasdata[pankreasdata$AvdRESH %in% pankreas$AvdRESH, ]
pankreasdata$andel <- round(pankreasdata$andel, 1)

kolondata[,c(1,3,2)]
leverdata[,c(1,3,2)]
pankreasdata[,c(1,3,2)]

### PID-liste Vestfold - 55 pasienter til valideringsprosjekt ########################################
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
RegData <- RegData[RegData$Aar == 2016 & RegData$Sykehusnavn == 'Sykehuset i Vestfold HF', ]

set.seed(78239955)
pidlist <- sample(RegData$PasientID, 55)
write.csv2(pidlist, 'PidValideringNoRGastVestfold.csv', row.names = F)


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












