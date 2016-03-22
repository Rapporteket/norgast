# setwd('C:/GIT/norgast/inst/')
rm(list=ls())

# Les inn data
# RegData <- read.table('C:/SVN/jasper/norgast/data/all_variables2016-02-01 13-05-11.txt', header=TRUE, sep=";", encoding = 'UFT-8')
RegData <- read.table('C:/GIT/norgast/data/AlleVarNum2016-02-01 13-05-05.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('C:/GIT/norgast/data/ForlopsOversikt2016-02-01 13-05-10.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('MCEID','BMI_CATEGORY','WEIGHTLOSS','DIABETES','CHEMOTHERAPY_ONLY','RADIATION_THERAPY_ONLY',
                      'CHEMORADIOTHERAPY','WHO_ECOG_SCORE','MODIFIED_GLASGOW_SCORE','ASA','ANESTHESIA_START','NCSP','OPERATION_DATE',
                      'ANASTOMOSIS','OSTOMY','ABDOMINAL_ACCESS','ROBOTASSISTANCE','THORAX_ACCESS','RELAPAROTOMY','RELAPAROTOMY_YES',
                      'ACCORDION_SCORE', 'PRS_SCORE','STATUS', 'READMISSION_STATUS', 'READMISSION_ACCORDION_SCORE',
                      'READMISSION_RELAPAROTOMY', 'READMISSION_RELAPAROTOMY_YES')]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID')]


RegData <- merge(RegData, ForlopData, by.x = "MCEID", by.y = "ForlopsID")
# RegData$AvdRESH <- RegData$AvdRESH.x

# reshID <- c(708761, 102145, 102143, 102141, 707232, 700922, 700413, 601225, 107440, 108162, 114271, 100100, 4204082, 4204500)
reshID <- 601225 #  #Må sendes med til funksjon
minald <- 0  #alder, fra og med
maxald <- 130	#alder, til og med
erMann <- 99
datoFra <- as.POSIXlt('2014-01-01', format="%Y-%m-%d") 	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- as.POSIXlt('2015-12-31', format="%Y-%m-%d")
enhetsUtvalg <- 1 #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
valgtVar <- 'Op_gr'
op_gruppe<- 0
outfile <- ''
preprosess<-T
hentData <- F
stabel=F
andel=T
elektiv=99
BMI <- c('')  # c('1', '3', '5')
# valgtShus <- c('708761', '102145', '601225')
valgtShus <- c('')
tilgang <- 2
minPRS <- 0
maxPRS <- 2
ASA <- '' # c('1', '3', '5')
whoEcog <- ''  #c('0', '1', '3', '5')
forbehandling <- 3

if (outfile == '') {x11()}
FigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
           minald=minald, maxald=maxald, erMann=erMann, op_gruppe=op_gruppe, outfile=outfile,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, stabel=stabel, andel=andel,
           preprosess=preprosess, hentData=hentData, elektiv = elektiv, BMI = BMI,
           valgtShus = valgtShus, tilgang = tilgang, minPRS=minPRS, maxPRS=maxPRS, ASA=ASA,
           whoEcog=whoEcog, forbehandling=forbehandling)

## Finn avvik mellom "Interaktive andelsdiagrammer" og Tabell 2

Pross <- NorgastPreprosess(RegData)

aux1 <- test[test$OperasjonsDato>=as.POSIXlt('2014-01-01') & test$OperasjonsDato<=as.POSIXlt('2015-12-31'), ]
aux1 <- aux1[aux1$Op_gr==1, ]
aux2 <- aux1[as.numeric(aux1$ANESTHESIA_START) %in% 8:15, ]
aux3 <- aux1[as.numeric(aux1$ANESTHESIA_START) %in% 0:7 | as.numeric(aux1$ANESTHESIA_START) %in% 16:23, ]


### mE-PASS (PRS-score) utenfor range #############

RegData0 <- RegData_old[RegData_old$PRS_SCORE<0 & !is.na(RegData_old$PRS_SCORE), c('decimalAge', 'HEART_DISEASE',
                                                           'LUNG_DISEASE', 'DIABETES', 'WHO_ECOG_SCORE', 'ASA', 'PRS_SCORE')]

RegData1 <- RegData[RegData$PRS_SCORE<0 & !is.na(RegData$PRS_SCORE), c('Sykehusnavn','HovedDato' ,'PasientAlder', 'HEART_DISEASE',
                                                           'LUNG_DISEASE', 'DIABETES', 'WHO_ECOG_SCORE', 'ASA', 'PRS_SCORE')]

RegData2 <- RegData[RegData$PRS_SCORE>1.1 & !is.na(RegData$PRS_SCORE), c('Sykehusnavn','HovedDato' ,'PasientAlder', 'HEART_DISEASE',
                                                           'LUNG_DISEASE', 'DIABETES', 'WHO_ECOG_SCORE', 'ASA', 'PRS_SCORE')]


-0.0686 +0.00345*RegData1$PasientAlder + 0.323*RegData1$HEART_DISEASE + 0.205*RegData1$LUNG_DISEASE + 0.153*RegData1$DIABETES +
  0.148*RegData1$WHO_ECOG_SCORE + 0.0666*RegData1$ASA


-0.0686 +0.00345*RegData2$PasientAlder + 0.323*RegData2$HEART_DISEASE + 0.205*RegData2$LUNG_DISEASE + 0.153*RegData2$DIABETES +
  0.148*RegData2$WHO_ECOG_SCORE + 0.0666*RegData2$ASA

RegData <- RegData[!is.na(RegData$PRS_SCORE), ]

tmp <- RegData$PRS_SCORE - (-0.0686 +0.00345*RegData$PasientAlder + 0.323*RegData$HEART_DISEASE + 0.205*RegData$LUNG_DISEASE +
                    0.153*RegData$DIABETES + 0.148*RegData$WHO_ECOG_SCORE + 0.0666*RegData$ASA)


-0.0686 +0.00345*130 + 0.323*1 + 0.205*1 + 0.153*1 +0.148*4 + 0.0666*4

############# Avdød under opphold Mo i Rana ######################

setwd('C:/SVN/jasper/norgast/doc/')
rm(list=ls())

# Les inn data
# RegData <- read.table('C:/SVN/jasper/norgast/data/all_variables2015-12-15 10-33-18.txt', header=TRUE, sep=";")
tmp1 <- read.table('C:/SVN/jasper/norgast/data/AlleVarNum2016-01-22 09-41-57.txt', header=TRUE, sep=";")
tmp2 <- read.table('C:/SVN/jasper/norgast/data/AlleVariablerNum2016-01-22 09-42-00.txt', header=TRUE, sep=";")
tmp3 <- read.table('C:/SVN/jasper/norgast/data/ForlopsOversikt2016-01-22 09-42-02.txt', header=TRUE, sep=";")

# tmp3 <- read.table('C:/SVN/jasper/norgast/data/AlleVariablerNum2015-12-15 10-14-43.txt', header=TRUE, sep=";")


DoedData <- RegData[intersect(which(RegData$AvdRESH==102141), which(RegData$IN_HOUSE_DEATH==1 | RegData$READMISSION_IN_HOUSE_DEATH == 1)),]

IDer <- RegData$MCEID[intersect(which(RegData$AvdRESH==102141), which(RegData$IN_HOUSE_DEATH==1 | RegData$READMISSION_IN_HOUSE_DEATH == 1))]


## Lag kobling til Sykehusnavn #####################


#
#
# RegData <- RegData[which(RegData$OperasjonsDato>=datoFra & RegData$OperasjonsDato<=datoTil),]
# RegData <- RegData[which(RegData$Op_gr==op_gruppe),]
#
#
# RegData2 <- RegData[which(RegData$ABDOMINAL_ACCESS==2),]
# RegData2 <- RegData[which(RegData$ANESTHESIA_START>=7 & RegData$ANESTHESIA_START<=15),]
# RegData2 <- RegData[which(RegData$ANESTHESIA_START<7 | RegData$ANESTHESIA_START>15),]
#
# vekttap <- RegData2$WEIGHTLOSS[!is.na(RegData2$WEIGHTLOSS)]
# asa <- RegData2$ASA[!is.na(RegData2$ASA)]
#
# mean(vekttap)
# median(vekttap)
# mean(asa)
# median(asa)
#
#
# # RegData<-RegData[RegData$Op_gr==op_gruppe,]
# reshID <- unique(RegData$AvdRESH[RegData$Op_gr==op_gruppe])
# op_gr<-as.character(RegData$Operasjonsgrupper[match(op_gruppe, RegData$Op_gr)])
#
# for (p in reshID){
# #   x11()
# #   print(p)
#   sh<-as.character(RegData$SykehusNavn[match(p, RegData$AvdRESH)])
#   outfile <- paste0(valgtVar, op_gr, sh, '.pdf')
#   print(outfile)
#
#   FigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
#    minald=minald, maxald=maxald, erMann=erMann, op_gruppe=op_gruppe, libkat, outfile=outfile,
#    reshID=p, enhetsUtvalg=enhetsUtvalg, stabel=F, andel=T, preprosess=preprosess)
# }
#
#
#
#
#
#
#
#
# # RegData$OperasjonsDato<-as.POSIXlt(as.character(RegData$OperasjonsDato), format="%Y-%m-%d")
# # RegData<-RegData[RegData$OperasjonsDato>as.POSIXlt('2014-01-01'),]
# # RegData<- RegData[RegData$Op_gr==1,]
# #
# #
# # sum(RegData$ANASTOMOSIS)
# # sum(RegData$OSTOMY[RegData$ANASTOMOSIS==1])
# #
# # RegData2 <- RegData[RegData$ANASTOMOSIS==0,]
# # sum(RegData2$OSTOMY)
#
#
#
#
#
#
#
#
#
