# setwd('C:/GIT/norgast/inst/')
rm(list=ls())

# Les inn data
RegData <- read.table('C:/SVN/jasper/norgast/data/AlleVarNum2016-03-22 08-21-59.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('C:/SVN/jasper/norgast/data/ForlopsOversikt2016-03-22 08-22-02.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('MCEID','BMI_CATEGORY','WEIGHTLOSS','DIABETES','CHEMOTHERAPY_ONLY','RADIATION_THERAPY_ONLY',
                      'CHEMORADIOTHERAPY','WHO_ECOG_SCORE','MODIFIED_GLASGOW_SCORE','ASA','ANESTHESIA_START','NCSP','OPERATION_DATE',
                      'ANASTOMOSIS','OSTOMY','ABDOMINAL_ACCESS','ROBOTASSISTANCE','THORAX_ACCESS','RELAPAROTOMY','RELAPAROTOMY_YES',
                      'ACCORDION_SCORE', 'PRS_SCORE','STATUS', 'READMISSION_STATUS', 'READMISSION_ACCORDION_SCORE',
                      'READMISSION_RELAPAROTOMY', 'READMISSION_RELAPAROTOMY_YES', 'DECEASED', 'DECEASED_DATE')]
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
valgtVar <- 'DECEASED'
op_gruppe<- 0
outfile <- ''
preprosess<-T
hentData <- F
stabel=F
# andel=T
elektiv=99
BMI <- c('')  # c('1', '3', '5')
# valgtShus <- c('708761', '102145', '601225')
valgtShus <- c('')
tilgang <- 99
minPRS <- 0
maxPRS <- 2
ASA <- '' # c('1', '3', '5')
whoEcog <- ''  #c('0', '1', '3', '5')
forbehandling <- 99

if (outfile == '') {x11()}
FigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
           minald=minald, maxald=maxald, erMann=erMann, op_gruppe=op_gruppe, outfile=outfile,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, stabel=stabel,
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

RegData$PRS_SCORE_minutregning <- (-0.0686 +0.00345*RegData$PasientAlder + 0.323*RegData$HEART_DISEASE + 0.205*RegData$LUNG_DISEASE +
                    0.153*RegData$DIABETES + 0.148*RegData$WHO_ECOG_SCORE + 0.0666*RegData$ASA)


tmp <- RegData[,c('MCEID', 'PRS_SCORE', 'PRS_SCORE_minutregning')]
tmp$PRS_SCORE_minutregning[RegData$WHO_ECOG_SCORE==9] <- NA

forskjell <- tmp$PRS_SCORE - tmp$PRS_SCORE_minutregning

indUlik <- which(abs(forskjell)>10^(-12) | (is.na(tmp$PRS_SCORE) & !is.na(tmp$PRS_SCORE_minutregning)) |
                   (!is.na(tmp$PRS_SCORE) & is.na(tmp$PRS_SCORE_minutregning)) )
avvik <- tmp[indUlik,]

setwd('C:/SVN/jasper/norgast/doc/')
write.csv2(avvik, 'avvik.csv', row.names = F)

prs<-read.table('C:/SVN/jasper/norgast/doc/Rescoring_PRS_score.csv', header=TRUE, sep=";", encoding = 'UFT-8')
prs$Endring <- as.character(prs$Endring)
prs_forskj <- prs[substr(prs$Endring, 1, 9)!='No change', ]
prs_forskj$Endring<-gsub('.*=([0-9]+).*','\\1', prs_forskj$Endring)
names(prs_forskj)[1] <- 'MCEID'
prs_forskj$MCEID <- as.numeric(prs_forskj$MCEID)
prs_forskj$PRS_GAMMEL <- as.numeric(as.character(prs_forskj$PRS_GAMMEL))
prs_forskj$PRS_NY <- as.numeric(as.character(prs_forskj$PRS_NY))


avvik[avvik$MCEID %in% setdiff(avvik$MCEID, prs_forskj$MCEID), ]
prs_forskj[prs_forskj$MCEID %in% setdiff(prs_forskj$MCEID, avvik$MCEID), ]


prsTorkil<-read.table('C:/SVN/jasper/norgast/doc/Rescoring_PRS_score Torkil.csv', header=TRUE, sep=",", encoding = 'UFT-8')
prsTorkil$NEW_PRSCORE <- as.numeric(as.character(prsTorkil$NEW_PRSCORE))
prsTorkil$OLD_PRS_SCORE <- as.numeric(as.character(prsTorkil$OLD_PRS_SCORE))

forskjell2 <- prsTorkil$OLD_PRS_SCORE - prsTorkil$NEW_PRSCORE

indUlik <- which(abs(forskjell2)>10^(-12) | (is.na(prsTorkil$OLD_PRS_SCORE) & !is.na(prsTorkil$NEW_PRSCORE)) |
                   (!is.na(prsTorkil$OLD_PRS_SCORE) & is.na(prsTorkil$NEW_PRSCORE)) )
avvik2 <- prsTorkil[indUlik,]

setdiff(avvik$MCEID, avvik2$MCEID)
setdiff(avvik2$MCEID, avvik$MCEID)

RegData[RegData$MCEID %in% c(setdiff(avvik$MCEID, avvik2$MCEID), setdiff(avvik2$MCEID, avvik$MCEID)),
        c('MCEID', 'PRS_SCORE', 'PRS_SCORE_minutregning')]
prsTorkil[prsTorkil$MCEID %in% c(setdiff(avvik$MCEID, avvik2$MCEID), setdiff(avvik2$MCEID, avvik$MCEID)), ]




Endelig <- merge(tmp, prsTorkil, by = 'MCEID')

forskjell3 <- Endelig$PRS_SCORE_minutregning - Endelig$NEW_PRSCORE
indUlik <- which(abs(forskjell3)>10^(-12) | (is.na(Endelig$NEW_PRSCORE) & !is.na(Endelig$PRS_SCORE_minutregning)) |
                   (!is.na(Endelig$NEW_PRSCORE) & is.na(Endelig$PRS_SCORE_minutregning)) )

avvik <- Endelig[indUlik, ]

setwd('C:/SVN/jasper/norgast/doc/')
write.csv2(avvik[, c('MCEID', 'NEW_PRSCORE', 'PRS_SCORE_minutregning')], 'avvik.csv', row.names = F)


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
