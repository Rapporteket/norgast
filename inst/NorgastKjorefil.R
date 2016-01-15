setwd('C:/SVN/jasper/norgast/doc/')
rm(list=ls())

# Les inn data
RegData <- read.table('C:/SVN/jasper/norgast/data/all_variables2015-11-24 09-31-03.txt', header=TRUE, sep=";")
ForlopData <- read.table('C:/SVN/jasper/norgast/data/ForlopsOversikt2015-11-24 09-31-08.txt', header=TRUE, sep=";")

RegData <- RegData[,c('MCEID', 'AvdRESH','Avdeling','BMI_CATEGORY','WEIGHTLOSS','DIABETES','CHEMOTHERAPY_ONLY','RADIATION_THERAPY_ONLY',
                      'CHEMORADIOTHERAPY','WHO_ECOG_SCORE','MODIFIED_GLASGOW_SCORE','ASA','ANESTHESIA_START','NCSP','OPERATION_DATE',
                      'ANASTOMOSIS','OSTOMY','ABDOMINAL_ACCESS','ROBOTASSISTANCE','THORAX_ACCESS','RELAPAROTOMY','RELAPAROTOMY_YES',
                      'ACCORDION_SCORE', 'isMale','decimalAge', 'PRS_SCORE','STATUS', 'READMISSION_STATUS', 'READMISSION_ACCORDION_SCORE',
                      'READMISSION_RELAPAROTOMY', 'READMISSION_RELAPAROTOMY_YES')]

RegData <- merge(RegData, ForlopData, by.x = "MCEID", by.y = "ForlopsID")
RegData$AvdRESH <- RegData$AvdRESH.x

# reshID <- c(708761, 102145, 102143, 102141, 707232, 700922, 700413, 601225, 107440, 108162, 114271, 100100, 4204082, 4204500)
reshID <- 601225 #  #Må sendes med til funksjon
minald <- 0  #alder, fra og med
maxald <- 130	#alder, til og med
erMann <- 99
datoFra <- as.POSIXlt('2014-01-01', format="%Y-%m-%d") 	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- as.POSIXlt('2016-01-01', format="%Y-%m-%d")
enhetsUtvalg <- 1 #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
valgtVar <- 'Alder'
op_gruppe<- ''
outfile <- ''
preprosess<-T
hentData <- F
stabel=F
andel=T

x11()
FigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
           minald=minald, maxald=maxald, erMann=erMann, op_gruppe=op_gruppe, outfile=outfile,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, stabel=stabel, andel=andel,
           preprosess=preprosess, hentData=hentData)


############# Avdød under opphold Mo i Rana ######################

setwd('C:/SVN/jasper/norgast/doc/')
rm(list=ls())

# Les inn data
RegData <- read.table('C:/SVN/jasper/norgast/data/all_variables2015-12-15 10-33-18.txt', header=TRUE, sep=";")
tmp <- read.table('C:/SVN/jasper/norgast/data/AlleVariablerNum2015-12-15 10-14-43.txt', header=TRUE, sep=";")


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
