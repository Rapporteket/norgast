setwd('C:/GIT/norgast/doc/')
library(norgast)
rm(list=ls())

# Les inn data
RegData <- read.table('I:/norgast/AlleVariablerNum2018-03-19 08-21-23.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# aux <- read.table('C:/SVN/jasper/norgast/data/AlleVar2017-03-09 13-09-16.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-03-19 08-21-33.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori', 'BMI', 'VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'PostopLiggedogn', "Hoveddiagnose")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]

RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData=RegData)

tmp <- substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr==1])), 1, 5)
# tmp <- tmp[-(5:15)]
ncsp <- '' #tmp
ny=c('(JFB[2-5][0-9]|JFB6[0-4])|JFH', 'JGB', 'JCC', 'JDC|JDD', 'JJB', 'JLC30|JLC31',
     'JLC[0-2][0-9]|JLC[4-9][0-9]|JLC[3][2-9]', 'JKA21|JKA20', 'JEA00|JEA01',
     'JFB00|JFB01', 'JDF10|JDF11', 'JDF96|JDF97')
reseksjonsGr <-  '' #ny[1]

# reshID <- c(708761, 102145, 102143, 102141, 707232, 700922, 700413, 601225, 107440, 108162, 114271, 100100, 4204082, 4204500)
reshID <- 601225 #  #Må sendes med til funksjon
minald <- 0  #alder, fra og med
maxald <- 130	#alder, til og med
erMann <- 99
datoFra <- '2016-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2020-01-01'
enhetsUtvalg <- 1       #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
valgtVar <- 'Saarruptur'
# valgtVar <- 'Malign'
valgtVar <- 'Alder'
outfile <- ''
# outfile <- paste0(valgtVar, '.pdf')
preprosess<-F
hentData <- F
stabel=F
# andel=T
elektiv=99
BMI <- c('')  # c('1', '3', '5')
valgtShus <- c('708761', '102145', '601225')
# valgtShus <- c('')
tilgang <- 1
minPRS <- 0
maxPRS <- 2
ASA <- '' # c('1', '3', '5')
whoEcog <- ''  #c('0', '1', '3', '5')
forbehandling <- 99
tidsenhet <- 'Halvaar'
inkl_konf <- 1
malign <- 99


if (outfile == '') {x11()}
FigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
           minald=minald, maxald=maxald, erMann=erMann, outfile=outfile,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, stabel=stabel,
           preprosess=preprosess, hentData=hentData, elektiv = elektiv, BMI = BMI,
           valgtShus = valgtShus, tilgang = tilgang, minPRS=minPRS, maxPRS=maxPRS, ASA=ASA,
           whoEcog=whoEcog, forbehandling=forbehandling, malign=malign, reseksjonsGr=reseksjonsGr, ncsp=ncsp)

valgtVar <- 'ReLapNarkose'
outfile <- 'ReLapNarkose.pdf'
if (outfile == '') {x11()}
NorgastFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
           minald=minald, maxald=maxald, erMann=erMann, outfile=outfile,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, inkl_konf=inkl_konf,
           preprosess=preprosess, hentData=hentData, elektiv = elektiv, BMI = BMI,
           valgtShus = valgtShus, tilgang = tilgang, minPRS=minPRS, maxPRS=maxPRS, ASA=ASA,
           whoEcog=whoEcog, forbehandling=forbehandling, tidsenhet=tidsenhet, malign=malign, reseksjonsGr=reseksjonsGr, ncsp=ncsp)


if (outfile == '') {x11()}
NorgastFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                       minald=minald, maxald=maxald, erMann=erMann, outfile=outfile,
                       reshID=reshID, inkl_konf=inkl_konf,
                       preprosess=preprosess, hentData=hentData, elektiv = elektiv, BMI = BMI,
                       valgtShus = valgtShus, tilgang = tilgang, minPRS=minPRS, maxPRS=maxPRS, ASA=ASA,
                       whoEcog=whoEcog, forbehandling=forbehandling, malign=malign, reseksjonsGr=reseksjonsGr, ncsp=ncsp)

if (outfile == '') {x11()}
NorgastFigGjsnGrVar(RegData=RegData, valgtVar='PRSScore', datoFra=datoFra, datoTil=datoTil,
                    minald=minald, maxald=maxald, erMann=erMann, outfile=outfile,
                    reshID=reshID, preprosess=preprosess, malign=malign,
                    elektiv=elektiv, BMI=BMI, tilgang=tilgang, valgtShus=valgtShus, minPRS=minPRS,
                    maxPRS=maxPRS, ASA=ASA, whoEcog= whoEcog, forbehandling=forbehandling,
                    hentData=hentData, reseksjonsGr=reseksjonsGr, ncsp=ncsp)

if (outfile == '') {x11()}
NorgastFigAndelStabelGrVar(RegData=RegData, valgtVar='AccordionGrad', datoFra=datoFra, datoTil=datoTil,
                           minald=minald, maxald=maxald, erMann=erMann, outfile=outfile,
                           reshID=reshID, preprosess=preprosess, malign=malign,
                           elektiv=elektiv, BMI=BMI, tilgang=tilgang, valgtShus=valgtShus, minPRS=minPRS,
                           maxPRS=maxPRS, ASA=ASA, whoEcog= whoEcog, forbehandling=forbehandling,
                           hentData=hentData, reseksjonsGr=reseksjonsGr, ncsp=ncsp)


if (outfile == '') {x11()}
NorgastFigAndelStabelKunNasjonal(RegData, valgtVar='AccordionGrad', elektiv=0, outfile=outfile,
                                 reshID=reshID, reseksjonsGr='(JFB[2-5][0-9]|JFB6[0-4])|JFH')


# NorgastFigAndelTid(RegData=RegData, valgtVar=valgtVar, reshID=reshID, preprosess=preprosess)


## Liste over kolonreseksjoner i NoRGast 2015 #############

regdata <- NorgastPreprosess(RegData)

regdata <- regdata[regdata$Aar==2015 & regdata$Op_gr==1 & regdata$AvdRESH==601225, c("PasientID", "ForlopsID")]
write.csv2(regdata, 'NoRGastKolonTromso2015.csv', row.names = F)

## Liste over elektive kolonreseksjoner 2014-2015 med accordion-grad  3 eller høyere#############
Pasientliste <- RegData$PasientID[which(RegData$Op_gr==1 & RegData$AvdRESH==601225 & RegData$Aar %in% c(2014,2015) &
                                          RegData$Hastegrad == 1 & RegData$AccordionGrad >=3)]

Forlopsliste <- RegData$ForlopsID[which(RegData$Op_gr==1 & RegData$AvdRESH==601225 & RegData$Aar %in% c(2014,2015) &
                                          RegData$Hastegrad == 1 & RegData$AccordionGrad >=3)]


################ Lag liste for NPR ######################


# RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jlc")] <- "Pankreasreseksjoner"
# RegData$Operasjonsgrupper[intersect(which(substr(RegData$ncsp_lowercase,1,3)=="jlc"),
#                                     which(as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% 0:20))] <- "Andre Pancreasreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jlc" & (as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% 0:20 |
                                                     as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% 40:99))] <- "Andre Pancreasreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jhc" & (as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% 10:99))] <-
  "Gallegangsreseksjoner"

setdiff(which(substr(RegData$ncsp_lowercase,1,3)=="jlc"), which(RegData$Operasjonsgrupper %in% c("Whipples operasjon", "Andre Pancreasreseksjoner")))
setdiff(which(RegData$Operasjonsgrupper %in% c("Whipples operasjon", "Andre Pancreasreseksjoner")), which(substr(RegData$ncsp_lowercase,1,3)=="jlc"))

RegData2014 <- RegData[RegData$Aar == 2014, ]
RegData2015 <- RegData[RegData$Aar == 2015, ]
NoRGastObligOperasjoner2014 <- table(RegData2014$Sykehusnavn, RegData2014$Operasjonsgrupper, useNA = 'ifany')
NoRGastObligOperasjoner2015 <- table(RegData2015$Sykehusnavn, RegData2015$Operasjonsgrupper, useNA = 'ifany')
NoRGastObligOperasjoner2014RESH <- table(RegData2014$AvdRESH, RegData2014$Operasjonsgrupper, useNA = 'ifany')
NoRGastObligOperasjoner2015RESH <- table(RegData2015$AvdRESH, RegData2015$Operasjonsgrupper, useNA = 'ifany')

write.csv2(NoRGastObligOperasjoner2014, 'NoRGastObligOperasjoner2014.csv', row.names = TRUE)
write.csv2(NoRGastObligOperasjoner2015, 'NoRGastObligOperasjoner2015.csv', row.names = TRUE)
write.csv2(NoRGastObligOperasjoner2014RESH, 'NoRGastObligOperasjoner2014RESH.csv', row.names = TRUE)
write.csv2(NoRGastObligOperasjoner2015RESH, 'NoRGastObligOperasjoner2015RESH.csv', row.names = TRUE)

####### Liste NPR 2017-06-28  ##########################################################
RegData$Sykehusnavn <- as.character(RegData$Sykehusnavn)
RegData$Operasjonsgrupper[RegData$Op_gr %in% 6:7] <- 'Pankreasreseksjoner'
RegData$Operasjonsgrupper[RegData$Op_gr > 7] <- 'Annet'
RegData2014 <- RegData[RegData$Aar == 2014, ]
RegData2015 <- RegData[RegData$Aar == 2015, ]
RegData2016 <- RegData[RegData$Aar == 2016, ]

NoRGastObligOperasjoner2014 <- as.data.frame(addmargins(table(RegData2014[, c('Sykehusnavn', 'Operasjonsgrupper')], useNA = 'ifany')))
NoRGastObligOperasjoner2014 <- tidyr::spread(data = NoRGastObligOperasjoner2014,key = Operasjonsgrupper, value = Freq)
NoRGastObligOperasjoner2014$AvdRESH <- RegData$AvdRESH[match(NoRGastObligOperasjoner2014$Sykehusnavn, RegData$Sykehusnavn)]
NoRGastObligOperasjoner2014 <- NoRGastObligOperasjoner2014[, c(10,1,3:8,2,9)]
NoRGastObligOperasjoner2015 <- as.data.frame(addmargins(table(RegData2015[, c('Sykehusnavn', 'Operasjonsgrupper')], useNA = 'ifany')))
NoRGastObligOperasjoner2015 <- tidyr::spread(data = NoRGastObligOperasjoner2015,key = Operasjonsgrupper, value = Freq)
NoRGastObligOperasjoner2015$AvdRESH <- RegData$AvdRESH[match(NoRGastObligOperasjoner2015$Sykehusnavn, RegData$Sykehusnavn)]
NoRGastObligOperasjoner2015 <- NoRGastObligOperasjoner2015[, c(10,1,3:8,2,9)]
NoRGastObligOperasjoner2016 <- as.data.frame(addmargins(table(RegData2016[, c('Sykehusnavn', 'Operasjonsgrupper')], useNA = 'ifany')))
NoRGastObligOperasjoner2016 <- tidyr::spread(data = NoRGastObligOperasjoner2016,key = Operasjonsgrupper, value = Freq)
NoRGastObligOperasjoner2016$AvdRESH <- RegData$AvdRESH[match(NoRGastObligOperasjoner2016$Sykehusnavn, RegData$Sykehusnavn)]
NoRGastObligOperasjoner2016 <- NoRGastObligOperasjoner2016[, c(10,1,3:8,2,9)]

write.csv2(NoRGastObligOperasjoner2014, 'NPRtall2014.csv', row.names = FALSE)
write.csv2(NoRGastObligOperasjoner2015, 'NPRtall2015.csv', row.names = FALSE)
write.csv2(NoRGastObligOperasjoner2016, 'NPRtall2016.csv', row.names = FALSE)


####  Hent øsofagus for Tromsø

tosdata <- NorgastPreprosess(RegData)

tosdata <- tosdata[tosdata$AvdRESH==reshID & tosdata$Op_gr == 3, ]


tosdata$ForlopsID[tosdata$Op_gr==6 & tosdata$NyAnastomose==0]




### Hent ut pasient id for alle pasienter operert i Tromsø 29.09.15-25.02.16 som har accordion 3 eller mer.

RegData <- NorgastPreprosess(RegData=RegData)

# Uttrekk <- RegData[RegData$AccordionGrad >= 3 & RegData$AvdRESH == 601225, c('PasientID', 'OperasjonsDato', 'Sykehusnavn')]

Uttrekk <- RegData[which((RegData$AccordionGrad >= 3 | RegData$Avdod ==1 | RegData$ReLapNarkose == 1 |
                            RegData$ReinnlAndreInst == 1 | RegData$ReinnlEgenInst == 1) &
                           RegData$AvdRESH == 601225), c('PasientID', 'OperasjonsDato', 'Sykehusnavn')]

Uttrekk <- Uttrekk[Uttrekk$OperasjonsDato >= as.POSIXlt('2015-09-29') & Uttrekk$OperasjonsDato <= as.POSIXlt('2016-02-25'), ]
sort(table(Uttrekk$PasientID, useNA = 'ifany'), decreasing = TRUE)

tmp1 <- as.numeric(names(table(Uttrekk$PasientID, useNA = 'ifany')))

RegData[RegData$PasientID %in% setdiff(tmp2, tmp1) & RegData$OperasjonsDato >= as.POSIXlt('2015-09-29') &
          RegData$OperasjonsDato <= as.POSIXlt('2016-02-25'), c('PasientID', 'AccordionGrad', 'Avdod', 'ReLapNarkose',
                                                      'ReinnlAndreInst', 'ReinnlEgenInst', 'OppfStatus')]



RegData$Avdod[which((RegData$AccordionGrad >= 3 | RegData$Avdod ==1 | RegData$ReLapNarkose == 1 |
                           RegData$ReinnlAndreInst == 1 | RegData$ReinnlEgenInst == 1) &
                          RegData$AvdRESH == 601225)]



## Studerer avdøde #########################
tmp <- RegData$PasientID[RegData$Avdod == 1]

tmp <- sort(table(tmp), decreasing = T)
tmp <- tmp[tmp>1]

multiIDer <- as.numeric(names(tmp))

tellehjelp <- RegData[RegData$PasientID %in% multiIDer, c('Avdod', 'AvdodDato', 'PasientID', 'OperasjonsDato', 'ForlopsID')]
tellehjelp <- tellehjelp[order(tellehjelp$PasientID), ]

maxidato <- tapply(tellehjelp$OperasjonsDato, tellehjelp$PasientID, max)

tapply(tellehjelp$ForlopsID, tellehjelp$PasientID, max)




## Finn avvik mellom "Interaktive andelsdiagrammer" og Tabell 2

tmpdata <- NorgastPreprosess(RegData)

aux1 <- test[test$OperasjonsDato>=as.POSIXlt('2014-01-01') & test$OperasjonsDato<=as.POSIXlt('2015-12-31'), ]
aux1 <- aux1[aux1$Op_gr==1, ]
aux2 <- aux1[as.numeric(aux1$AnestesiStartKl) %in% 8:15, ]
aux3 <- aux1[as.numeric(aux1$AnestesiStartKl) %in% 0:7 | as.numeric(aux1$AnestesiStartKl) %in% 16:23, ]


### mE-PASS (PRS-score) utenfor range #############

RegData0 <- RegData_old[RegData_old$PRSScore<0 & !is.na(RegData_old$PRSScore), c('decimalAge', 'Hjertesykdom',
                                                           'Lungesykdom', 'MedDiabetes', 'WHOECOG', 'ASA', 'PRSScore')]

RegData1 <- RegData[RegData$PRSScore<0 & !is.na(RegData$PRSScore), c('Sykehusnavn','HovedDato' ,'PasientAlder', 'Hjertesykdom',
                                                           'Lungesykdom', 'MedDiabetes', 'WHOECOG', 'ASA', 'PRSScore')]

RegData2 <- RegData[RegData$PRSScore>1.1 & !is.na(RegData$PRSScore), c('Sykehusnavn','HovedDato' ,'PasientAlder', 'Hjertesykdom',
                                                           'Lungesykdom', 'MedDiabetes', 'WHOECOG', 'ASA', 'PRSScore')]


-0.0686 +0.00345*RegData1$PasientAlder + 0.323*RegData1$Hjertesykdom + 0.205*RegData1$Lungesykdom + 0.153*RegData1$MedDiabetes +
  0.148*RegData1$WHOECOG + 0.0666*RegData1$ASA


-0.0686 +0.00345*RegData2$PasientAlder + 0.323*RegData2$Hjertesykdom + 0.205*RegData2$Lungesykdom + 0.153*RegData2$MedDiabetes +
  0.148*RegData2$WHOECOG + 0.0666*RegData2$ASA

RegData <- RegData[!is.na(RegData$PRSScore), ]

RegData$PRSScore_minutregning <- (-0.0686 +0.00345*RegData$PasientAlder + 0.323*RegData$Hjertesykdom + 0.205*RegData$Lungesykdom +
                    0.153*RegData$MedDiabetes + 0.148*RegData$WHOECOG + 0.0666*RegData$ASA)


tmp <- RegData[,c('ForlopsID', 'PRSScore', 'PRSScore_minutregning')]
tmp$PRSScore_minutregning[RegData$WHOECOG==9] <- NA

forskjell <- tmp$PRSScore - tmp$PRSScore_minutregning

indUlik <- which(abs(forskjell)>10^(-12) | (is.na(tmp$PRSScore) & !is.na(tmp$PRSScore_minutregning)) |
                   (!is.na(tmp$PRSScore) & is.na(tmp$PRSScore_minutregning)) )
avvik <- tmp[indUlik,]

setwd('C:/SVN/jasper/norgast/doc/')
write.csv2(avvik, 'avvik.csv', row.names = F)

prs<-read.table('C:/SVN/jasper/norgast/doc/Rescoring_PRS_score.csv', header=TRUE, sep=";", encoding = 'UFT-8')
prs$Endring <- as.character(prs$Endring)
prs_forskj <- prs[substr(prs$Endring, 1, 9)!='No change', ]
prs_forskj$Endring<-gsub('.*=([0-9]+).*','\\1', prs_forskj$Endring)
names(prs_forskj)[1] <- 'ForlopsID'
prs_forskj$ForlopsID <- as.numeric(prs_forskj$ForlopsID)
prs_forskj$PRS_GAMMEL <- as.numeric(as.character(prs_forskj$PRS_GAMMEL))
prs_forskj$PRS_NY <- as.numeric(as.character(prs_forskj$PRS_NY))


avvik[avvik$ForlopsID %in% setdiff(avvik$ForlopsID, prs_forskj$ForlopsID), ]
prs_forskj[prs_forskj$ForlopsID %in% setdiff(prs_forskj$ForlopsID, avvik$ForlopsID), ]


prsTorkil<-read.table('C:/SVN/jasper/norgast/doc/Rescoring_PRS_score Torkil.csv', header=TRUE, sep=",", encoding = 'UFT-8')
prsTorkil$NEW_PRSCORE <- as.numeric(as.character(prsTorkil$NEW_PRSCORE))
prsTorkil$OLD_PRSScore <- as.numeric(as.character(prsTorkil$OLD_PRSScore))

forskjell2 <- prsTorkil$OLD_PRSScore - prsTorkil$NEW_PRSCORE

indUlik <- which(abs(forskjell2)>10^(-12) | (is.na(prsTorkil$OLD_PRSScore) & !is.na(prsTorkil$NEW_PRSCORE)) |
                   (!is.na(prsTorkil$OLD_PRSScore) & is.na(prsTorkil$NEW_PRSCORE)) )
avvik2 <- prsTorkil[indUlik,]

setdiff(avvik$ForlopsID, avvik2$ForlopsID)
setdiff(avvik2$ForlopsID, avvik$ForlopsID)

RegData[RegData$ForlopsID %in% c(setdiff(avvik$ForlopsID, avvik2$ForlopsID), setdiff(avvik2$ForlopsID, avvik$ForlopsID)),
        c('ForlopsID', 'PRSScore', 'PRSScore_minutregning')]
prsTorkil[prsTorkil$ForlopsID %in% c(setdiff(avvik$ForlopsID, avvik2$ForlopsID), setdiff(avvik2$ForlopsID, avvik$ForlopsID)), ]




Endelig <- merge(tmp, prsTorkil, by = 'ForlopsID')

forskjell3 <- Endelig$PRSScore_minutregning - Endelig$NEW_PRSCORE
indUlik <- which(abs(forskjell3)>10^(-12) | (is.na(Endelig$NEW_PRSCORE) & !is.na(Endelig$PRSScore_minutregning)) |
                   (!is.na(Endelig$NEW_PRSCORE) & is.na(Endelig$PRSScore_minutregning)) )

avvik <- Endelig[indUlik, ]

setwd('C:/SVN/jasper/norgast/doc/')
write.csv2(avvik[, c('ForlopsID', 'NEW_PRSCORE', 'PRSScore_minutregning')], 'avvik.csv', row.names = F)


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


DoedData <- RegData[intersect(which(RegData$AvdRESH==102141), which(RegData$DodUnderOpphold==1 | RegData$OppfDodUnderOpphold == 1)),]

IDer <- RegData$ForlopsID[intersect(which(RegData$AvdRESH==102141), which(RegData$DodUnderOpphold==1 | RegData$OppfDodUnderOpphold == 1))]


## Lag kobling til Sykehusnavn #####################


#
#
# RegData <- RegData[which(RegData$OperasjonsDato>=datoFra & RegData$OperasjonsDato<=datoTil),]
# RegData <- RegData[which(RegData$Op_gr==op_gruppe),]
#
#
# RegData2 <- RegData[which(RegData$Tilgang==2),]
# RegData2 <- RegData[which(RegData$AnestesiStartKl>=7 & RegData$AnestesiStartKl<=15),]
# RegData2 <- RegData[which(RegData$AnestesiStartKl<7 | RegData$AnestesiStartKl>15),]
#
# vekttap <- RegData2$VekttapProsent[!is.na(RegData2$VekttapProsent)]
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
#   sh<-as.character(RegData$Sykehusnavn[match(p, RegData$AvdRESH)])
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
# # sum(RegData$NyAnastomose)
# # sum(RegData$NyStomi[RegData$NyAnastomose==1])
# #
# # RegData2 <- RegData[RegData$NyAnastomose==0,]
# # sum(RegData2$NyStomi)
#
#
#
#
#
#
#
#
########################## datokødd

# datoFra <- '2014-01-01'	 # min og max dato i utvalget vises alltid i figuren.
# datoTil <- '2016-12-31'
#
#
# dFra <- as.POSIXlt(datoFra)
# dTil <- as.POSIXlt(datoTil)
#
#
# difftid <- dTil-dFra
#
#

erMann=99
minald=0
maxald=130
enhetsUtvalg=1
stabel=F
preprosess=F
elektiv=99
BMI=''
tilgang=99
valgtShus=c('')
minPRS=0
maxPRS=2
ASA=''
whoEcog= ''
forbehandling=99
hentData=F


RegData <- RegData[which(RegData$Op_gr %in% 1:7 & RegData$OperasjonsDato >= '2016-05-01' & RegData$OperasjonsDato < '2017-03-01'), ]
old <- RegData
RegData <- old
RegData <- RegData[RegData$LapTilgang == 0, ]
RegData$Sykehusnavn <- as.character(RegData$Sykehusnavn)
RegData$Sykehusnavn[RegData$Sykehusnavn != 'St. Olavs Hospital HF'] <- 'Landet.forovrig'
RegData$Sykehusnavn <- as.factor(RegData$Sykehusnavn)

# tmp <- aggregate(RegData$PostopLiggedogn, by=list(Sykehus=RegData$Sykehusnavn, Operasjonsgruppe=RegData$Operasjonsgrupper), mean, na.rm=T)
# Gj_Liggetid <- reshape(tmp, direction = 'wide', timevar = 'Operasjonsgruppe', idvar = 'Sykehus')

tmp <- aggregate(RegData$PostopLiggedogn, by=list(Sykehus=RegData$Sykehusnavn, Operasjonsgruppe=RegData$Op_gr), mean, na.rm=T)
Gj_Liggetid <- reshape(tmp, direction = 'wide', timevar = 'Operasjonsgruppe', idvar = 'Sykehus')

names(Gj_Liggetid) <- c('Sykehus', RegData$Operasjonsgrupper[match(sort(unique(RegData$Op_gr)), RegData$Op_gr)])
Gj_Liggetid <- Gj_Liggetid[c(2,1), ]

Gj_Liggetid[, -1] <- round(Gj_Liggetid[, -1], 1)

write.csv2(Gj_Liggetid, 'Gjsn_liggetidStOlavÅpenKonv.csv', row.names = F)


# tmp <- aggregate(RegData$PostopLiggedogn, by=list(Sykehus=RegData$Sykehusnavn, Operasjonsgruppe=RegData$Operasjonsgrupper), function(x){sum(!is.na(x))})
# N_liggetid <- reshape(tmp, direction = 'wide', timevar = 'Operasjonsgruppe', idvar = 'Sykehus')
#
#
# tmp <- aggregate(RegData$PostopLiggedogn, by=list(Sykehus=RegData$Sykehusnavn, Operasjonsgruppe=RegData$Operasjonsgrupper), sum, na.rm=T)
# Sum_Liggetid <- reshape(tmp, direction = 'wide', timevar = 'Operasjonsgruppe', idvar = 'Sykehus')

#################################################################################################
################### Fiks diagnosebeskrivelser   ###################################################

tmp1 <- sort(table(RegData$Hoveddiagnose, useNA = 'ifany'), decreasing = T)
tmp2 <- sort(table(substr(RegData$Hoveddiagnose, 1, 4), useNA = 'ifany'), decreasing = T)
tmp3 <- sort(table(substr(RegData$Hoveddiagnose, 1, 5), useNA = 'ifany'), decreasing = T)

navn1 <- sort(names(tmp2))
navn2 <- sort(names(tmp3))


navn1 <- c(navn1, rep(NA, length(navn2)-length(navn1)))

navndiff <- data.frame(navn2=navn2, navn1=navn1)


#####################################################################################
############# Accordion-grad 6 UNN 2017  ############################################

ACC6 <- RegData$PasientID[which(RegData$AccordionGrad==6 & RegData$OperasjonsDato >= '2017-01-01' & RegData$AvdRESH == 601225)]


ACC6 <- RegData$PasientID[which(RegData$OppfAccordionGrad==6 & RegData$OperasjonsDato >= '2017-01-01' & RegData$AvdRESH == 601225)]



