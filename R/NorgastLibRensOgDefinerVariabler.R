#'   Denne funksjonen definerer en del nye (sammensatte) variabler relevante for rapporter i NoRGast
#'   og rensker opp i andre.
#'
#'   Må ha tilgang til filen 'Helligdager2008-2022.csv'
#'
#'   @inheritParams FigAndeler
#'
#'   @return Data En "list" som består av det prosesserte datasettet RegData samt sykehusnavnet basert på oppgitt reshID
#'
#'   @export


NorgastLibRensOgDefinerVariabler <- function(RegData=RegData, reshID=reshID)

{

names(RegData)[which(names(RegData)=='isMale')]<-'erMann'
RegData <- RegData[which(RegData$STATUS==1),] # Inkluder kun lukkede registreringer
RegData$OperasjonsDato <- as.POSIXlt(RegData$OPERATION_DATE, format="%Y-%m-%d") # %H:%M:%S" )  #"%d.%m.%Y"	"%Y-%m-%d"
RegData$RegMnd <- RegData$OperasjonsDato$mon +1
#     RegData$DoedsDato <- as.POSIXlt(RegData$DECEASED_DATE, format="%Y-%m-%d")
#     RegData$OpDoedTid <- difftime(RegData$DoedsDato,RegData$OperasjonsDato)
RegData$Avdeling <- as.character(RegData$Avdeling)

RegData$SykehusNavn <- RegData$Sykehusnavn

shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])

RegData$Alder <- floor(RegData[ ,'decimalAge'])
RegData$ncsp_lowercase <- substr(tolower(RegData$NCSP), 1, 5)
lowercase <- which(substr(RegData$NCSP, 1, 5)!=toupper(substr(RegData$NCSP, 1, 5)))
uppercase <- match(toupper(RegData$NCSP[lowercase]), substr(RegData$NCSP, 1, 5))
RegData$NCSP[lowercase[which(!is.na(uppercase))]] <- RegData$NCSP[uppercase[which(!is.na(uppercase))]]
RegData$NCSP <- iconv(RegData$NCSP, "UTF-8", "latin1")
RegData$Vektendring <- -RegData$WEIGHTLOSS
RegData$Forbehandling <- NA
RegData$Forbehandling[which(as.numeric(RegData$CHEMOTHERAPY_ONLY)==1)] <- 1
RegData$Forbehandling[which(as.numeric(RegData$RADIATION_THERAPY_ONLY)==1)] <- 2
RegData$Forbehandling[which(as.numeric(RegData$CHEMORADIOTHERAPY)==1)] <- 3
RegData$Forbehandling[intersect(intersect(which(as.numeric(RegData$CHEMOTHERAPY_ONLY)==0),
                                          which(as.numeric(RegData$RADIATION_THERAPY_ONLY)==0)),
                                which(as.numeric(RegData$CHEMORADIOTHERAPY)==0))] <- 9
RegData$BMI_kodet <- NA
RegData$BMI_kodet[which(RegData$BMI_CATEGORY=='Alvorlig undervekt')] <- 1
RegData$BMI_kodet[which(RegData$BMI_CATEGORY=='Undervekt')] <- 2
RegData$BMI_kodet[which(RegData$BMI_CATEGORY=='Mild undervekt')] <- 3
RegData$BMI_kodet[which(RegData$BMI_CATEGORY=='Normal')] <- 4
RegData$BMI_kodet[which(RegData$BMI_CATEGORY=='Overvekt')] <- 5
RegData$BMI_kodet[which(RegData$BMI_CATEGORY=='Moderat fedme, klasse I')] <- 6
RegData$BMI_kodet[which(RegData$BMI_CATEGORY=='Fedme, klasse II')] <- 7
RegData$BMI_kodet[which(RegData$BMI_CATEGORY=='Fedme, klasse III')] <- 8

# Definer operasjonsgrupper basert på NCSP kode
RegData <- RegData[which(RegData$ncsp_lowercase!=''),]
RegData$Operasjonsgrupper <- "Annet"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jfh")] <- "Kolonreseksjoner"
RegData$Operasjonsgrupper[intersect(which(substr(RegData$ncsp_lowercase,1,3)=="jfb"),
                                    which(as.numeric(substr(RegData$ncsp_lowercase,4,5)) > 19 &
                                            as.numeric(substr(RegData$ncsp_lowercase,4,5))<65))] <- "Kolonreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jgb")] <- "Rektumreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jcc")] <- "Øsofagusreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jdc")] <- "Ventrikkelreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jdd")] <- "Ventrikkelreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jjb")] <- "Leverreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,5)=="jlc30")] <- "Whipples operasjon"

RegData$Op_gr <- 0
RegData$Op_gr[which(RegData$Operasjonsgrupper == "Kolonreseksjoner")] <- 1
RegData$Op_gr[which(RegData$Operasjonsgrupper == "Rektumreseksjoner")] <- 2
RegData$Op_gr[which(RegData$Operasjonsgrupper == "Øsofagusreseksjoner")] <- 3
RegData$Op_gr[which(RegData$Operasjonsgrupper == "Ventrikkelreseksjoner")] <- 4
RegData$Op_gr[which(RegData$Operasjonsgrupper == "Leverreseksjoner")] <- 5
RegData$Op_gr[which(RegData$Operasjonsgrupper == "Whipples operasjon")] <- 6
RegData$Op_gr[which(RegData$Operasjonsgrupper == "Annet")] <- 9

RegData$Op_gr2 <- 9
RegData$Op_gr2[intersect(which(RegData$Operasjonsgrupper=='Kolonreseksjoner'), which(RegData$ANASTOMOSIS==1))] <- 1
RegData$Op_gr2[intersect(which(RegData$Operasjonsgrupper=='Kolonreseksjoner'), which(RegData$ANASTOMOSIS==0))] <- 2
RegData$Op_gr2[intersect(which(RegData$Operasjonsgrupper=='Rektumreseksjoner'), which(RegData$ANASTOMOSIS==1))] <- 3
RegData$Op_gr2[intersect(which(RegData$Operasjonsgrupper=='Rektumreseksjoner'), which(RegData$ANASTOMOSIS==0))] <- 4
RegData$Op_gr2[RegData$Operasjonsgrupper=='Øsofagusreseksjoner'] <- 5
RegData$Op_gr2[intersect(which(RegData$Operasjonsgrupper=='Ventrikkelreseksjoner'), which(RegData$ANASTOMOSIS==1))] <- 6
RegData$Op_gr2[intersect(which(RegData$Operasjonsgrupper=='Ventrikkelreseksjoner'), which(RegData$ANASTOMOSIS==0))] <- 7
RegData$Op_gr2[RegData$Operasjonsgrupper=='Whipples operasjon'] <- 8


#### Inkluder ACCORDION SCORE fra oppfølgingsskjema
RegData$ACCORDION_SCORE <- as.character(RegData$ACCORDION_SCORE)
RegData$ACCORDION_SCORE[RegData$ACCORDION_SCORE=='Mindre enn 3'] <- '1'
RegData$ACCORDION_SCORE <- as.numeric(RegData$ACCORDION_SCORE)
RegData$READMISSION_ACCORDION_SCORE <- as.character(RegData$READMISSION_ACCORDION_SCORE)
RegData$READMISSION_ACCORDION_SCORE[RegData$READMISSION_ACCORDION_SCORE=='Mindre enn 3'] <- 1
RegData$READMISSION_ACCORDION_SCORE <- as.numeric(RegData$READMISSION_ACCORDION_SCORE)
RegData$READMISSION_ACCORDION_SCORE[RegData$READMISSION_STATUS!=1]<-NA
RegData$ACCORDION_SCORE[!is.na(pmax(RegData$ACCORDION_SCORE, RegData$READMISSION_ACCORDION_SCORE))] <-
  pmax(RegData$ACCORDION_SCORE, RegData$READMISSION_ACCORDION_SCORE)[!is.na(pmax(RegData$ACCORDION_SCORE,
                                                                               RegData$READMISSION_ACCORDION_SCORE))]

#### Inkluder Relaparotomi fra oppfølgingsskjema

RegData$READMISSION_RELAPAROTOMY[RegData$READMISSION_STATUS!=1] <- NA
RegData$READMISSION_RELAPAROTOMY_YES[RegData$READMISSION_STATUS!=1] <- NA
RegData$RELAPAROTOMY <- pmax(RegData$RELAPAROTOMY, RegData$READMISSION_RELAPAROTOMY, na.rm = TRUE)
RegData$RELAPAROTOMY_YES <- pmin(RegData$RELAPAROTOMY_YES, RegData$READMISSION_RELAPAROTOMY_YES, na.rm = TRUE)

##############
# Helligdager <- read.table(paste0(libkat, 'Helligdager2008-2022.csv'), header=TRUE, sep=";")
# Helligdager <- sort(as.POSIXlt(Helligdager$Dato, format="%d.%m.%Y"))

Helligdager <- sort(Helligdager2008til2022$Dato)

RegData$Hastegrad <- NA
RegData$Hastegrad[as.numeric(RegData$ANESTHESIA_START) %in% 8:15] <- 1
RegData$Hastegrad[as.numeric(RegData$ANESTHESIA_START) %in% c(0:7, 16:23)] <- 0
RegData$Hastegrad[RegData$OperasjonsDato$wday %in% c(0, 6)] <- 0
RegData$Hastegrad[RegData$OperasjonsDato %in% Helligdager] <- 0

RegData$AvlastendeStomiRektum <- NA
RegData$AvlastendeStomiRektum[intersect(intersect(which(as.numeric(RegData$ANASTOMOSIS)==1), which(RegData$Op_gr==2)),
                              which(as.numeric(RegData$OSTOMY)==0))] <- 0
RegData$AvlastendeStomiRektum[union(which(is.na(RegData$ANASTOMOSIS)), which(is.na(RegData$OSTOMY)))] <- NA
RegData$AvlastendeStomiRektum[intersect(intersect(which(as.numeric(RegData$ANASTOMOSIS)==1),
                                                  which(as.numeric(RegData$OSTOMY)==1)),which(RegData$Op_gr==2))] <- 1

RegData$PermanentStomiColorektal <- NA
RegData$PermanentStomiColorektal[intersect(union(which(as.numeric(RegData$ANASTOMOSIS)==1), which(as.numeric(RegData$OSTOMY)==0)),
                                           union(which(RegData$Op_gr==1),which(RegData$Op_gr==2)))] <- 0
RegData$PermanentStomiColorektal[union(which(is.na(RegData$ANASTOMOSIS)), which(is.na(RegData$OSTOMY)))] <- NA
RegData$PermanentStomiColorektal[intersect(intersect(which(as.numeric(RegData$ANASTOMOSIS)==0),which(as.numeric(RegData$OSTOMY)==1)),
                                           union(which(RegData$Op_gr==1),which(RegData$Op_gr==2)))] <- 1

RegData$Anastomoselekkasje <- NA
RegData$Anastomoselekkasje[RegData$ANASTOMOSIS==1] <- 0
RegData$Anastomoselekkasje[RegData$RELAPAROTOMY_YES==1] <- 1
RegData$Anastomoselekkasje[RegData$ANASTOMOSIS!=1] <- NA

Data <- list(RegData=RegData, shtxt=shtxt)

return(invisible(Data))

}
