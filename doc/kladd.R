library(dplyr)
rm(list = ls())

appdata <- norgast::NorgastHentData()

RegData <- appdata$RegData


lagrefolder <- "C:/Users/kth200/regdata/norgast/torkil/"
mce <- rapbase::loadRegData("data", "SELECT * FROM mce")
patient <- rapbase::loadRegData("data", "SELECT * FROM patient")
allevarnum_hnikt <- rapbase::loadRegData("data", "SELECT * FROM allevarnum")

avvik1 <- mce |>
  filter(PATIENT_ID %in% setdiff(PATIENT_ID, patient$ID))
avvik2 <- mce |>
  filter(PATIENT_ID %in% setdiff(allevarnum_hnikt$PasientId, patient$ID))
write.csv2(avvik1,
           paste0(lagrefolder, "avvik_pasient_mce.csv"),
           row.names = F,
           fileEncoding = "Latin1")
write.csv2(avvik2,
           paste0(lagrefolder, "avvik_pasient_allevarnum.csv"),
           row.names = F,
           fileEncoding = "Latin1")

setdiff(mce$PATIENT_ID, patient$ID)

allevarnum_hnikt <- rapbase::loadRegData(registryName, "SELECT * FROM allevarnum")

setdiff(mce$PATIENT_ID, patient$ID)
setdiff(allevarnum_hnikt$PasientId, patient$ID)

### Finn manglende anastomoselekk Kalnes

# appdata <- norgast::NorgastHentData()
RegData_all <- norgast::NorgastHentRegData()
RegData <- RegData_all |>
  filter(PasientID %in% c(75149, 83001, 83153, 83234))

registration <- rapbase::loadRegData(
  "data", "SELECT * FROM registration")
readmission <- rapbase::loadRegData(
  "data", "SELECT * FROM readmission")
allevarnum_hnikt <- readmission <- rapbase::loadRegData(
  "data", "SELECT * FROM allevarnum")

RegData <- merge(
  RegData,
  registration |> select(MCEID, ANASTOMOTIC_LEAK),
  by.x = "ForlopsID", by.y = "MCEID") |>
  merge(readmission |> select(MCEID, ANASTOMOTIC_LEAK),
        by.x = "ForlopsID", by.y = "MCEID", suffixes = c("", "_Oppf"),
        all.x = TRUE)


betrakt <- RegData |>
  select(PasientID, OpDato, ViktigsteFunn, OppfViktigsteFunn,
         ANASTOMOTIC_LEAK, ANASTOMOTIC_LEAK_Oppf, Avdod, AvdodDato)

skjemaoversikt <- appdata$skjemaoversikt
skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
RegData <- norgast::NorgastPreprosess(RegData, behold_kladd = TRUE)

# appdata <- norgast::NorgastHentData()
# allevanum <- appdata$RegData |>
#   norgast::NorgastPreprosess()
regdata_gml <- norgast::NorgastHentRegData() |>
  dplyr::rename(Sykehusnavn = SykehusNavn) |>
  norgast::NorgastPreprosess()

mangler <- RegData |> filter(is.na(PasientID)) |>
  select(ForlopsID) |> unlist()
tmp <- regdata_gml |> filter(ForlopsID %in% mangler)
tmp2 <- RegData |> filter(ForlopsID %in% mangler)
#
query1 <- "SELECT * FROM allevarnum"
allevarnum_gml <- rapbase::loadRegData(registryName, query1, dbType) |>
  filter(#RegistreringStatus==1,
    as.Date(OpDato) >= "2014-01-01") |>
  rename(PasientID = PasientId)
query2 <- "SELECT * FROM registration"
registration <- rapbase::loadRegData(registryName, query2, dbType) |>
  filter(STATUS==1,
         as.Date(OPERATION_DATE) >= "2014-01-01")
mce <- rapbase::loadRegData("data", "SELECT * FROM mce")

samlet <- merge(allevarnum |>
                  select(-c(ICD10_VERSION, NCSP_VERSION)) |>
                  filter(RegistreringStatus==1),
                allevarnum_gml |> filter(RegistreringStatus==1),
                by = "ForlopsID",
                suffixes = c("_ny", "_gml")) %>%
  relocate(sort(names(.)))

agreement_simple <- function(df, vars) {
  results <- lapply(vars, function(v) {
    reg  <- df[[paste0(v, "_ny")]]
    na_reg <- sum(is.na(reg) | reg == -1 | as.character(reg) == "")
    utfylt_norvas <- length(reg) - na_reg
    reg[is.na(reg)] <- -1
    gold <- df[[paste0(v, "_gml")]]
    na_gold <- sum(is.na(gold) | gold == -1 | as.character(gold) == "")
    utfylt_val <- length(gold) - na_gold
    gold[is.na(gold)] <- -1
    utfylt_begge <- length(reg) -
      sum( (is.na(reg) |
              reg == -1 | as.character(reg) == "") |
             (is.na(gold) | gold == -1 | as.character(gold) == ""))
    ## FEIL, finn ut av!

    # exact match including NA
    matches <- reg == gold | (is.na(reg) & is.na(gold))

    percent_agreement <- mean(matches) * 100

    data.frame(
      variabel = v,
      samsvar = percent_agreement,
      utfylt_ny = utfylt_norvas,
      utfylt_gml = utfylt_val,
      utfylt_begge = utfylt_begge
    )
  })

  do.call(rbind, results)
}

agreement_simple(samlet, names(allevarnum_gml))

ulik <- samlet |> filter(is.na(Fodselsnummer_ny))





length(intersect(registration$MCEID, mce$MCEID))

patient <- rapbase::loadRegData("data", "SELECT * FROM patient")

feilsok <- mce |> filter(PATIENT_ID %in% setdiff(mce$PATIENT_ID, patient$ID))
write.csv2(feilsok, "feilsok_norgast.csv", row.names = F,
           fileEncoding = "Latin1")
# begge <- allevarnum_gml |> filter(ForlopsID %in% tmp$ForlopsID) |>
#   select(ForlopsID, RegistreringStatus)
#
# write.csv2(begge, "status1_norgast.csv", row.names = F,
#            fileEncoding = "Latin1")



# manglerpid <- allevarnum |>
#   dplyr::filter(is.na(PasientID)) |>
#   merge(forlopsoversikt |>
#           select(ForlopsID, PasientID, Avdod, AvdodDato),
#         by = "ForlopsID") %>%
#   relocate(sort(names(.)))











#############################################

norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(2021)), ], valgtVar = "Saarruptur",
                                      minald=as.numeric(0),
                                      maxald=as.numeric(120), outfile="", tittel="tittel",  width=600, height=700,
                                      decreasing=F, terskel=5, minstekrav = 80, maal = 90,
                                      legPlass='topleft', minstekravTxt="", maalTxt="maalTxt", graaUt="",
                                      inkl_konf=F, op_gruppe='',
                                      hastegrad_hybrid=1, malign=99, lavDG = "", lavDGtekst = "")



RegData = RegData[which(RegData$Aar <= as.numeric(2021)), ]; valgtVar = "Saarruptur";
minald=as.numeric(0);
maxald=as.numeric(120); outfile=""; tittel="tittel";  width=600; height=700;
decreasing=F; terskel=5; minstekrav = 80; maal = 90;
legPlass='topleft'; minstekravTxt=""; maalTxt="maalTxt"; graaUt="";
inkl_konf=F; op_gruppe='';
hastegrad_hybrid=1; malign=99; lavDG = ""; lavDGtekst = ""


datoFra='2014-01-01'; datoTil='2050-12-31'
erMann=99
elektiv=99
hastegrad=99
dagtid =99
BMI=''
tilgang=''
minPRS=0; maxPRS=2.2
ASA=''
whoEcog= ''
ncsp=''
forbehandling=''
skriftStr=1.3


####################################################################





library(norgast)
library(dplyr)
library(tidyverse)

#############################################################################################################################
##############################      Finnmarkssykehuset       #############################################################
setwd('C:/GIT/norgast/doc/')
rm(list = ls())

RegData <- read.table('I:/norgast/allevarnum2019-06-12 09-09-03.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/forlopsoversikt2019-06-12 09-09-17.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)


skjemaoversikt <- read.table('I:/norgast/skjemaoversikt2019-06-12 09-09-22.txt', header=TRUE, sep=";", encoding = 'UFT-8')
skjemaoversikt$Sykehusnavn <- iconv(skjemaoversikt$Sykehusnavn, from = 'UTF-8', to = '')
skjemaoversikt$Skjemanavn <- iconv(skjemaoversikt$Skjemanavn, from = 'UTF-8', to = '')
skjemaoversikt$OpprettetDato <- as.Date(skjemaoversikt$OpprettetDato)


hfest <- skjemaoversikt[skjemaoversikt$Sykehusnavn=='Hammerfest', ]
hfest <- hfest[hfest$Skjemanavn=='Registrering' & hfest$SkjemaStatus==1, ]
hfest[hfest$OpprettetDato >= '2019-01-01', ]




#############################################################################################################################
#############################################################################################################################

setwd('C:/GIT/norgast/doc/')
rm(list = ls())

RegData <- read.table('I:/norgast/allevarnum2019-04-26 10-59-03.txt', header=TRUE, sep=";", encoding = 'UFT-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/forlopsoversikt2019-04-26 10-59-18.txt', header=TRUE, sep=";", encoding = 'UFT-8')

# RegData <- RegData[RegData$OppfStatus == 'Ferdigstilt', ]

RegData$aar <- as.numeric(format(as.Date(RegData$OpDato, format="%Y-%m-%d"), '%Y'))
RegData <- RegData[RegData$aar <= 2018, ]
RegData$ncsp_lowercase <- substr(tolower(RegData$Hovedoperasjon), 1, 5)
RegData$Operasjonsgrupper <- "Annet"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jfh")] <- "Kolonreseksjoner"
RegData$Operasjonsgrupper[intersect(which(substr(RegData$ncsp_lowercase,1,3)=="jfb"),
                                    which(as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% 20:64))] <- "Kolonreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jgb")] <- "Rektumreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jcc")] <- "Øsofagusreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jdc")] <- "Ventrikkelreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jdd")] <- "Ventrikkelreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,3)=="jjb")] <- "Leverreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,5) %in% c('jlc00','jlc10','jlc11','jlc20','jlc40'))] <- "Andre pankreasreseksjoner"
RegData$Operasjonsgrupper[which(substr(RegData$ncsp_lowercase,1,5) %in% c("jlc30","jlc31"))] <- "Whipples operasjon"
RegData <- RegData[RegData$Operasjonsgrupper != "Annet", ]

antall_na <- RegData %>% group_by(aar) %>% summarise_all(function(x){sum(is.na(x) | x == '')})
antall_na <- antall_na[, colSums(antall_na) != 0]
n_aar <- table(RegData$aar, useNA = 'ifany')


tr_summarize_output <- function(x){

  rekkefolge <- names(x)[-1]
  y <- x %>% gather(names(x)[-1], key=nokkel, value = verdi) %>%
    spread(key=names(x)[1], value = verdi)
  y <- y[match(rekkefolge, y$nokkel), ]
  names(y)[1] <- ''

  return(invisible(y))
}

antall_na <- tr_summarize_output(antall_na)
antall_na <- bind_rows(antall_na, n_aar)
antall_na$totalt <- rowSums(antall_na[,-1])

antall_na[,-1] <- apply(antall_na[,-1], 2, function(x){round(x/x[length(x)]*100, 1)})
antall_na <- antall_na[antall_na$totalt >= 5, ]
antall_na[dim(antall_na)[1], -c(1,dim(antall_na)[2])] <- n_aar
antall_na$totalt[dim(antall_na)[1]] <- sum(n_aar)
antall_na[dim(antall_na)[1], 1] <- 'N'

write.csv2(antall_na, 'missing_norgast_v2.csv', row.names = F)


############## Spørsmål fra Linn angående n i mortalitetsfigurer ######################

## Nye tall:
library(norgast)
rm(list = ls())

RegData <- read.table('I:/norgast/allevarnum2019-06-06 08-41-44.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/forlopsoversikt2019-06-06 08-42-14.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
# RegData <- RegData[RegData$Aar==2016, ]

gr <- c(1:6)
grtxt <- c('Kol.','Rekt.','Øsof.','Ventr.',
           'Lever',"Pankreas")
RegData$Op_grAarsrapp[RegData$Op_grAarsrapp==7]<- 6
RegData$Op_grAarsrapp[RegData$Op_grAarsrapp %in% c(8,99)]<- NA
# RegData$Op_grAarsrapp[RegData$Op_grAarsrapp==99]<-NA

rap_aar <- 2018 # Året rapporten skal kjøres for
ant_aar <- 3 # Hvor mange år som skal inkluderes i flerårsfigurer

RegData$Op_grAarsrapp <- factor(RegData$Op_grAarsrapp, levels=gr, labels = grtxt)

reshID <- 0
datoFra= paste0(rap_aar, '-01-01')
datoTil= paste0(rap_aar, '-12-31')

RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn) # Fjern mellomrom før og etter sykehusnavn

RegDataAll <- RegData[RegData$Aar<=rap_aar, ]

## Forrige versjon

RegData <- read.table('I:/norgast/allevarnum2019-04-26 10-59-03.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/forlopsoversikt2019-04-26 10-59-18.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
# RegData <- RegData[RegData$Aar==2016, ]

gr <- c(1:6)
grtxt <- c('Kol.','Rekt.','Øsof.','Ventr.',
           'Lever',"Pankreas")
RegData$Op_grAarsrapp[RegData$Op_grAarsrapp==7]<- 6
RegData$Op_grAarsrapp[RegData$Op_grAarsrapp %in% c(8,99)]<- NA
# RegData$Op_grAarsrapp[RegData$Op_grAarsrapp==99]<-NA

rap_aar <- 2018 # Året rapporten skal kjøres for
ant_aar <- 3 # Hvor mange år som skal inkluderes i flerårsfigurer

RegData$Op_grAarsrapp <- factor(RegData$Op_grAarsrapp, levels=gr, labels = grtxt)

reshID <- 0
datoFra= paste0(rap_aar, '-01-01')
datoTil= paste0(rap_aar, '-12-31')

RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn) # Fjern mellomrom før og etter sykehusnavn

RegDataAll_gml <- RegData[RegData$Aar<=rap_aar, ]


tmp<-RegDataAll_gml[RegDataAll_gml$Sykehusnavn == 'UNN-Tromsø' & RegDataAll_gml$Op_gr==6 & RegDataAll_gml$Aar %in% 2016:2018, ]
tmp2<-RegDataAll[RegDataAll$Sykehusnavn == 'UNN-Tromsø' & RegDataAll$Op_gr==6 & RegDataAll$Aar %in% 2016:2018, ]








# write.csv2(antall_na, 'missing_norgast_ferdigoppf_v2.csv', row.names = F)





# antall_tomtekst <- RegData %>% group_by(aar) %>% summarise_all(function(x){sum(x == '')})
# antall_tomtekst <- antall_tomtekst[, colSums(antall_tomtekst) != 0]


# dplyr::summarise_all(RegData, function(x){sum(is.na(x))})
# dplyr::summarise_all(RegData, function(x){sum(x == '', na.rm = T)})
#
# table(RegData$BasisRegStatus)
# table(RegData$RegistreringStatus)
#
# RegData$Vekt6MndFoer
