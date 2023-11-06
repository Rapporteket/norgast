library(norgast)
library(tidyverse)
rm(list = ls())

rap_aar <- 2022

RegData <-  norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData) %>%
  filter(Aar <= rap_aar)
RegData$AvdRESH[RegData$AvdRESH == 4204126] <- 4204084 # Tull med Ringerike
gr <- c(1:6)
grtxt <- c('Kol.','Rekt.','Øsof.','Ventr.',
           'Lever',"Pankreas")
RegData$Op_grAarsrapp <- RegData$Op_gr
RegData$Op_grAarsrapp[RegData$Op_gr %in% 6:8]<- 6
RegData$Op_grAarsrapp[!(RegData$Op_grAarsrapp %in% 1:6)]<- NA
RegData$Op_grAarsrapp <- factor(RegData$Op_grAarsrapp, levels=gr, labels = grtxt)

width=600
height=700
sideTxt='Sykehus'
decreasing=F
terskel=10
minstekrav = NA
maal = NA
skriftStr=0.9
pktStr=1.2
legPlass='top'
minstekravTxt='Akseptabelt'
dg_tekst <- "Dekningsgrad < 60 %"
maalTxt='Mål'
graaUt=NA
minald=0
maxald=130
erMann <- 99
inkl_konf <- T
elektiv=99
# datoFra <- '2015-01-01'
datoFra= paste0(rap_aar-2, '-01-01')
datoTil= paste0(rap_aar, '-12-31')
tittel <- ''
hentData <- F
preprosess <- F
BMI=''
tilgang=''
minPRS=0
maxPRS=2.2
ASA=''
whoEcog= ''
ncsp=''
forbehandling=''
valgtShus=c('')
op_gruppe <- ''
malign <- 99
annet_format_ut <- T
ut_format <- 'svg'
tabfolder <- "~/mydata/norgast/fig_aarsrapp2022/tabeller/"
if (!dir.exists(tabfolder)) {
  dir.create(tabfolder)
}

valgtVar <- 'Vekttap_registrert'
outfile <- ""

dash_data <- norgastIndikator_rapporteket(
  RegData[RegData$Op_gr %in% 1:8, ], valgtVar = valgtVar, outfile=outfile,
  tittel=tittel, width=width, height=height, decreasing=decreasing, terskel=terskel,
  minstekrav = 80, maal = 90, skriftStr=skriftStr, pktStr=pktStr, legPlass='topleft',
  minstekravTxt=minstekravTxt, maalTxt=maalTxt, graaUt=graaUt, inkl_konf=inkl_konf,
  op_gruppe=op_gruppe, datoFra=datoFra, datoTil=datoTil, hastegrad_hybrid=1,
  malign=malign)


data.frame(Sykehus = rownames(dash_data$ant_tilfeller),
           Antall = dash_data$ant_tilfeller$`2022`,
           N = dash_data$N$`2022`) %>%
  mutate(Andel = Antall/N*100) %>%
  arrange(desc(Andel)) %>%
  write.csv2(paste0(tabfolder, "vekttap_dash_2022.csv"), row.names = F, fileEncoding = "Latin1")


valgtVar <- 'AktivKontroll_v2'
dash_data <- norgastIndikator_rapporteket(
  RegData[RegData$Op_gr %in% 1:8, ], valgtVar = valgtVar, outfile=outfile,
  tittel=tittel, width=width, height=height, decreasing=decreasing, terskel=terskel,
  minstekrav = 70, maal = 90, skriftStr=skriftStr, pktStr=pktStr, legPlass='topleft',
  minstekravTxt=minstekravTxt, maalTxt=maalTxt, graaUt=graaUt, inkl_konf=inkl_konf,
  op_gruppe=op_gruppe, datoFra=datoFra, datoTil=datoTil, hastegrad_hybrid=1,
  malign=malign)

data.frame(Sykehus = rownames(dash_data$ant_tilfeller),
           Antall = dash_data$ant_tilfeller$`2022`,
           N = dash_data$N$`2022`) %>%
  mutate(Andel = Antall/N*100) %>%
  arrange(desc(Andel)) %>%
  write.csv2(paste0(tabfolder, "aktivkontroll_dash_2022.csv"),
             row.names = F, fileEncoding = "Latin1")


######## Kjønn

RegData %>% filter(Op_gr %in% 1:8) %>%
  summarise(Ant_kvinner = sum(erMann == 0),
            Ant_menn = sum(erMann),
            N = n(),
            .by = c(Op_grAarsrapp, Aar)) %>%
  janitor::adorn_totals() %>%
  write.csv2(paste0(tabfolder, "kjonnsfordeling_dash.csv"),
             row.names = F, fileEncoding = "Latin1")

##### Alder

alder2022 <- RegData %>%
  filter(Op_gr %in% 1:8) %>%
  filter(Aar == 2022) %>%
  summarise(Gj.sn.alder = mean(Alder),
            N = n(),
            .by = c(Op_grAarsrapp)) %>%
  bind_rows(summarise(., Op_grAarsrapp = "Totalt",
                      Gj.sn.alder = sum(Gj.sn.alder*N)/sum(N),
                      N = sum(N))) %>%
  mutate(Aar = "2022")

alder2020_22 <- RegData %>%
  filter(Op_gr %in% 1:8) %>%
  filter(Aar %in% 2020:2022) %>%
  summarise(Gj.sn.alder = mean(Alder),
            N = n(),
            .by = c(Op_grAarsrapp)) %>%
  bind_rows(summarise(., Op_grAarsrapp = "Totalt",
                      Gj.sn.alder = sum(Gj.sn.alder*N)/sum(N),
                      N = sum(N))) %>%
  mutate(Aar = "2020-2022")

bind_rows(alder2022, alder2020_22) %>%
  rename(Op_gr = Op_grAarsrapp) %>%
  select(Op_gr, Aar, Gj.sn.alder, N) %>%
  write.csv2(paste0(tabfolder, "gjsnalder_dash.csv"),
             row.names = F, fileEncoding = "Latin1")





######### Kolon/rektum

valgtVar <- 'Hastegrad'
dash_data <- norgastIndikator_rapporteket(
  RegData, valgtVar = valgtVar, outfile=outfile, tittel=tittel, width=width,
  height=height, decreasing=F, terskel=terskel, minstekrav = NA, maal = NA,
  skriftStr=skriftStr, pktStr=pktStr, legPlass=legPlass,
  inkl_konf=inkl_konf, op_gruppe=1, datoFra='2018-05-01', datoTil=datoTil,
  hastegrad_hybrid=99, malign=99)

data.frame(Sykehus = rownames(dash_data$ant_tilfeller),
           Antall = dash_data$ant_tilfeller$`2022`,
           N = dash_data$N$`2022`) %>%
  mutate(Andel = Antall/N*100) %>%
  arrange(desc(Andel)) %>%
  write.csv2(paste0(tabfolder, "akuttkirurgi2022.csv"),
             row.names = F, fileEncoding = "Latin1")


valgtVar <- 'ohjelp_kveld'
dash_data <- norgastIndikator_rapporteket(
  RegData, valgtVar = valgtVar, outfile=outfile, tittel=tittel, width=width,
  height=height, decreasing=F, terskel=terskel, minstekrav = NA, maal = NA,
  skriftStr=skriftStr, pktStr=pktStr, legPlass=legPlass,
  inkl_konf=inkl_konf, op_gruppe=1, datoFra='2018-05-01', datoTil=datoTil,
  hastegrad=2, malign=99)

data.frame(Sykehus = rownames(dash_data$ant_tilfeller),
           Antall = dash_data$ant_tilfeller$`2022`,
           N = dash_data$N$`2022`) %>%
  mutate(Andel = Antall/N*100) %>%
  arrange(desc(Andel)) %>%
  write.csv2(paste0(tabfolder, "ohjelp_kveld_2022.csv"),
             row.names = F, fileEncoding = "Latin1")

valgtVar <- 'Anastomoselekkasje'
dash_data <- norgastIndikator_rapporteket(
  RegData, valgtVar = valgtVar, outfile=outfile, tittel=tittel, width=width,
  height=height, decreasing=T, terskel=terskel, minstekrav = 7, maal = 5,
  skriftStr=skriftStr, maalretn='lav',
  pktStr=pktStr, legPlass='topright',
  whoEcog = 0:1, inkl_konf=inkl_konf, op_gruppe=2,
  datoFra=datoFra, datoTil=datoTil, hastegrad_hybrid=1, malign=1)

data.frame(Sykehus = rownames(dash_data$ant_tilfeller),
           Antall = dash_data$ant_tilfeller$`2022`,
           N = dash_data$N$`2022`) %>%
  mutate(Andel = Antall/N*100) %>%
  arrange(desc(Andel)) %>%
  write.csv2(paste0(tabfolder, "anastomose_rektum_2022.csv"),
             row.names = F, fileEncoding = "Latin1")

### Ævre GI
valgtVar <- 'Anastomoselekk_osofagus'
dash_data <- norgastIndikator_rapporteket(
  RegData, valgtVar = valgtVar, outfile=outfile, width=width, height=height,
  decreasing=T, terskel=terskel, minstekrav = NA, maal = 20, skriftStr=skriftStr,
  pktStr=pktStr, legPlass=legPlass,
  maalretn='lav',
  inkl_konf=inkl_konf, op_gruppe=3, datoFra='2018-05-01', datoTil=datoTil,
  malign=malign,
  tittel=c('Anastomoselekkasje eller dyp infeksjon, eller endoskopisk',
           ' intervensjon for lekkasje v/ øsofaguskirurgi'))

data.frame(Sykehus = rownames(dash_data$ant_tilfeller),
           Antall = dash_data$ant_tilfeller$`2022`,
           N = dash_data$N$`2022`) %>%
  mutate(Andel = Antall/N*100) %>%
  arrange(desc(Andel)) %>%
  write.csv2(paste0(tabfolder, "anastomose_oesofagus_2022.csv"),
             row.names = F, fileEncoding = "Latin1")


valgtVar <- 'KumAcc'
dash_data <- norgastIndikator_rapporteket(
  RegData, valgtVar = valgtVar, outfile=outfile, tittel=tittel, width=width,
  height=height, decreasing=decreasing, terskel=terskel, minstekrav = minstekrav,
  maal = maal, skriftStr=skriftStr, pktStr=pktStr, legPlass=legPlass,
  minstekravTxt=minstekravTxt, maalTxt=maalTxt,
  inkl_konf=inkl_konf, op_gruppe=4,
  datoFra=datoFra, datoTil=datoTil,
  malign=malign)

data.frame(Sykehus = rownames(dash_data$ant_tilfeller),
           Antall = dash_data$ant_tilfeller$`2022`,
           N = dash_data$N$`2022`) %>%
  mutate(Andel = Antall/N*100) %>%
  arrange(desc(Andel)) %>%
  write.csv2(paste0(tabfolder, "Acc3_2022.csv"),
             row.names = F, fileEncoding = "Latin1")

######### HPB
valgtVar <- 'ReLapNarkose'
dash_data <- norgastIndikator_rapporteket(
  RegData, valgtVar = valgtVar, outfile=outfile, tittel=tittel, width=width,
  height=height, decreasing=T, terskel=terskel, minstekrav = 10, maal = 7,
  skriftStr=skriftStr, pktStr=pktStr,
  legPlass=legPlass, minstekravTxt=minstekravTxt, maalTxt=maalTxt,
  maalretn = 'lav', inkl_konf=inkl_konf, op_gruppe=5, datoFra=datoFra,
  datoTil=datoTil, malign=malign)

data.frame(Sykehus = rownames(dash_data$ant_tilfeller),
           Antall = dash_data$ant_tilfeller$`2022`,
           N = dash_data$N$`2022`) %>%
  mutate(Andel = Antall/N*100) %>%
  arrange(desc(Andel)) %>%
  write.csv2(paste0(tabfolder, "relap_lever_2022.csv"),
             row.names = F, fileEncoding = "Latin1")

valgtVar <- 'mortalitet90'
dash_data <- norgastIndikator_rapporteket(
  RegData, valgtVar = valgtVar, outfile=outfile, tittel=tittel, width=width,
  height=height, decreasing=T, terskel=terskel, minstekrav = 8, maal = 5,
  skriftStr=skriftStr, pktStr=pktStr, legPlass=legPlass, minstekravTxt=minstekravTxt,
  maalTxt=maalTxt,
  inkl_konf=inkl_konf, op_gruppe=6, datoFra=datoFra, datoTil=datoTil,
  malign=malign, kun_ferdigstilte = F)

data.frame(Sykehus = rownames(dash_data$ant_tilfeller),
           Antall = dash_data$ant_tilfeller$`2022`,
           N = dash_data$N$`2022`) %>%
  mutate(Andel = Antall/N*100) %>%
  arrange(desc(Andel)) %>%
  write.csv2(paste0(tabfolder, "mortalitet_whipple_2022.csv"),
             row.names = F, fileEncoding = "Latin1")

