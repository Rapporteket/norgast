# setwd('C:/GIT/norgast/doc/')
library(norgast)
library(tidyverse)
rm(list=ls())

#### Kjerstin 30.05.2023 ########################################################
RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)


# Oppsum <-
RegData %>%
  filter(Op_gr %in% 1:8) %>%
  filter(Aar == 2021) %>%
  summarise(N = n(),
            OpDoedTidUnder90 = sum(OpDoedTid <= 90, na.rm = T),
            Acc6 = sum(AccordionGrad == 6),
            OppholdOver30 = sum(as.numeric(PostopLiggedogn > 30), na.rm = T),
            DoedUnderOpphold = sum(DodUnderOpphold),
            EntenEller = sum(PostopLiggedogn > 30 | DodUnderOpphold),
            OppfUferdig = sum(OppfStatus != 1 | is.na(OppfStatus)),
            OppfNA = sum(is.na(OppfStatus)),
            .by = Operasjonsgrupper) %>%
  write.csv2("~/mydata/norgast/statusgreier.csv",
             row.names = F, fileEncoding = "Latin1")


# kolon <-
RegData %>%
  filter(Op_gr == 1) %>%
  filter(Aar == 2021) %>%
  filter(Hastegrad_hybrid == 0) %>%
  summarise(N = n(),
            OpDoedTidUnder90 = sum(OpDoedTid <= 90, na.rm = T),
            Acc6 = sum(AccordionGrad == 6),
            OppholdOver30 = sum(as.numeric(PostopLiggedogn > 30), na.rm = T),
            DoedUnderOpphold = sum(DodUnderOpphold),
            EntenEller = sum(PostopLiggedogn > 30 | DodUnderOpphold),
            OppfUferdig = sum(OppfStatus != 1 | is.na(OppfStatus)),
            OppfNA = sum(is.na(OppfStatus))) %>%
  write.csv2("~/mydata/norgast/statusgreier_akuttkolon.csv",
             row.names = F, fileEncoding = "Latin1")



#### Morten Eriksen FHI Kolon 20.04.2023 ########################################
RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)

RegData <- RegData %>% filter(Op_gr %in% 1:8) %>%
  filter(!is.na(KumAcc)) %>% filter(Aar %in% 2018:2022) %>%
  filter(OppfStatus == 1 | is.na(OppfStatus))

RegData$Avdeling <- "Resten"
RegData$Avdeling[RegData$AvdRESH == 106168] <- "OUS-Ullevål"

RegData %>%
  group_by(Avdeling, Aar) %>%
  summarise(Acc_3 = sum(AccordionGrad == 3),
            Acc_4 = sum(AccordionGrad == 4),
            Acc_5 = sum(AccordionGrad == 5),
            Acc_6 = sum(AccordionGrad == 6),
            N = n()
  ) %>% write.csv2("~/mydata/norgast/norgast_kompl.csv", row.names = F, fileEncoding = "Latin1")

RegData %>%
  filter(WHOECOG %in% 0:1 & Hastegrad_hybrid == 1) %>%
  group_by(Avdeling, Aar) %>%
  summarise(Acc_3 = sum(AccordionGrad == 3),
            Acc_4 = sum(AccordionGrad == 4),
            Acc_5 = sum(AccordionGrad == 5),
            Acc_6 = sum(AccordionGrad == 6),
            N = n()
  ) %>% write.csv2("~/mydata/norgast/norgast_kompl_index.csv", row.names = F, fileEncoding = "Latin1")


#### Sjarmoffansiv FHI Kolon 11.04.2023 ########################################

RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
RegData <- RegData %>%
  dplyr::filter(Aar %in% 2016:2022) %>%
  dplyr::filter(Op_gr == 1)
grtxt <- c('Åpen', 'Laparoskopisk', 'Konvertert')
RegData$Tilgang <- factor(RegData$Tilgang, levels=1:3, labels = grtxt)

RegData %>% dplyr::group_by(Tilgang) %>%
  dplyr::summarise("Antall kolonreseksjoner" = n(),
                   "Dyp infeksjon" = sum(ViktigsteFunn == 2, na.rm = T)) %>%
  write.csv2("~/mydata/norgast/FHI_kolon_dypinfeksjon_2016_2022.csv", row.names = F, fileEncoding = "Latin1")


# RegData %>% dplyr::group_by(Aar, Tilgang) %>%
#   dplyr::summarise("Antall kolonreseksjoner" = n(),
#                    "Dyp infeksjon" = sum(ViktigsteFunn == 2, na.rm = T))



#### Gjenskap tabeller til kreg fra 2022, 03.04.2023 ###########################
# fra_krg <- read.csv2("/home/rstudio/.ssh/pancreas2021_norgast_koblet_KRG.csv",
#                      colClasses = c("character", "Date", "numeric"))
fra_krg <- readxl::read_xlsx("~/mydata/norgast/liste_norgast.xlsx")

# fra_krg <- fra_krg[fra_krg$pdac_krg == 1, ]

RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
RegData <- RegData[RegData$Aar %in% 2022, ]
fid <- read.csv2("~/mydata/norgast/NoRGast_koblingstabell_datadump_03.04.2023.csv",
                 colClasses = c("integer", "character"))

RegData <- merge(RegData, fid, by.x = "PasientID", by.y = "PID", all.x = T)
kobletdata <- RegData[RegData$SSN %in% fra_krg$FNR, ]
kobletdata <- kobletdata[kobletdata$Op_gr %in% 6:8, ]

# kobletdata <- merge(fra_krg, RegData, by.x = c("fnr", "operasjonsdato"), by.y = c("SSN", "OperasjonsDato"))

aggdata <- kobletdata %>% #dplyr::group_by(Sykehusnavn) %>%
  dplyr::summarise(
    N = n(),
    gj.sn.alder = mean(Alder),
    median.alder = median(Alder),
    andel.kvinner = sum(erMann==0)/N*100,
    med.bmi = median(BMI, na.rm = T),
    andel.whipple = sum(Op_gr==6)/N*100,
    andel.distal = sum(Op_gr==7)/N*100,
    andel.annet = sum(Op_gr==8)/N*100,
    andel.åpen.konv.distal = sum(Op_gr == 7 & Tilgang %in% c(1,3))/sum(Op_gr == 7)*100,
    andel.åpen.konv.annet = sum(Op_gr == 8 & Tilgang %in% c(1,3))/sum(Op_gr == 8)*100,
    andel.åpen.konv.begge = sum(Op_gr %in% 7:8 & Tilgang %in% c(1,3))/sum(Op_gr %in% 7:8)*100,
    andel.lap.distal = sum(Op_gr == 7 & Tilgang == 2)/sum(Op_gr == 7)*100,
    andel.lap.annet = sum(Op_gr == 8 & Tilgang == 2)/sum(Op_gr == 8)*100,
    andel.lap.begge = sum(Op_gr %in% 7:8 & Tilgang == 2)/sum(Op_gr %in% 7:8)*100,
    andel.portvene.whipple = sum(Op_gr == 6 & Rekonstruksjonstype %in% c(1,3))/sum(Op_gr==6)*100,
    andel.arterie.whipple = sum(Op_gr == 6 & Rekonstruksjonstype %in% c(2,3))/sum(Op_gr==6)*100,
    andel.accord_str_4.whipple = sum(Op_gr == 6 & KumAcc2 == 1)/sum(Op_gr==6)*100,
    andel.accord_str_4.distal = sum(Op_gr == 7 & KumAcc2 == 1)/sum(Op_gr==7)*100,
    andel.accord_str_4.annet = sum(Op_gr == 8 & KumAcc2 == 1)/sum(Op_gr==8)*100,
    andel.accord_str_4.begge = sum(Op_gr %in% 7:8 & KumAcc2 == 1)/sum(Op_gr %in% 7:8)*100,
    andel.relap.whipple = sum(Op_gr == 6 & ReLapNarkose == 1)/sum(Op_gr==6)*100,
    andel.relap.distal = sum(Op_gr == 7 & ReLapNarkose == 1)/sum(Op_gr==7)*100,
    andel.relap.annet = sum(Op_gr == 8 & ReLapNarkose == 1)/sum(Op_gr==8)*100,
    andel.relap.begge = sum(Op_gr %in% 7:8 & ReLapNarkose == 1)/sum(Op_gr %in% 7:8)*100
  )

write.csv2(aggdata, "~/mydata/norgast/krg_pdac_2022.csv",
           row.names = F, fileEncoding = 'Latin1')


##### Beregn DG NoRGast 2022 ###################################################
nprtall <- read.csv2("~/norgast/doc/frekvens_norgast.csv", fileEncoding = "Latin1")
# 974588951 Aker
# 974589095 Ullevål
nprtall$sh_standard[nprtall$sh == 974588951] <- "OUS, Ullevål"
nprtall$sh[nprtall$sh == 974588951] <- 974589095
nprtall <- nprtall %>% group_by(hierarki, sh) %>%
  summarise(hf_standard = first(hf_standard),
            sh_standard = first(sh_standard),
            n = sum(n))

mapping_npr <- read.csv2('~/norgast/doc/Koblingstabell_AvdRESH_sh_standard.csv', fileEncoding = "Latin1")
mapping_npr <- dplyr::bind_rows(mapping_npr,
                                data.frame(AvdRESH = c(4216823),
                                           sh = c(974747545),
                                           sh_standard = c("Helse Møre og Romsdal, Volda")))

nprtall <- merge(nprtall, data.frame(Op_gr = c(8,7,1,5,2,4,6,3),
                                     hierarki = c("Andre_pankreas",
                                                  "Distal_pankreas",
                                                  "Kolon",
                                                  "Lever",
                                                  "Rektum",
                                                  "Ventrikkel",
                                                  "Whipple",
                                                  "Øsofagus")),
                 by = "hierarki", all.x = T)

RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
RegData$AvdRESH[RegData$AvdRESH == 4204126] <- 4204084
RegData <- RegData %>%
  dplyr::filter(Aar == 2022) %>%
  dplyr::filter(Op_gr %in% 1:8) %>%
  merge(mapping_npr[, c("AvdRESH", "sh")], by = "AvdRESH", all.x = T)

mapping_npr <- dplyr::bind_cols(
  mapping_npr,
  "Sykehusnavn"=RegData$SykehusNavn[match(mapping_npr$AvdRESH, RegData$AvdRESH)])

norgasttall <- RegData %>% group_by(Op_gr, sh) %>%
  summarise(N_norgast = n())

samlet <- merge(nprtall, norgasttall, by = c("Op_gr", "sh"), all = T)
samlet$N_norgast[is.na(samlet$N_norgast)] <- 0

pr_shus <- samlet %>% group_by(sh) %>%
  summarise(Sykehus = first(sh_standard),
            n_norgast = sum(N_norgast),
            n_npr = sum(n)) %>%
  janitor::adorn_totals() %>%
  mutate("DG" = n_norgast/n_npr*100)

pr_opgr <- samlet %>% group_by(hierarki) %>%
  summarise(n_norgast = sum(N_norgast),
            n_npr = sum(n)) %>%
  janitor::adorn_totals() %>%
  mutate("DG" = n_norgast/n_npr*100)

write.csv2(pr_shus, "~/mydata/norgast/dg_pr_shus.csv", row.names = F)
write.csv2(pr_opgr, "~/mydata/norgast/dg_pr_opgr.csv", row.names = F)

pr_shus_opgr <- samlet %>% group_by(sh, Op_gr) %>%
  summarise(Sykehus = first(sh_standard),
            n_norgast = sum(N_norgast),
            n_npr = sum(n)) %>%
  mutate("DG" = n_norgast/n_npr*100) %>%
  merge(mapping_npr[,c("sh", "AvdRESH", "Sykehusnavn")], by = "sh", all.x = T)

write.csv2(pr_shus_opgr, "~/mydata/norgast/dg_opgr_shus.csv", row.names = F)

ventrikkel <- samlet %>% group_by(sh, hierarki) %>%
  summarise(Sykehus = first(sh_standard),
            n_norgast = sum(N_norgast),
            n_npr = sum(n)) %>%
  filter(hierarki == "Ventrikkel") %>%
  janitor::adorn_totals() %>%
  mutate("DG" = n_norgast/n_npr*100)

write.csv2(ventrikkel, "~/mydata/ventrikkel.csv", row.names = F)


##### Legg til navn_i_rapportket i klokebok. ###################################
klokebok1 <- readxl::read_xlsx("~/mydata/norgast/Klokeboken NORGAST 2023 Variabelutvalg.xlsx",
                               sheet = 1) %>% select(-"navn_i_rapporteket")
varnavn_kobl <- varnavn_kobl %>% dplyr::mutate(variabel_id = sub("\\.", "_", dbnavn)) %>%
  mutate(navn_i_rapporteket = rapporteket) %>% select(-"rapporteket")

klokebok <- merge(klokebok, varnavn_kobl[,c("variabel_id", "navn_i_rapporteket")], by = "variabel_id", all.x = T)
klokebok <- klokebok[, c("skjemanavn", "navn_i_rapporteket", "ledetekst", "type", "listeverdier",
                         "listetekst", "fysisk_feltnavn", "variabel_id", "hjelpetekst")]

write.csv2(klokebok, "~/mydata/norgast/klokebok_m_rapnavn.csv", row.names = F,
           fileEncoding = "Latin1")

##### Uttrekk Søreide, pankreas 2016-2021, 08.03.2023 ##########################
varnavn_kobl <- data.frame(
  kol = c("mce.MCEID AS ForlopsID",
          "mce.PATIENT_ID AS PasientId",
          "mce.CENTREID AS AvdRESH",
          "patient.SSN AS Fodselsnummer",
          "patient.DECEASED AS Avdod",
          "patient.DECEASED_DATE AS AvdodDato",
          "centre.CENTRENAME AS SenterNavn",
          "registration.PREVIOUS_WEIGHT AS Vekt6MndFoer",
          "registration.PREVIOUS_WEIGHT_MISS AS Vekt6MndFoerUkjent",
          "registration.ADMISSION_WEIGHT AS VektVedInnleggelse",
          "registration.ADMISSION_WEIGHT_MISS AS VektVedInnleggelseUkjent",
          "registration.HEIGHT AS Hoyde",
          "registration.HEIGHT_MISS AS HoydeUkjent",
          "registration.BMI AS BMI",
          "registration.BMI_CATEGORY AS BMIKategori",
          "registration.WEIGHTLOSS AS VekttapProsent",
          "registration.DIABETES AS MedDiabetes",
          "registration.CHEMOTHERAPY_ONLY AS KunCytostatika",
          "registration.RADIATION_THERAPY_ONLY AS KunStraaleterapi",
          "registration.CHEMORADIOTHERAPY AS KjemoRadioKombo",
          "registration.WHO_ECOG_SCORE AS WHOECOG",
          "registration.ALBUMIN AS Albumin",
          "registration.CRP AS CRP",
          "registration.GLASGOW_SCORE AS GlasgowScore",
          "registration.MODIFIED_GLASGOW_SCORE AS ModGlasgowScore",
          "registration.ASA AS ASA",
          "registration.LUNG_DISEASE AS Lungesykdom",
          "registration.HEART_DISEASE AS Hjertesykdom",
          "registration.URGENCY AS Hastegrad",
          "registration.ANESTHESIA_START AS AnestesiStartKl",
          "registration.PRS_SCORE AS PRSScore",
          "registration.OPERATION_DATE AS OpDato",
          "registration.NCSP AS Hovedoperasjon",
          "registration.ABLATION AS LeverAblasjon",
          "registration.RECONSTRUCTION AS Rekonstruksjon",
          "registration.RECONSTRUCTION_TYPE AS Rekonstruksjonstype",
          "registration.ANASTOMOSIS_LEVEL AS Anastomoseniva",
          "registration.ANASTOMOSIS AS NyAnastomose",
          "registration.ANAL_GUARD_DISTANCE AS AvstandAnalVerge",
          "registration.ANAL_GUARD_DISTANCE_MISS AS AvstandAnalVergeIkkeAkt",
          "registration.TATME AS TaTME",
          "registration.OSTOMY AS NyStomi",
          "registration.ABDOMINAL_ACCESS AS Tilgang",
          "registration.ROBOTASSISTANCE AS Robotassistanse",
          "registration.THORAX_ACCESS AS ThoraxTilgang",
          "registration.RELAPAROTOMY AS ReLapNarkose",
          "registration.RELAPAROTOMY_YES AS ViktigsteFunn",
          "registration.FINDINGS_SPESIFISER AS FunnSpesifiser",
          "registration.RELAPAROTOMY_NO AS AnnenOpIAnestsi",
          "registration.INTERVENTION_WITHOUT_ANESTHESIA AS IntUtenAnestesi",
          "registration.PERCUTANEOUS_DRAINAGE AS PerkDrenasje",
          "registration.HIGH_AMYLASE_CONCENTRATION AS HoyAmylaseKons",
          "registration.LEAK_INTERVENTION AS EndoInterLekkasje",
          "registration.BLEED_INTERVENTION AS EndoInterBlod",
          "registration.ANGIO_INTERVENTION AS AngioInter",
          "registration.LIQUID_DRAINAGE AS KunDrenasje",
          "registration.SINGLE_ORGAN_FAILURE AS EttOrganSvikt",
          "registration.MULTI_ORGAN_FAILURE AS MultiOrganSvikt",
          "registration.IN_HOUSE_DEATH AS DodUnderOpphold",
          "registration.IN_HOUSE_DEATH_DATE AS DodUnderOppholdDato",
          "registration.ACCORDION_SCORE AS AccordionGrad",
          "registration.DISCHARGE_DATE AS UtskrivelseDato",
          "registration.BED_DAYS AS PostopLiggedogn",
          "registration.ICD10 AS Hoveddiagnose",
          "registration.DISCHARGE_TO AS UtskrevetTil",
          "registration.STATUS AS RegistreringStatus",
          "readmission.OWN_INSTITUTION AS ReinnlEgenInst",
          "readmission.OTHER_INSTITUTIONS AS ReinnlAndreInst",
          "readmission.CONTROL AS AktivKontroll",
          "readmission.PHYSICAL_CONTROL AS FysiskKontroll",
          "readmission.PHONE_CONTROL AS TelefonKontroll",
          "readmission.RELAPAROTOMY AS OppfReLapNarkose",
          "readmission.RELAPAROTOMY_YES AS OppfViktigsteFunn",
          "readmission.FINDINGS_SPESIFISER AS OppfFunnSpesifiser",
          "readmission.RELAPAROTOMY_NO AS OppfAnnenOpIAnestsi",
          "readmission.INTERVENTION_WITHOUT_ANESTHESIA AS OppfIntUtenAnestesi",
          "readmission.PERCUTANEOUS_DRAINAGE AS OppfPerkDrenasje",
          "readmission.HIGH_AMYLASE_CONCENTRATION AS OppfHoyAmylaseKons",
          "readmission.LEAK_INTERVENTION AS OppfEndoInterLekkasje",
          "readmission.BLEED_INTERVENTION AS OppfEndoInterBlod",
          "readmission.ANGIO_INTERVENTION AS OppfAngioInter",
          "readmission.LIQUID_DRAINAGE AS OppfKunDrenasje",
          "readmission.SINGLE_ORGAN_FAILURE AS OppfEttOrganSvikt",
          "readmission.MULTI_ORGAN_FAILURE AS OppfMultiOrganSvikt",
          "readmission.IN_HOUSE_DEATH AS OppfDodUnderOpphold",
          "readmission.IN_HOUSE_DEATH_DATE AS OppfDodUnderOppholdDato",
          "readmission.ACCORDION_SCORE AS OppfAccordionGrad",
          "readmission.STATUS AS OppfStatus")) %>%
  tidyr::separate(col="kol", into=c("dbnavn", "rapporteket"), sep = " AS ") %>%
  dplyr::mutate(dbnavn = toupper(dbnavn))
# dplyr::as_tibble() %>%
# tidyr::separate(col="dbnavn", into=c("skjema", "dbnavn"), sep = "\\." )

utlevernavn <- readr::read_csv2("~/mydata/norgast/utlevering_soreide.csv")
utlevernavn <- utlevernavn %>% dplyr::mutate(variabel_id = sub("_", ".", variabel_id))

utlevernavn <- utlevernavn$variabel_id %>% unique()
rappnavn_utlevering <- varnavn_kobl %>%
  dplyr::filter(dbnavn %in% utlevernavn)

RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
kobling <- readr::read_csv2("~/mydata/norgast/NoRGast_koblingstabell_datadump_03.03.2023.csv")

utlevering <- RegData %>%
  dplyr::filter(Aar %in% 2016:2021 &
                  ncsp_lowercase %in% c("jlc10", "jlc11", "jlc30", "jlc31")) %>%
  dplyr::select(c(rappnavn_utlevering$rapporteket,
                  "PasientID", "PasientKjonn", "AvdRESH", "SykehusNavn", "ForlopsID", "Alder")) %>%
  merge(kobling, by.x = "PasientID", by.y = "PID") %>%
  mutate(fdato = paste0(substr(SSN, 1,2), ".", substr(SSN, 3,4), ".", substr(SSN, 5,6))) %>%
  select(-"SSN")


write.csv2(utlevering, "~/mydata/norgast/textbook_outcomes_pancreas.csv", row.names = F, fileEncoding = "Latin1")

##### Uttrekk Kolecystectomier OUS 2023 ########################################


utlevernavn <- readr::read_csv2("~/mydata/norgast/varnavn_utlevering_v2.csv")
utlevernavn <- utlevernavn %>% dplyr::mutate(variabel_id = sub("_", ".", variabel_id))

utlevernavn <- utlevernavn$variabel_id %>% unique()

rappnavn_utlevering <- varnavn_kobl %>%
  dplyr::filter(dbnavn %in% utlevernavn)

# setdiff(utlevernavn, rappnavn_utlevering$dbnavn)

RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)

utlevering <- RegData %>%
  dplyr::filter(Aar == 2019 &
                  SykehusNavn %in% c("OUS-Radiumhospitalet", "OUS-Rikshospitalet", "OUS-Ullevål") &
                  ncsp_lowercase %in% c("jka20", "jka21", "jka96", "jka97")) %>%
  dplyr::select(c(rappnavn_utlevering$rapporteket,
                  "PasientID", "PasientKjonn", "AvdRESH", "SykehusNavn", "ForlopsID"))

write.csv2(utlevering, "~/mydata/norgast/kolocytectomi_ous_2019.csv", row.names = F, fileEncoding = "Latin1")

##### v2: fra qreg ##################
registration <- readr::read_csv2("~/mydata/norgast/NoRGast_registration_datadump_03.03.2023.csv")
readmission <- readr::read_csv2("~/mydata/norgast/NoRGast_readmission_datadump_03.03.2023.csv")
mce <- readr::read_csv2("~/mydata/norgast/NoRGast_mce_datadump_03.03.2023.csv")
user <- readr::read_csv2("~/mydata/norgast/NoRGast_user_datadump_03.03.2023.csv")
mce_patient <- readr::read_csv2("~/mydata/norgast/NoRGast_mce_patient_data_datadump_03.03.2023.csv")
kobling <- readr::read_csv2("~/mydata/norgast/NoRGast_koblingstabell_datadump_03.03.2023.csv")

varnavn <- tidyr::separate(data.frame(utlevernavn), col = utlevernavn, into=c("tabell", "varnavn"), sep = "\\.")

# mce_patient <- mce_patient[, varnavn$varnavn[varnavn$tabell=="PATIENT"]]
# registration <- registration[, varnavn$varnavn[varnavn$tabell=="REGISTRATION"]]
# readmission <- readmission[, varnavn$varnavn[varnavn$tabell=="READMISSION"]]

tmp <- merge(utlevering, registration[, c("MCEID", "NCSP_VERSION", "USERCOMMENT")],
             by.x = "ForlopsID", by.y = "MCEID") %>%
  merge(readmission[, c("MCEID", "USERCOMMENT")], by.x = "ForlopsID", by.y = "MCEID",
        suffixes = c("", "_readm")) %>%
  merge(kobling, by.x = "PasientID", by.y = "PID")


write.csv2(tmp, "~/mydata/norgast/kolocytectomi_ous_2019_v2.csv", row.names = F, fileEncoding = "Latin1")


###### DG-sanalyse 2023 - dato uvisst ##########################################
RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
fid <- read.csv2("~/mydata/NoRGast_koblingstabell_datadump_06.02.2023.csv",
                 colClasses = c("integer", "character"))

RegData <- RegData[RegData$Op_gr %in% 1:8 & RegData$Aar == 2022, ]


RegData <- RegData[,c("PasientID", "ForlopsID", "AvdRESH", "Sykehusnavn",
                      "OperasjonsDato", "Operasjonsgrupper", "Hovedoperasjon")]

fid <- fid[fid$PID %in% RegData$PasientID, ]
names(fid) <- c("PasientID", "Fnr")

# write.csv2(RegData, "/home/rstudio/delt_folder/aktivitetsdata_norgast_2021.csv",
#            row.names = F, fileEncoding = 'Latin1')
# write.csv2(fid, "/home/rstudio/delt_folder/kobling_norgast_2021.csv",
#            row.names = F, fileEncoding = 'Latin1')


### Leveranse Lassen 2023-02-08 #####################################

RegData <- rapbase::loadStagingData("norgast", "RegData")

krissdata <- RegData %>% filter(HovedDato >= "2016-01-01" &
                                  HovedDato < "2023-01-01") %>%
  filter(SykehusNavn == "OUS-Rikshospitalet") %>%
  filter(substr(Hoveddiagnose, 1, 3) == "C22")

fnr <- readr::read_csv2("~/mydata/NoRGast_koblingstabell_datadump_06.02.2023.csv")

krissdata <- merge(krissdata, fnr, by.x = "PasientId", by.y = "PID", all.x = T)

utlevering <- krissdata[, c("PasientID", "SSN", "Hoveddiagnose")]

write.csv2(utlevering, "~/mydata/utlev_lassen_08022023.csv", row.names = F, fileEncoding = "Latin1")


### Leveranse Lassen 2022-10-21 #####################################


RegData <- rapbase::loadStagingData("norgast", "RegData")

RegData <- RegData %>% filter(HovedDato >= "2017-01-01" &
                                HovedDato < "2022-07-01") %>%
  filter(Op_gr == 1) %>%
  filter(NyAnastomose == 1) %>%
  filter(NyStomi == 0) %>%
  filter(Hastegrad_hybrid == 1) %>%
  filter(Malign == 1) %>%
  filter(WHOECOG %in% 0:1) %>%
  filter(OppfStatus==1)

RegData$Tilgang_utvidet[RegData$Tilgang_utvidet==5] <- 4
RegData$Tilgang_utvidet <- factor(RegData$Tilgang_utvidet, levels = 1:4,
                                  labels = c("Åpen", "Laparoskopisk", "Laparoskopi med robotassistanse", "Konvertert"))

rater <- RegData %>% group_by(Tilgang_utvidet) %>%
  summarise(n_lekkasje = sum(Anastomoselekkasje),
            N = n()) %>%
  janitor::adorn_totals() %>%
  mutate(Andel = n_lekkasje/N*100)

konf <- binomkonf(rater$n_lekkasje, rater$N)

rater$konf95_lav <- konf[1, ]*100
rater$konf95_hoy <- konf[2, ]*100

write.csv2(rater, "~/.ssh/norgast/rater_norgast_oktober2022_kunferdige.csv", row.names = F, fileEncoding = "Latin1")

##### Stig Norderval, andel malign kolon med robotassistanse 19.04.2022 ########

RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
aux <- RegData[which(RegData$Aar >= 2020 & RegData$AvdRESH == 601225 &
                       RegData$Malign == 1 & RegData$Op_gr == 1 &
                       RegData$Hastegrad == 1 & RegData$Robotassistanse == 1), ]
# fid <- read.csv2("/home/rstudio/.ssh/NoRGast_koblingstabell_datadump_20.04.2022.csv",
#                  colClasses = c("integer", "character"))
# aux <- merge(aux[, c("PasientID", "ForlopsID", "OperasjonsDato", "Hovedoperasjon", "Hoveddiagnose")],
#              fid, by.x = "PasientID", by.y = "PID", all.x = T)

write.csv2(aux[, c("PasientID", "ForlopsID")], "~/delt_folder/robot.csv", row.names = F, fileEncoding = "Latin1")

###### Utlevering kreftregisteret ######################################3

fra_krg <- read.csv2("/home/rstudio/.ssh/pancreas2021_norgast_koblet_KRG.csv",
                     colClasses = c("character", "Date", "numeric"))

fra_krg <- fra_krg[fra_krg$pdac_krg == 1, ]

RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
RegData <- RegData[RegData$Aar %in% 2021, ]
fid <- read.csv2("/home/rstudio/.ssh/NoRGast_koblingstabell_datadump_18.03.2022.csv",
                 colClasses = c("integer", "character"))

RegData <- merge(RegData, fid, by.x = "PasientID", by.y = "PID", all.x = T)

kobletdata <- merge(fra_krg, RegData, by.x = c("fnr", "operasjonsdato"), by.y = c("SSN", "OperasjonsDato"))

aggdata <- kobletdata %>% #dplyr::group_by(Sykehusnavn) %>%
  dplyr::summarise(
    N = n(),
    gj.sn.alder = mean(Alder),
    median.alder = median(Alder),
    andel.kvinner = sum(erMann==0)/N*100,
    med.bmi = median(BMI, na.rm = T),
    andel.whipple = sum(Op_gr==6)/N*100,
    andel.distal = sum(Op_gr==7)/N*100,
    andel.annet = sum(Op_gr==8)/N*100,
    andel.åpen.konv.distal = sum(Op_gr == 7 & Tilgang %in% c(1,3))/sum(Op_gr == 7)*100,
    andel.åpen.konv.annet = sum(Op_gr == 8 & Tilgang %in% c(1,3))/sum(Op_gr == 8)*100,
    andel.åpen.konv.begge = sum(Op_gr %in% 7:8 & Tilgang %in% c(1,3))/sum(Op_gr %in% 7:8)*100,
    andel.lap.distal = sum(Op_gr == 7 & Tilgang == 2)/sum(Op_gr == 7)*100,
    andel.lap.annet = sum(Op_gr == 8 & Tilgang == 2)/sum(Op_gr == 8)*100,
    andel.lap.begge = sum(Op_gr %in% 7:8 & Tilgang == 2)/sum(Op_gr %in% 7:8)*100,
    andel.portvene.whipple = sum(Op_gr == 6 & Rekonstruksjonstype %in% c(1,3))/sum(Op_gr==6)*100,
    andel.arterie.whipple = sum(Op_gr == 6 & Rekonstruksjonstype %in% c(2,3))/sum(Op_gr==6)*100,
    andel.accord_str_4.whipple = sum(Op_gr == 6 & KumAcc2 == 1)/sum(Op_gr==6)*100,
    andel.accord_str_4.distal = sum(Op_gr == 7 & KumAcc2 == 1)/sum(Op_gr==7)*100,
    andel.accord_str_4.annet = sum(Op_gr == 8 & KumAcc2 == 1)/sum(Op_gr==8)*100,
    andel.accord_str_4.begge = sum(Op_gr %in% 7:8 & KumAcc2 == 1)/sum(Op_gr %in% 7:8)*100,
    andel.relap.whipple = sum(Op_gr == 6 & ReLapNarkose == 1)/sum(Op_gr==6)*100,
    andel.relap.distal = sum(Op_gr == 7 & ReLapNarkose == 1)/sum(Op_gr==7)*100,
    andel.relap.annet = sum(Op_gr == 8 & ReLapNarkose == 1)/sum(Op_gr==8)*100,
    andel.relap.begge = sum(Op_gr %in% 7:8 & ReLapNarkose == 1)/sum(Op_gr %in% 7:8)*100
  )

write.csv2(aggdata, "/home/rstudio/delt_folder/krg_pdac_2021.csv",
           row.names = F, fileEncoding = 'Latin1')

# registryName <- "norgast"
# dbType <- "mysql"
# query <- "SELECT * FROM AlleVarNum"
# allevar <- rapbase::loadRegData(registryName, query, dbType)


###### DG- og frafallsanalyse 2021 - 18.03.2022###############################################################
RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
fid <- read.csv2("/home/rstudio/.ssh/NoRGast_koblingstabell_datadump_18.03.2022.csv",
                 colClasses = c("integer", "character"))

RegData <- RegData[RegData$Op_gr %in% 1:8 & RegData$Aar == 2021, ]


RegData <- RegData[,c("PasientID", "ForlopsID", "AvdRESH", "Sykehusnavn",
                      "OperasjonsDato", "Operasjonsgrupper", "Hovedoperasjon")]

fid <- fid[fid$PID %in% RegData$PasientID, ]
names(fid) <- c("PasientID", "Fnr")

write.csv2(RegData, "/home/rstudio/delt_folder/aktivitetsdata_norgast_2021.csv",
           row.names = F, fileEncoding = 'Latin1')
write.csv2(fid, "/home/rstudio/delt_folder/kobling_norgast_2021.csv",
           row.names = F, fileEncoding = 'Latin1')

############ Sårruptur Stavanger ###############################################################
# Hei Kevin,
#
# Stavanger bruker Rapporteket aktivt i sin kvalitetsforbedring og stusser over egne tall for sårruptur i 2020 og 2021 (betydelig høyere enn forventet).
#
# Kan du lage en kryptert liste over personnummer + operasjonsdato for de som tilfredsstiller
# Operasjonsdato 2020 + 2021
# Stavanger
# Åpent eller konvertert inngrep
# Sårruptur som hovedfunn ved reoperasjon.

RegData <- norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
fid <- read.csv2("/home/rstudio/delt_folder/NoRGast_koblingstabell_datadump_2021-11-30.csv",
                 colClasses = c("integer", "character"))

utdata <- RegData[RegData$AvdRESH == 114271 & RegData$Aar %in% 2020:2021 & RegData$Saarruptur == 1, ] %>%
  merge(fid, by.x = "PasientID", by.y = "PID", all.x = TRUE)
utdata <- utdata[, c("SSN", "OperasjonsDato")]
write.csv2(utdata, "/home/rstudio/delt_folder/saarruptur_stavanger.csv", row.names = F, fileEncoding = "Latin1")


############ Finn pasientID - Dille-Andam 29.09.2021 ##########################################

forlopsliste <- read.csv2("/home/rstudio/delt_folder/Datadump_NoRGast 2021 lever og pankreas V3.csv")
pid <- forlopsliste[, c("PasientID", "ForlopsID", "AvdRESH", "Sykehusnavn")]
fid <- read.csv2("/home/rstudio/delt_folder/NoRGast_koblingstabell_datadump_2021-10-01.csv",
                 colClasses = c("integer", "character"))

kobl <- merge(pid, fid, by.x = "PasientID", by.y = "PID", all.x = TRUE)
kobl$Sykehusnavn[kobl$AvdRESH == 601225] <- "UNN-Tromsø"

write.csv2(kobl[which(kobl$AvdRESH == 700922), c("PasientID", "ForlopsID", "SSN")],
           "/home/rstudio/delt_folder/Haukeland.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(kobl[which(kobl$AvdRESH %in% c(103312, 700413)), c("PasientID", "ForlopsID", "SSN")],
           "/home/rstudio/delt_folder/OUS.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(kobl[which(kobl$AvdRESH == 107440), c("PasientID", "ForlopsID", "SSN")],
           "/home/rstudio/delt_folder/StOlavs.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(kobl[which(kobl$AvdRESH == 114271), c("PasientID", "ForlopsID", "SSN")],
           "/home/rstudio/delt_folder/Stavanger.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(kobl[which(kobl$AvdRESH == 601225), c("PasientID", "ForlopsID", "SSN")],
           "/home/rstudio/delt_folder/UNNTromso.csv", row.names = F, fileEncoding = "Latin1")

##### Tall til Kristoffer 30.03.2021 ####################################################
RegData <- read.table('I:/norgast/AlleVarNum2021-03-25 14-46-40.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2021-03-25 14-46-40.txt', header=TRUE, sep=";",
                         encoding = 'UTF-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad",
                      "AvstandAnalVerge")]
names(ForlopData)[match(c("SykehusNavn", "erMann"), names(ForlopData))] <- c("Sykehusnavn", "ErMann")
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)

utvalg <- NorgastUtvalg(RegData=RegData, datoFra = '2018-01-01', datoTil = '2020-12-31', whoEcog = c(0, 1),
                        op_gruppe = 1)
PlotParams <- NorgastPrepVar(RegData=utvalg$RegData, valgtVar="Anastomoselekkasje", enhetsUtvalg=0)
RegData_lekk <- PlotParams$RegData
PlotParams$RegData <- NA
andel_lekk <- sum(RegData_lekk$Variabel)/length(RegData_lekk$Variabel)*100
konf_lekk <- binomkonf(sum(RegData_lekk$Variabel), length(RegData_lekk$Variabel), konfnivaa = .9)*100

PlotParams <- NorgastPrepVar(RegData=utvalg$RegData, valgtVar="KumAcc2", enhetsUtvalg=0)
RegData_acc <- PlotParams$RegData
RegData_acc <- RegData_acc[which(RegData_acc$NyAnastomose==1), ]
PlotParams$RegData <- NA
andel_acc <- sum(RegData_acc$Variabel)/length(RegData_acc$Variabel)*100
konf_acc <- binomkonf(sum(RegData_acc$Variabel), length(RegData_acc$Variabel), konfnivaa = .9)*100

##### Feilsøk etter flytting av tilhørighet OUS-pasienter 11.03.2021 ####################################################
RegData <- read.table('I:/norgast/AlleVarNum2021-03-11 09-25-01.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2021-03-11 09-25-01.txt', header=TRUE, sep=";",
                         encoding = 'UTF-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad",
                      "AvstandAnalVerge")]
names(ForlopData)[match(c("SykehusNavn", "erMann"), names(ForlopData))] <- c("Sykehusnavn", "ErMann")
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess_behold_kladd(RegData)
RegData <- RegData[which(RegData$RegistreringStatus==1),]
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
RegData_ny <-  RegData

RegData <- read.table('I:/norgast/AlleVarNum2021-02-15 10-00-19.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2021-02-15 10-00-19.txt', header=TRUE, sep=";",
                         encoding = 'UTF-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad",
                      "AvstandAnalVerge")]
names(ForlopData)[names(ForlopData) %in% c("SykehusNavn", "erMann")] <- c("Sykehusnavn", "ErMann")
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess_behold_kladd(RegData)
RegData <- RegData[which(RegData$RegistreringStatus==1),]
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)

RegData <- merge(RegData[, c("ForlopsID", "HovedDato", "Op_gr", "Operasjonsgrupper", "Hovedoperasjon", "AvdRESH",
                             "Sykehusnavn")], RegData_ny[, c("ForlopsID", "Op_gr", "AvdRESH", "Sykehusnavn")], by = "ForlopsID",
                 suffixes = c("", "_ny"))

tmp <- as.data.frame.matrix(table(RegData$Sykehusnavn[RegData$Sykehusnavn %in% c("OUS", "OUS-Radiumhospitalet", "OUS-Rikshospitalet")],
                                  RegData$Sykehusnavn_ny[RegData$Sykehusnavn %in% c("OUS", "OUS-Radiumhospitalet", "OUS-Rikshospitalet")],
                                  useNA = 'ifany'))
tmp2 <- as.data.frame.matrix(table(RegData$AvdRESH[RegData$Sykehusnavn %in% c("OUS", "OUS-Radiumhospitalet", "OUS-Rikshospitalet")],
                                   RegData$AvdRESH_ny[RegData$Sykehusnavn %in% c("OUS", "OUS-Radiumhospitalet", "OUS-Rikshospitalet")],
                                   useNA = 'ifany'))


write.csv2(tmp, "I:/norgast/konvertering.csv")


###### Feilsøk jmfr. e-post Kristoffer 15.02.2021 #################################
mangler <- c(31533, 31750, 31865, 13526, 34068, 34348, 37090, 37223)

RegData <- read.table('I:/norgast/NoRGast_AlleVarNum_datadump_2021-02-16.csv', header=TRUE, sep=";",
                      fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/NoRGast_ForlopsOversikt_datadump_2021-02-16.csv', header=TRUE, sep=";",
                         fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad",
                      "AvstandAnalVerge")]
names(ForlopData)[names(ForlopData) %in% c("SykehusNavn", "erMann")] <- c("Sykehusnavn", "ErMann")
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")

skjemaoversikt <- read.table('I:/norgast/NoRGast_SkjemaOversikt_datadump_2021-02-16.csv', header=TRUE, sep=';',
                             stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)

kobl_tab <- read.table('I:/norgast/NoRGast_koblingstabell_datadump_2021-02-16.csv', header=TRUE, sep=';',
                       stringsAsFactors = F, fileEncoding = 'UTF-8-BOM', colClasses = "character")
RegDataOUS <- RegData[RegData$Sykehusnavn %in% c("OUS", "OUS-Radiumhospitalet", "OUS-Rikshospitalet"), ]
RegDataOUS <- RegDataOUS[which(tolower(substr(RegDataOUS$Hovedoperasjon, 1, 3)) == "jjb"), ]
# RegDataOUS <- RegDataOUS[which(as.Date(RegDataOUS$HovedDato) >= "2020-01-01" &
#                                  as.Date(RegDataOUS$HovedDato) <= "2020-12-31" &
#                                  RegDataOUS$RegistreringStatus == 1), ]
RegDataOUS <- RegDataOUS[which(as.Date(RegDataOUS$HovedDato) >= "2020-01-01" &
                                 as.Date(RegDataOUS$HovedDato) <= "2020-12-31"), ]
tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering', c("ForlopsID", "SkjemaStatus", "HovedDato", "OpprettetDato", "Sykehusnavn", "AvdRESH")],
             skjemaoversikt[skjemaoversikt$Skjemanavn=='Reinnleggelse/oppføl', c("ForlopsID", "SkjemaStatus", "Sykehusnavn")],
             by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))
tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
tmp$HovedDato[is.na(tmp$HovedDato)] <- tmp$OpprettetDato[is.na(tmp$HovedDato)]
tmp <- merge(tmp, RegDataOUS[,c("ForlopsID", "Hovedoperasjon", "PasientID", "OpDato")], by = "ForlopsID", all.x = T)

tmp <- tmp[!is.na(tmp$Hovedoperasjon), ]
tmp <- tmp[which(tmp$HovedDato >= "2020-01-01" & tmp$HovedDato <= "2020-12-31"), ]
table(tmp$Sykehusnavn)

mpnr <- merge(tmp, kobl_tab, by.x = "PasientID", by.y = "PID", all.x = T)
utlevering <- mpnr[, c("SSN", "OpDato", "Hovedoperasjon", "Sykehusnavn", "AvdRESH")]
names(utlevering)[1] <- "Fnr"

write.csv2(utlevering, "I:/norgast/NoRGast_lever_OUS_2020.csv", row.names = F, fileEncoding = "Latin1")


### 05.02.2021 - Antall jfb koder i Sandefjord 2019 ##########################
library(tidyverse)
skjemaoversikt <- read.table('I:/norgast/SkjemaOversikt2021-02-03 15-28-13.txt', header=TRUE, sep=';',
                             stringsAsFactors = F, encoding = 'UTF-8')
RegData <- read.table('I:/norgast/AlleVarNum2021-02-03 15-28-13.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering', c("ForlopsID", "SkjemaStatus", "HovedDato", "OpprettetDato", "Sykehusnavn", "AvdRESH")],
             skjemaoversikt[skjemaoversikt$Skjemanavn=='Reinnleggelse/oppføl', c("ForlopsID", "SkjemaStatus")],
             by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))

tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
tmp$HovedDato[is.na(tmp$HovedDato)] <- tmp$OpprettetDato[is.na(tmp$HovedDato)]
tmp <- merge(tmp, RegData[,c("ForlopsID", "Hovedoperasjon")], by = "ForlopsID", all.x = T)
tmp2 <- tmp[which(tolower(substr(tmp$Hovedoperasjon, 1, 3)) == "jfb"), ]

aux <- tmp %>% filter(HovedDato >= "2019-01-01" & HovedDato <= "2019-12-31") %>%
  group_by(Sykehusnavn) %>% summarise('Ferdige forløp' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==1, na.rm = T),
                                      'Oppfølging i kladd' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==0, na.rm = T),
                                      'Ferdig basisreg. oppfølging mangler' = sum(SkjemaStatus==1 & is.na(SkjemaStatus_oppf), na.rm = T),
                                      'Basisreg i kladd' = sum(SkjemaStatus==0, na.rm = T),
                                      'N' = n())
aux2 <- tmp2 %>% filter(HovedDato >= "2019-01-01" & HovedDato <= "2019-12-31") %>%
  group_by(Sykehusnavn) %>% summarise('Ferdige forløp' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==1, na.rm = T),
                                      'Oppfølging i kladd' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==0, na.rm = T),
                                      'Ferdig basisreg. oppfølging mangler' = sum(SkjemaStatus==1 & is.na(SkjemaStatus_oppf), na.rm = T),
                                      'Basisreg i kladd' = sum(SkjemaStatus==0, na.rm = T),
                                      'N' = n())

ant_skjema <- bind_rows(aux, aux2)

ant_skjema[ant_skjema$Sykehusnavn == "HS-Sandnessjøen", ]




###### Testdata ifm. dataprodukter. Aksel 14.01.2021 ##############################
library(tidyverse)
AlleVarNum <- read.table('I:/norgast/AlleVarNum2021-01-14 13-59-17.txt', header=TRUE, sep=";",
                         encoding = 'UTF-8', stringsAsFactors = F)
AlleVar <- read.table('I:/norgast/AlleVar2021-01-14 13-59-17.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
ForlopOversikt <- read.table('I:/norgast/ForlopsOversikt2021-01-14 13-59-17.txt', header=TRUE, sep=";",
                             encoding = 'UTF-8', stringsAsFactors = F)
SkjemaOversikt <- read.table('I:/norgast/SkjemaOversikt2021-01-14 13-59-17.txt', header=TRUE, sep=';',
                             stringsAsFactors = F, encoding = 'UTF-8')

AlleVarNum <- apply(AlleVarNum, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
AlleVar <- apply(AlleVar, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
ForlopOversikt <- apply(ForlopOversikt, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
SkjemaOversikt <- apply(SkjemaOversikt, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()

write.csv2(AlleVar, "I:/norgast/AlleVar.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(AlleVarNum, "I:/norgast/AlleVarNum.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(ForlopOversikt, "I:/norgast/ForlopOversikt.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(SkjemaOversikt, "I:/norgast/SkjemaOversikt.csv", row.names = F, fileEncoding = "Latin1")



##### Linn 26.06.2020 - kvalitetssikring strålingstall ############################
library(tidyverse)
RegData <- read.table('I:/norgast/AlleVarNum2020-04-02 16-59-12.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2020-04-02 16-59-12.txt', header=TRUE, sep=";", encoding = 'UTF-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad",
                      "Rekonstruksjon", "Rekonstruksjonstype", "EndoInterLekkasje", "EndoInterBlod", "PerkDrenasje",
                      "HoyAmylaseKons", "AvstandAnalVerge", "KunDrenasje", "TelefonKontroll", "FysiskKontroll", "PostopLiggedogn")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus',
                            'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegDataAll <- NorgastPreprosess(RegData)
RegData <- RegDataAll[which(RegDataAll$Op_gr==2 & RegDataAll$Malign==1), ]

aux <- RegData %>% group_by(Sykehusnavn, Aar) %>%
  summarise(antall_kun_cytostatika = sum(KunCytostatika),
            antall_kun_straaling = sum(KunStraaleterapi),
            antall_kombo = sum(KjemoRadioKombo),
            antall_ingen = sum(KunCytostatika==0  & KunStraaleterapi==0 & KjemoRadioKombo==0),
            N=n())

write.csv2(aux, 'forbehandling.csv', row.names = F, fileEncoding = 'Latin1')

aux$straaling <- paste0(aux$antall_kun_straaling + aux$antall_kombo, ' (', aux$N, ')')
aux$kun_cytostatika <- paste0(aux$antall_kun_cytostatika, ' (', aux$N, ')')
aux$kun_straaling <- paste0(aux$antall_kun_straaling, ' (', aux$N, ')')
aux$ingen <- paste0(aux$antall_ingen, ' (', aux$N, ')')
straaling <- aux[,c(1,2,8)] %>% spread(key=Aar, value = straaling, fill = '')
kun_straaling <- aux[,c(1,2,10)] %>% spread(key=Aar, value = kun_straaling, fill = '')
kun_cytostatika <- aux[,c(1,2,9)] %>% spread(key=Aar, value = kun_cytostatika, fill = '')
ingen <- aux[,c(1,2,11)] %>% spread(key=Aar, value = ingen, fill = '')

write.csv2(straaling, 'straaling.csv', row.names = F, fileEncoding = 'Latin1')
write.csv2(kun_straaling, 'kun_straaling.csv', row.names = F, fileEncoding = 'Latin1')
write.csv2(kun_cytostatika, 'kun_cytostatika.csv', row.names = F, fileEncoding = 'Latin1')
write.csv2(ingen, 'ingen_forbehandling.csv', row.names = F, fileEncoding = 'Latin1')

##### St.Olavs 02.06.2020 - postopliggetid Per Even Storli ####################################################
library(tidyverse)
RegData <- read.table('I:/norgast/AlleVarNum2020-04-02 16-59-12.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2020-04-02 16-59-12.txt', header=TRUE, sep=";", encoding = 'UTF-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad",
                      "Rekonstruksjon", "Rekonstruksjonstype", "EndoInterLekkasje", "EndoInterBlod", "PerkDrenasje",
                      "HoyAmylaseKons", "AvstandAnalVerge", "KunDrenasje", "TelefonKontroll", "FysiskKontroll", "PostopLiggedogn")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus',
                            'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegDataAll <- NorgastPreprosess(RegData)
RegData <- RegDataAll[RegDataAll$Sykehusnavn == 'St.Olavs' & RegDataAll$Aar < 2020, ]
RegData <- RegData[(RegData$Op_gr == 3 & RegData$Malign==1) | (RegData$Op_gr == 4 & RegData$Malign==1) | RegData$Op_gr %in% c(5,6), ]

tabell <- RegData %>% group_by(Aar, Operasjonsgrupper) %>% summarise(gj.sn.liggetid = sum(PostopLiggedogn, na.rm = T),
                                                                     N = sum(!is.na(PostopLiggedogn)))

tabell$gj.sn.liggetid <- paste0(round(tabell$gj.sn.liggetid, 1), ' (', tabell$N, ')')
spread(tabell[,-4], key = Aar, value = gj.sn.liggetid)

RegData <- RegDataAll[RegDataAll$Sykehusnavn != 'St.Olavs' & RegDataAll$Aar < 2020, ]
RegData <- RegData[(RegData$Op_gr == 3 & RegData$Malign==1) | (RegData$Op_gr == 4 & RegData$Malign==1) | RegData$Op_gr %in% c(5,6), ]

tabell <- RegData %>% group_by(Aar, Operasjonsgrupper) %>% summarise(gj.sn.liggetid = sum(PostopLiggedogn, na.rm = T),
                                                                     N = sum(!is.na(PostopLiggedogn)))

tabell$gj.sn.liggetid <- paste0(round(tabell$gj.sn.liggetid, 1), ' (', tabell$N, ')')
spread(tabell[,-4], key = Aar, value = gj.sn.liggetid)

##### Alta 20.05.2020 - saarruptur ####################################################

RegData <- read.table('I:/norgast/AlleVarNum2020-04-02 16-59-12.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2020-04-02 16-59-12.txt', header=TRUE, sep=";", encoding = 'UTF-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad",
                      "Rekonstruksjon", "Rekonstruksjonstype", "EndoInterLekkasje", "EndoInterBlod", "PerkDrenasje",
                      "HoyAmylaseKons", "AvstandAnalVerge", "KunDrenasje", "TelefonKontroll", "FysiskKontroll")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)

saarruptur <- RegData[RegData$Aar==2018 & RegData$Sykehusnavn=='Hammerfest' & RegData$Saarruptur==1, ]
saarruptur2 <- RegData[RegData$Aar %in% 2016:2018 & RegData$Sykehusnavn=='Hammerfest' & RegData$Op_gr %in% 1:7 &
                         RegData$Hastegrad_tid==1 & RegData$Tilgang %in% c(1,3), ]

acc3pluss <- RegData[RegData$Aar==2018 & RegData$Sykehusnavn=='Hammerfest' & RegData$AccordionGrad>=3 &
                       RegData$Malign==1, ]


###### Undersøk Haugesund robotassistert lap 25.03.2020 ###########################
RegData <- read.table('I:/norgast/AlleVarNum2020-03-11 14-52-26.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2020-03-11 14-52-26.txt', header=TRUE, sep=";",
                         encoding = 'UTF-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)

enhetsliste <- RegData[match(unique(RegData$AvdRESH), RegData$AvdRESH), c("AvdRESH", "Sykehusnavn")]

aux <- RegData[which(RegData$Sykehusnavn == 'Haugesund' & RegData$Aar == 2019 & RegData$Op_gr == 2 & RegData$Robotassistanse == 1), ]
aux[,c("PasientID", "ForlopsID")]


###### Data til dekningsgradsanalyse NPR, 17.03.2020 St. Paddy's #########################################################################

RegData <- read.table('I:/norgast/AlleVarNum2020-03-11 14-52-26.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2020-03-11 14-52-26.txt', header=TRUE, sep=";",
                         encoding = 'UTF-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]

ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
# RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
# RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
RegData$op_gr_npr <- 'Annet'
RegData$ncsp_lowercase <- substr(tolower(RegData$Hovedoperasjon), 1, 5)
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jfh")] <- "Kolonreseksjoner"
RegData$op_gr_npr[intersect(which(substr(RegData$ncsp_lowercase,1,3)=="jfb"),
                            which(as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% 20:64))] <- "Kolonreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jgb")] <- "Rektumreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jcc")] <- "Øsofagusreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jdc")] <- "Ventrikkelreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jdd")] <- "Ventrikkelreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jjb")] <- "Leverreseksjoner"
RegData$op_gr_npr[intersect(which(substr(RegData$ncsp_lowercase,1,3)=="jlc"),
                            which(as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% c(0:40, 96)))] <- "Pankreasreseksjoner"
RegData <- RegData[which(RegData$op_gr_npr != 'Annet'), ]
RegData$Aar <- format(as.Date(RegData$HovedDato), '%Y')
RegData <- RegData[RegData$Aar == 2019,  ]
RegData <- RegData[RegData$BasisRegStatus == 1, ]

library(tidyverse)
tmp <- RegData %>% group_by(PasientID, HovedDato, Hovedoperasjon) %>% summarise(antall = n(),
                                                                                ForlopsID = ForlopsID[1])
tmp <- tmp[tmp$antall>1, ]
RegData <- RegData[!(RegData$ForlopsID %in% tmp$ForlopsID), ] # fjern dobbelreg
RegData <- RegData[, c("PasientID", "ForlopsID", "HovedDato", "Hovedoperasjon", "op_gr_npr", "AvdRESH", "Sykehusnavn")]

kobling <- read.table('I:/norgast/NORGAST-334_AllenorgastPasienter_2020-03-11.csv', header=TRUE, sep=",",
                      encoding = 'UTF-8', stringsAsFactors = F, colClasses = c('integer', 'character'))

names(kobling)[1] <- 'PasientID'
kobling <- kobling[kobling$PasientID %in% unique(RegData$PasientID), ]

write.csv2(kobling, 'I:/norgast/koblingsfil_norgast_2019.csv', row.names = F)
write.csv2(RegData, 'I:/norgast/aktivitetsdata_norgast_2019.csv', row.names = F)



###### Kristoffer Avdøde innen 90 dager, OUS, lever og pankreasreseksjoner 2016-2019, Utlevert 11.03.2020 ################

RegData <- read.table('I:/norgast/AlleVarNum2020-03-11 14-52-26.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2020-03-11 14-52-26.txt', header=TRUE, sep=";",
                         encoding = 'UTF-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)

kobling <- read.table('I:/norgast/NORGAST-334_AllenorgastPasienter_2020-03-11.csv', header=TRUE, sep=",",
                      encoding = 'UTF-8', stringsAsFactors = F, colClasses = c('integer', 'character'))


uttrekk <- RegData[which(RegData$AvdRESH == 700413 & RegData$Avdod == 1 & RegData$Op_gr %in% 5:7 & RegData$Aar %in% 2016:2019), ]
uttrekk <- uttrekk[order(uttrekk$OperasjonsDato, decreasing = F), ] # Sorter slik at man velger eldste operasjon når flere
uttrekk <- uttrekk[match(unique(uttrekk$PasientID), uttrekk$PasientID), ]
uttrekk$Avdod90 <- 0
uttrekk$Avdod90[which(uttrekk$OpDoedTid <= 90 & uttrekk$OpDoedTid >= 0)] <- 1
uttrekk <- uttrekk[which(uttrekk$Avdod90 == 1), ]
uttrekk <- merge(uttrekk, kobling, by.x = 'PasientID', by.y = 'PID')
uttrekk <- uttrekk [, c("Fnr", "PasientID", "ForlopsID", "OperasjonsDato", "DoedsDato", "Hovedoperasjon")]
write.csv2(uttrekk, 'I:/norgast/norgast_avdod90_OUS_2016-2019.csv', row.names = F, fileEncoding = 'Latin1')


############# Kristiansand 2019 - Utlevering 19.11.2019 ###############################

RegData <- read.table('I:/norgast/AlleVarNum2019-11-19 10-04-46.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2019-11-19 10-05-04.txt', header=TRUE, sep=";",
                         encoding = 'UTF-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- RegData[which(RegData$AvdRESH == 100353), ]
RegData <- RegData[as.Date(RegData$HovedDato) >= "2019-01-01", ]
RegData <- NorgastPreprosess(RegData)

RegData <- RegData[RegData$Op_gr %in% 1:7, c("OpDato", "erMann", "Alder", "Hovedoperasjon")]

write.csv2(RegData, 'I:/norgast/norgast_kristiansand2019.csv', row.names = F)


############# Stig Norderval - Utlevering 27.08.2019 ###############################
koblingsinfo <- read.table('I:/norgast/AlleNorgastPasienterAugsti2019.csv', header=TRUE, sep=",", colClasses = c('integer', 'character'))
koblingsinfo <- koblingsinfo[match(unique(koblingsinfo$PID), koblingsinfo$PID), ]

RegData <- read.table('I:/norgast/AlleVarNum2019-06-27 12-09-02.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2019-06-27 12-09-18.txt', header=TRUE, sep=";",
                         encoding = 'UTF-8', stringsAsFactors = F)
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- RegData[which(RegData$BasisRegStatus == 1), ]
RegData <- RegData[which(substr(tolower(RegData$Hovedoperasjon),1,3)=="jgb"), ]
RegData <- RegData[which(substr(RegData$Hoveddiagnose, 1, 1) == 'C'), ]

RegDataNum <- merge(RegData, koblingsinfo, by.x = "PasientID", by.y = "PID", all.x = T)
RegDataNum <- RegDataNum[which(as.Date(RegDataNum$HovedDato) <= "2018-12-31"), ]

RegData <- read.table('I:/norgast/AlleVar2019-06-27 12-08-46.txt', header=TRUE, sep=";",
                      encoding = 'UTF-8', stringsAsFactors = F)
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegDataLabel <- merge(RegData, koblingsinfo, by.x = "PasientID", by.y = "PID", all.x = T)
RegDataLabel <- RegDataLabel[which(RegDataLabel$ForlopsID %in% RegDataNum$ForlopsID), ]

write.csv2(RegDataNum, 'norgastdata_num_27082019.csv', row.names = F)
write.csv2(RegDataLabel, 'norgastdata_label_27082019.csv', row.names = F)

############# Fagråd - Innhold av Annet under årsak til reoperasjon 16.06.2019 ###############################

RegData <- read.table('I:/norgast/AlleVarNum2019-06-12 09-09-03.txt', header=TRUE, sep=";",
                      encoding = 'UFT-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2019-06-12 09-09-17.txt', header=TRUE, sep=";",
                         encoding = 'UFT-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', 'OppfAnnenOpIAnestsi', 'AnnenOpIAnestsi')]

ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)

RegData <- RegData[RegData$Aar %in% c(2017, 2018), ]
RegData <- RegData[which(RegData$ViktigsteFunn==5), ]

Utlevering <- RegData[, c("PasientID", "OperasjonsDato", "Sykehusnavn")]
Utlevering <- Utlevering[order(Utlevering$Sykehusnavn), ]

write.csv2(Utlevering, 'reopererte_funn_annet.csv', row.names = F)

####### Tall til dekningsgradsanalyse NPR 2018 ##########################################

persondata <- read.csv('I:/norgast/AlleNorgastPasienterApril2019.csv', colClasses = "character")
persondata$PID <- as.numeric(persondata$PID)
# RegData <- read.table('I:/norgast/AlleVarNum2019-04-04 15-53-26.txt', header=TRUE, sep=";",
#                       encoding = 'UFT-8', stringsAsFactors = F)
# RegData$OpDato <- as.Date(RegData$OpDato)
# RegData$Aar <- format(RegData$OpDato, '%Y')
# RegData <- RegData[RegData$Aar == 2018 & RegData$RegistreringStatus==1, ]
# RegData <- RegData[, c("PasientId", "OpDato", "Hovedoperasjon", "AvdRESH", "SenterNavn")]
# RegData$Hovedoperasjon <- substr(RegData$Hovedoperasjon, 1,5)
#

# setdiff(RegData$PasientId, persondata$PID)
# setdiff(persondata$PID, RegData$PasientId)
# RegData[RegData$PasientId %in% setdiff(RegData$PasientId, persondata$PID), "RegistreringStatus"]
# RegData[RegData$PasientId %in% c(22455, 22458, 22460, 22464), "RegistreringStatus"]

RegData <- read.table('I:/norgast/AlleVarNum2019-04-11 09-02-00.txt', header=TRUE, sep=";",
                      encoding = 'UFT-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2019-04-11 08-59-44.txt', header=TRUE, sep=";",
                         encoding = 'UFT-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]

ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
RegData$op_gr_npr <- 'Annet'
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jfh")] <- "Kolonreseksjoner"
RegData$op_gr_npr[intersect(which(substr(RegData$ncsp_lowercase,1,3)=="jfb"),
                            which(as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% 20:64))] <- "Kolonreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jgb")] <- "Rektumreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jcc")] <- "Øsofagusreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jdc")] <- "Ventrikkelreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jdd")] <- "Ventrikkelreseksjoner"
RegData$op_gr_npr[which(substr(RegData$ncsp_lowercase,1,3)=="jjb")] <- "Leverreseksjoner"
RegData$op_gr_npr[intersect(which(substr(RegData$ncsp_lowercase,1,3)=="jlc"),
                            which(as.numeric(substr(RegData$ncsp_lowercase,4,5)) %in% c(0:40, 96)))] <- "Pankreasreseksjoner"

RegData <- RegData[RegData$Aar == 2018 & RegData$RegistreringStatus==1, ]
RegData <- RegData[RegData$op_gr_npr != 'Annet', ]
RegData <- RegData[, c("PasientID", "HovedDato", "Hovedoperasjon", "op_gr_npr", "AvdRESH", "Sykehusnavn")]
RegData$Hovedoperasjon <- substr(RegData$Hovedoperasjon, 1,5)
persondata <- persondata[persondata$PID %in% unique(RegData$PasientID), ]
persondata <- persondata[match(unique(persondata$PID), persondata$PID), ]
persondata <- persondata[, c("Fnr", "PID")]

write.csv2(persondata, 'I:/norgast/koblingsfil_norgast.csv', row.names = F)
write.csv2(RegData, 'I:/norgast/aktivitetsdata_norgast.csv', row.names = F)


NoRGastObligOperasjoner2018 <- as.data.frame(addmargins(table(RegData[, c('Sykehusnavn', 'op_gr_npr')], useNA = 'ifany')))
NoRGastObligOperasjoner2018 <- tidyr::spread(data = NoRGastObligOperasjoner2018,key = op_gr_npr, value = Freq)
NoRGastObligOperasjoner2018$AvdRESH <- RegData$AvdRESH[match(NoRGastObligOperasjoner2018$Sykehusnavn, RegData$Sykehusnavn)]
NoRGastObligOperasjoner2018 <- NoRGastObligOperasjoner2018[, c(dim(NoRGastObligOperasjoner2018)[2],1:(dim(NoRGastObligOperasjoner2018)[2]-1))]

write.csv2(NoRGastObligOperasjoner2018, 'I:/norgast/norgast_telling.csv', row.names = F)





###### Stig Norderval 08.02.2019 Pasienter ved UNN - Tromsø i NoRGast med rektum elektiv, malign 2016 og 2017 #########
RegData <- read.table('I:/norgast/AlleVarNum2018-12-20 12-31-21.txt', header=TRUE, sep=";",
                      encoding = 'UFT-8', stringsAsFactors = F)
ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-12-20 12-31-43.txt', header=TRUE, sep=";",
                         encoding = 'UFT-8', stringsAsFactors = F)

RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose'
                      , "Hastegrad")]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)

RegData <- RegData[RegData$Aar %in% c(2016,2017), ]
RegData <- RegData[which(RegData$Malign == 1 & RegData$Op_gr == 2 & RegData$Sykehusnavn == 'UNN-Tromsø'), ]


write.csv2(RegData[, c("ForlopsID", "PasientID", "OperasjonsDato", "Hovedoperasjon", "Hoveddiagnose")],
           'stig08022019.csv', row.names = F)


### Trond Dehli - 21.09.2018 Liste av PID robotassisterte op. ved St. Olavs ######################################################
RegData <- read.table('I:/norgast/AlleVariablerNum2018-09-21 08-38-12.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-09-21 08-38-23.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData <- RegData[RegData$AvdRESH == 601225 & RegData$HovedDato >= '2017-01-01' & RegData$HovedDato <= '2017-12-31', ]
RegData <- RegData[which(substr(RegData$Hoveddiagnose, 1, 3) %in% c('C18', 'C19')), ]
RegData <- RegData[which(substr(RegData$ncsp_lowercase,1,3) %in% c("jfb", 'jfh')), ]

RegData <- RegData[, c("Alder", "erMann", "OperasjonsDato", "Hovedoperasjon")]

write.csv2(RegData, 'UtleveringColoncancer.csv', row.names = F)


### Linn - 12.09.2018 Liste av PID robotassisterte op. ved St. Olavs ######################################################
RegData <- read.table('I:/norgast/AlleVariablerNum2018-09-12 08-52-43.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-09-12 08-52-54.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData <- RegData[RegData$Sykehusnavn=='St.Olavs', ]

RegData[which(RegData$Robotassistanse==1), c("PasientID", "ForlopsID")]



### Linn - 18.06.2018 Liste av PID og op.dato for utvalgte op.koder ######################################################
RegData <- read.table('I:/norgast/AlleVariablerNum2018-06-14 09-40-44.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-06-14 09-40-55.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData <- RegData[RegData$AvdRESH==601225, ]

RegData <- RegData[which(substr(RegData$ncsp_lowercase, 1, 3) %in% 'jfb'), ]
RegData <- RegData[which(as.numeric(substr(RegData$ncsp_lowercase, 4, 5)) %in% 21:54), ]
RegData <- RegData[RegData$Aar < 2018, ]

utlevering <- RegData[, c("PasientID", "OperasjonsDato")]
write.csv2(utlevering, 'Utlevering_Linn_18.06.2018.csv', row.names = F)



### Stig Norderval - 14.06.2018 ###############################################################
RegData <- read.table('I:/norgast/AlleVariablerNum2018-06-14 09-40-44.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-06-14 09-40-55.txt', header=TRUE, sep=";", encoding = 'UFT-8')

RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                      'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                      'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                      'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                      'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]
ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
RegData <- NorgastPreprosess(RegData)
RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
# RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn) # Fjern mellomrom før og etter sykehusnavn

width=600
height=700
sideTxt='Sykehus'
decreasing=F
terskel=10
minstekrav = NA
maal = NA
skriftStr=1.3
pktStr=1.4
legPlass='top'
minstekravTxt='Min.'
maalTxt='Mål'
graaUt=NA
minald=0
maxald=130
erMann <- 99
inkl_konf <- T
elektiv=99
datoFra <- '2015-01-01'
datoTil <- '2050-01-01'
tittel <- ''
hentData <- F
preprosess <- F
BMI=''
minPRS=0
maxPRS=2
ASA=''
whoEcog= ''
ncsp=''
forbehandling=99
valgtShus=c('')
reseksjonsGr <- ''
malign <- 99
annet_format_ut <- F
ut_format <- 'wmf'
reshID <- 601225

valgtVar <- 'Saarruptur'
tilgang=1
reseksjonsGr <- ''
enhetsUtvalg <- 1

outfile <- 'fig1.pdf'
if (annet_format_ut) {outfile <- paste0(substr(outfile, 1, nchar(outfile)-3), ut_format)}
FigAndeler(RegData, valgtVar=valgtVar, reseksjonsGr='', outfile=outfile, datoFra='2014-01-01',
           datoTil=datoTil, reshID=reshID, enhetsUtvalg=enhetsUtvalg, tilgang=tilgang)
outfile <- 'fig2.pdf'
if (annet_format_ut) {outfile <- paste0(substr(outfile, 1, nchar(outfile)-3), ut_format)}
NorgastFigAndelTid(RegData, valgtVar=valgtVar, reseksjonsGr='', outfile=outfile, datoFra='2014-01-01',
                   datoTil=datoTil, reshID=reshID, enhetsUtvalg=enhetsUtvalg, tilgang=tilgang)
outfile <- 'fig2_konf.pdf'
if (annet_format_ut) {outfile <- paste0(substr(outfile, 1, nchar(outfile)-3), ut_format)}
NorgastFigAndelTid(RegData, valgtVar=valgtVar, reseksjonsGr='', outfile=outfile, datoFra='2014-01-01',
                   datoTil=datoTil, reshID=reshID, enhetsUtvalg=enhetsUtvalg, tilgang=tilgang, inkl_konf=inkl_konf)
outfile <- 'fig3.pdf'
if (annet_format_ut) {outfile <- paste0(substr(outfile, 1, nchar(outfile)-3), ut_format)}
FigAndeler(RegData[RegData$Op_gr %in% 1:2, ], valgtVar=valgtVar, reseksjonsGr='', outfile=outfile, datoFra='2014-01-01',
           datoTil=datoTil, reshID=reshID, enhetsUtvalg=enhetsUtvalg, tilgang=tilgang)
outfile <- 'fig4.pdf'
NorgastFigAndelTid(RegData[RegData$Op_gr %in% 1:2, ], valgtVar=valgtVar, reseksjonsGr='', outfile=outfile, datoFra='2014-01-01',
                   datoTil=datoTil, reshID=reshID, enhetsUtvalg=enhetsUtvalg, tilgang=tilgang)
outfile <- 'fig4_konf.pdf'
if (annet_format_ut) {outfile <- paste0(substr(outfile, 1, nchar(outfile)-3), ut_format)}
NorgastFigAndelTid(RegData[RegData$Op_gr %in% 1:2, ], valgtVar=valgtVar, reseksjonsGr='', outfile=outfile, datoFra='2014-01-01',
                   datoTil=datoTil, reshID=reshID, enhetsUtvalg=enhetsUtvalg, tilgang=tilgang, inkl_konf=inkl_konf)

valgtVar <- 'Anastomoselekkasje'
reseksjonsGr <- '(JFB[2-5][0-9]|JFB6[0-4])|JFH'
outfile <- 'fig5.pdf'
if (annet_format_ut) {outfile <- paste0(substr(outfile, 1, nchar(outfile)-3), ut_format)}
FigAndeler(RegData, valgtVar=valgtVar, reseksjonsGr=reseksjonsGr, outfile=outfile, datoFra='2014-01-01',
           datoTil=datoTil, reshID=reshID, enhetsUtvalg=enhetsUtvalg)
outfile <- 'fig6.pdf'
if (annet_format_ut) {outfile <- paste0(substr(outfile, 1, nchar(outfile)-3), ut_format)}
FigAndeler(RegData, valgtVar=valgtVar, reseksjonsGr=reseksjonsGr, outfile=outfile, datoFra='2014-01-01',
           datoTil='2017-08-31', reshID=reshID, enhetsUtvalg=enhetsUtvalg)
outfile <- 'fig7.pdf'
if (annet_format_ut) {outfile <- paste0(substr(outfile, 1, nchar(outfile)-3), ut_format)}
FigAndeler(RegData, valgtVar=valgtVar, reseksjonsGr=reseksjonsGr, outfile=outfile, datoFra='2017-09-01',
           datoTil=datoTil, reshID=reshID, enhetsUtvalg=enhetsUtvalg)
outfile <- 'fig8.pdf'
if (annet_format_ut) {outfile <- paste0(substr(outfile, 1, nchar(outfile)-3), ut_format)}
NorgastFigAndelTid(RegData, valgtVar=valgtVar, reseksjonsGr=reseksjonsGr, outfile=outfile, datoFra='2014-01-01',
                   datoTil=datoTil, reshID=reshID, enhetsUtvalg=enhetsUtvalg, tilgang=tilgang)
outfile <- 'fig8_konf.pdf'
if (annet_format_ut) {outfile <- paste0(substr(outfile, 1, nchar(outfile)-3), ut_format)}
NorgastFigAndelTid(RegData, valgtVar=valgtVar, reseksjonsGr=reseksjonsGr, outfile=outfile, datoFra='2014-01-01',
                   datoTil=datoTil, reshID=reshID, enhetsUtvalg=enhetsUtvalg, tilgang=tilgang, inkl_konf=inkl_konf)



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












