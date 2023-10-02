library(norgast)
library(tidyverse)
rm(list = ls())

rap_aar <- 2022

RegData <-  norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
# RegData$AvdRESH[RegData$AvdRESH == 4204126] <- 4204084 # Tull med Ringerike

RegDataOblig <- RegData[RegData$Op_gr %in% 1:8, ]

ind <- c("norgast_saarruptur", "norgast_aktivkontroll", "norgast_vekt_reg",
         "norgast_avdoede_spiseroer", "norgast_avdoede_magesekk",
         "norgast_avdoede_bukspytt_tolv", "norgast_avdoede_lever",
         "norgast_lekkasje_tykktarm", "norgast_lekkasje_endetarm",
         "norgast_kikkhullsteknikk_endetarm", "norgast_kikkhullsteknikk_tykktarm",
         "norgast_kikkhullsteknikk_lever")

indikator <- norgastBeregnIndikator(RegDataOblig, ind[1])$Indikator

for (ind_id in ind[-1]) {
  indikator <- dplyr::bind_rows(indikator, norgastBeregnIndikator(RegDataOblig, ind_id)$Indikator)
}


indikatordata <- norgastBeregnIndikator(RegDataOblig, "norgast_vekt_reg")


norgastPlotIndikator(AntTilfeller = indikatordata$AntTilfeller,
                     N = indikatordata$N,
                     andeler = indikatordata$andeler,
                     decreasing = indikatordata$decreasing,
                     terskel = indikatordata$terskel,
                     minstekrav = indikatordata$minstekrav,
                     maal = indikatordata$maal,
                     utvalgTxt = indikatordata$utvalgTxt,
                     tittel = indikatordata$tittel)


### Tilbered dekningsgrad for sykehusviser

dg_kobl_resh_orgnr <- data.frame(orgnr_sh = c(974733013, 974631407, 974557746, 974632535, 974795787, 974705788, 974633574, 974795639,
                                              974724960, 974795361, 993467049, 974631326, 974749025, 974706490, 974703300, 974633752,
                                              874632562, 974743272, 974795515, 974116804, 974747138, 974745569, 974795833, 974633191,
                                              974724774, 974631091, 974795477, 974329506, 974316285, 974631407, 974795558,
                                              974795574, 874716782, 974707152, 974631776, 974744570, 974747545, 974753898, 974795558,
                                              974795574, 974754118, 974589095, 974754118, 974747545),
                                 resh = c(100353,4204126, 700922, 108355, 601225, 103091, 100100, 601231, 108354, 706264, 700413,
                                          4204082, 107440, 108162, 114271,4209222, 108357, 102939, 102141, 107505, 708761,4204500,
                                          101823, 102037, 701402, 100354, 102145,4211928, 100170, 4204084, 700840, 700841,
                                          103312,4205289, 974631776, 974744570, 974747545, 974753898, 974795558, 974795574, 4212917,
                                          106168, 4207594, 4216823))

# dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
#                          sheet = "Total DG per SH")
# dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# # dg$orgnr[which(dg$Sykehus=="Levanger")] <- 974754118
# dg <- dg[,c(10,6,2,3)]
# dg <- dg[!is.na(dg$orgnr), ]
# dg$ind_id <- "norgast_dg_total"
# names(dg)[2:4] <- c("year",	"var", "denominator")
# dg_samlet <- dg
#
#
# dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
#                          sheet = "DG Tykktarm")
# dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# # dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
# dg <- dg[,c(10,6,2,3)]
# dg <- dg[!is.na(dg$orgnr), ]
# dg$ind_id <- "norgast_dg_tykktarm"
# names(dg)[2:4] <- c("year",	"var", "denominator")
# dg_samlet <- bind_rows(dg_samlet, dg)
#
# dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
#                          sheet = "DG_Lever")
# dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# # dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
# dg <- dg[,c(10,6,2,3)]
# dg <- dg[!is.na(dg$orgnr), ]
# dg$ind_id <- "norgast_dg_lever"
# names(dg)[2:4] <- c("year",	"var", "denominator")
# dg_samlet <- bind_rows(dg_samlet, dg)
#
# dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
#                          sheet = "DG_Pankreas")
# dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# # dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
# dg <- dg[,c(10,6,2,3)]
# dg <- dg[!is.na(dg$orgnr), ]
# dg$ind_id <- "norgast_dg_pankreas"
# names(dg)[2:4] <- c("year",	"var", "denominator")
# dg_samlet <- bind_rows(dg_samlet, dg)
#
# dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
#                          sheet = "DG_Endetarm")
# dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# # dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
# dg <- dg[,c(10,6,2,3)]
# dg <- dg[!is.na(dg$orgnr), ]
# dg$ind_id <- "norgast_dg_endetarm"
# names(dg)[2:4] <- c("year",	"var", "denominator")
# dg_samlet <- bind_rows(dg_samlet, dg)
#
# dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
#                          sheet = "DG_Magesekk")
# dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# # dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
# dg <- dg[,c(10,6,2,3)]
# dg <- dg[!is.na(dg$orgnr), ]
# dg$ind_id <- "norgast_dg_magesekk"
# names(dg)[2:4] <- c("year",	"var", "denominator")
# dg_samlet <- bind_rows(dg_samlet, dg)
#
# dg <- readxl::read_excel("~/.ssh/norgast/DG_Norgast.xlsx",
#                          sheet = "DG_Spiseroer")
# dg$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(dg$ReshID, dg_kobl_resh_orgnr$resh)]
# # dg$orgnr[dg$Sykehus=="Levanger"] <- 974754118
# dg <- dg[,c(10,6,2,3)]
# dg <- dg[!is.na(dg$orgnr), ]
# dg$ind_id <- "norgast_dg_spiseroer"
# names(dg)[2:4] <- c("year",	"var", "denominator")
# dg_samlet <- bind_rows(dg_samlet, dg)
#
# # Legg til 2021
# mapping_npr <- read.csv2('~/.ssh/Sykehus/Koblingstabell_AvdRESH_sh_standard.csv', fileEncoding = "Latin1")
# DG <- read.csv2('~/.ssh/Sykehus/Alle_sh.csv', fileEncoding = "Latin1")
# DG$ind_id <- "norgast_dg_total"
# DG_samlet <- DG
#
# DG <- read.csv2('~/.ssh/Sykehus/Kolon_sh.csv', fileEncoding = "Latin1")
# DG$ind_id <- "norgast_dg_tykktarm"
# DG_samlet <- bind_rows(DG_samlet, DG)
#
# DG <- read.csv2('~/.ssh/Sykehus/Rektum_sh.csv', fileEncoding = "Latin1")
# DG$ind_id <- "norgast_dg_endetarm"
# DG_samlet <- bind_rows(DG_samlet, DG)
#
# DG <- read.csv2('~/.ssh/Sykehus/Lever_sh.csv', fileEncoding = "Latin1")
# DG$ind_id <- "norgast_dg_lever"
# DG_samlet <- bind_rows(DG_samlet, DG)
#
# DG <- read.csv2('~/.ssh/Sykehus/Ventrikkel_sh.csv', fileEncoding = "Latin1")
# DG$ind_id <- "norgast_dg_magesekk"
# DG_samlet <- bind_rows(DG_samlet, DG)
#
# DG <- read.csv2('~/.ssh/Sykehus/Whipple_sh.csv', fileEncoding = "Latin1")
# DG$ind_id <- "norgast_dg_pankreas"
# DG_samlet <- bind_rows(DG_samlet, DG)
#
# DG <- read.csv2('~/.ssh/Sykehus/Øsofagus_sh.csv', fileEncoding = "Latin1")
# DG$ind_id <- "norgast_dg_spiseroer"
# DG_samlet <- bind_rows(DG_samlet, DG)
#
# DG_samlet$AvdRESH <- mapping_npr$AvdRESH[match(DG_samlet$sh_standard, mapping_npr$sh_standard)]
# DG_samlet <- DG_samlet[!is.na(DG_samlet$AvdRESH), ]
# DG_samlet$orgnr <- dg_kobl_resh_orgnr$orgnr_sh[match(DG_samlet$AvdRESH, dg_kobl_resh_orgnr$resh)]
# DG_samlet$year <- 2021
# DG_samlet$var <- DG_samlet$Begge + DG_samlet$Kun_norgast
# DG_samlet$denominator <- DG_samlet$Total
#
# DG_samlet <- DG_samlet[,c("orgnr", "year", "var", "denominator", "ind_id")]
# dg_samlet <- bind_rows(DG_samlet, dg_samlet)
# dg_samlet$context <- "caregiver"
# dg_samlet$var <- round(dg_samlet$var)

dg_samlet <- read.csv2("~/mydata/norgast/dg_norgast.csv")
dg_samlet <- dg_samlet %>% dplyr::filter(substr(ind_id, 1, 10) == "norgast_dg")

DG_2022 <- read.csv2('~/mydata/norgast/dg_opgr_shus.csv')

DG_tot_2022 <- DG_2022 %>%
  summarise(n_norgast = sum(n_norgast),
            n_npr = sum(n_npr),
            DG = n_norgast/n_npr*100,
            AvdRESH = first(AvdRESH),
            Sykehusnavn = first(Sykehusnavn),
            .by = sh) %>%
  mutate(Op_gr = 7) %>%
  select(sh, n_norgast, n_npr, Op_gr)

DG_pankreas <- DG_2022 %>%
  filter(Op_gr %in% 6:8) %>%
  summarise(n_norgast = sum(n_norgast),
            n_npr = sum(n_npr),
            DG = n_norgast/n_npr*100,
            AvdRESH = first(AvdRESH),
            Sykehusnavn = first(Sykehusnavn),
            .by = sh) %>%
  mutate(Op_gr = 6) %>%
  select(sh, n_norgast, n_npr, Op_gr)

DG_2022 <- DG_2022 %>%
  filter(Op_gr %in% 1:5) %>%
  select(sh, n_norgast, n_npr, Op_gr) %>%
  dplyr::bind_rows(DG_pankreas) %>%
  dplyr::bind_rows(DG_tot_2022) %>%
  mutate(context = "caregiver",
         year = 2022) %>%
  rename(orgnr = sh,
         var = n_norgast,
         denominator = n_npr) %>%
  mutate(ind_id = case_when(Op_gr == 1 ~ "norgast_dg_tykktarm",
                            Op_gr == 2 ~ "norgast_dg_endetarm",
                            Op_gr == 3 ~ "norgast_dg_spiseroer",
                            Op_gr == 4 ~ "norgast_dg_magesekk",
                            Op_gr == 5 ~ "norgast_dg_lever",
                            Op_gr == 6 ~ "norgast_dg_pankreas",
                            Op_gr == 7 ~ "norgast_dg_total")) %>%
  select(-Op_gr)

dg_samlet <- bind_rows(dg_samlet, DG_2022)
dg_samlet$var[which(dg_samlet$var > dg_samlet$denominator)] <-
  dg_samlet$denominator[which(dg_samlet$var > dg_samlet$denominator)]

indikator <- bind_rows(indikator, dg_samlet)

write.csv2(indikator, paste0("~/mydata/norgast/norgast_indikator_", lubridate::today(), ".csv"),
           row.names = F, fileEncoding = 'UTF-8')

# slett_resident <- indikator[match(unique(indikator$ind_id), indikator$ind_id), ]
# slett_resident$context <- "resident"
# slett_resident$var <- 0
# slett_resident$denominator <- 1
#
# write.csv2(slett_resident, "~/.ssh/norgast/norgast_slett_resident_2022_09_06.csv", row.names = F, fileEncoding = 'UTF-8')

# write.csv2(dg_samlet[dg_samlet$year <= rap_aar, ], "~/.ssh/norgast/norgast_dg.csv", row.names = F, fileEncoding = 'UTF-8')
#
# ind_info <- readxl::read_xlsx("~/.ssh/norgast/Indikatorbeskrivelse publisering SKDE_KH.xlsx", sheet = 2)

