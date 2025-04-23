library(norgast)
library(tidyverse)
rm(list = ls())

rap_aar <- 2024

RegData <-  norgast::NorgastHentRegData()
RegData <- norgast::NorgastPreprosess(RegData)
# RegData$AvdRESH[RegData$AvdRESH == 4204126] <- 4204084 # Tull med Ringerike

RegDataOblig <- RegData[RegData$Op_gr %in% 1:8, ]
RegDataOblig <- RegDataOblig[RegDataOblig$Aar <= rap_aar, ]
# RegDataOblig <- RegDataOblig %>% filter(OpDato < "2023-07-01") ## Ad hoc, desemberpublisering

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

indikator <- indikator %>% select(-Sykehus)

### Tilbered dekningsgrad for sykehusviser
dg_kobl_resh_orgnr <- data.frame(
  orgnr_sh = c(974733013, 974631407, 974557746, 974632535,
               974795787, 974705788, 974633574, 974795639,
               974724960, 974795361, 993467049, 974631326,
               974749025, 974706490, 974703300, 974633752,
               874632562, 974743272, 974795515, 974116804,
               974747138, 974745569, 974795833, 974633191,
               974724774, 974631091, 974795477, 974329506,
               974316285, 974753898, 974795558,
               974795574, 874716782, 974707152, 974589095,
               974754118, 974747545, 983974732),
  resh = c(100353,4204126, 700922, 108355,
           601225, 103091, 100100, 601231,
           108354, 706264, 700413, 4204082,
           107440, 108162, 114271,4209222,
           108357, 102939, 102141, 107505,
           708761,4204500, 101823, 102037,
           701402, 100354, 102145,4211928,
           100170,4212917, 700840,
           700841, 103312, 4205289, 106168,
           4207594, 4216823, 100315))
# 100089 ahus
# 100092 østfold sarpsbog
# 100132 telemark skien
# 100315 førde
# 101719 unn tromsø
# 101971 finnmark hammerfest
# 107507 aleris oslo
# 4001031 ous

dg_kobl_resh_orgnr$Sykehus <-
  RegData$Sykehusnavn[match(dg_kobl_resh_orgnr$resh, RegData$AvdRESH)]


npr2024 <- read.csv2(
  system.file("extdata/frekvens_norgast_2024.csv", package = "norgast"),
  fileEncoding = "Latin1") |>
  dplyr::mutate(sh = case_when(sh == 974744570 ~ 983974732,
                               sh == 974588951 ~ 974589095,
                               .default = sh),
                Op_gr = case_when(hierarki == "Kolon" ~ 1,
                                  hierarki == "Rektum" ~ 2,
                                  hierarki == "Øsofagus" ~ 3,
                                  hierarki == "Ventrikkel" ~ 4,
                                  hierarki == "Lever" ~ 5,
                                  hierarki == "Whipple" ~ 6,
                                  hierarki == "Distal_pankreas" ~ 7,
                                  hierarki == "Andre_pankreas" ~ 8)) |>
  dplyr::summarise(n_npr = sum(n),
                   .by = c(Op_gr, sh))

dg_norgast <- RegDataOblig |>
  dplyr::filter(Aar == rap_aar) |>
  summarise(n_norgast = n(),
            .by = c(AvdRESH, Op_gr)) |>
  merge(dg_kobl_resh_orgnr, by.x = "AvdRESH", by.y = "resh", all.x = T) |>
  merge(npr2024, by.x = c("Op_gr", "orgnr_sh"),
        by.y = c("Op_gr", "sh"), all = T) |>
  dplyr::mutate(n_norgast = ifelse(is.na(n_norgast), 0, n_norgast),
                n_npr = ifelse(is.na(n_npr), 0, n_npr),
                DG = n_norgast/n_npr*100)|>
  merge(dg_kobl_resh_orgnr[, c("orgnr_sh", "resh")],
        .by = "orgnr_sh", all.x = T) |>
  select(-c(Sykehus, AvdRESH)) |>
  rename(AvdRESH = resh)

dg_norgast_samlet <- dg_norgast |>
  summarise(n_norgast = sum(n_norgast),
            n_npr = sum(n_npr),
            DG = n_norgast/n_npr*100,
            .by = orgnr_sh) |>
  merge(dg_kobl_resh_orgnr[, c("orgnr_sh", "resh")],
        .by = "orgnr_sh", all.x = T) |>
  rename(AvdRESH = resh) |>
  mutate(Op_gr = 99)

samlet <- bind_rows(dg_norgast, dg_norgast_samlet) |>
  mutate(DG = ifelse(DG > 100, 100, DG))

write.csv2(samlet, "inst/extdata/dg2024.csv",
           fileEncoding = "UTF-8", row.names = F)


# intersect(npr2024$sh, dg_kobl_resh_orgnr$orgnr_sh)
# setdiff(npr2024$sh, dg_kobl_resh_orgnr$orgnr_sh)
# setdiff(dg_kobl_resh_orgnr$orgnr_sh, npr2024$sh)

mangler_norgast <- npr2024[npr2024$sh %in% setdiff(npr2024$sh, dg_kobl_resh_orgnr$orgnr_sh), ]
mangler_norgast <- mangler_norgast[match(unique(mangler_norgast$sh), mangler_norgast$sh), ]

write.csv2(mangler_norgast, "orgnr_norgast_mangler.csv", fileEncoding = "Latin1", row.names = F)


# dg_samlet <- read.csv2("~/mydata/norgast/norgast_dg_2022.csv")
# dg_samlet <- dg_samlet %>% dplyr::filter(substr(ind_id, 1, 10) == "norgast_dg")
#
# DG_tot_2023 <- read.csv2("~/mydata/norgast/DG_Sykehus/Alle_sh.csv") %>%
#   merge(dg_kobl_resh_orgnr, by.x = "AvdRESH", by.y = "resh") %>%
#   mutate(context = "caregiver",
#          var = Kun_norgast + Begge,
#          year = 2023,
#          ind_id = "norgast_dg_total") %>%
#   rename(orgnr = orgnr_sh,
#          denominator = Total) %>%
#   select(context, orgnr, year, var, denominator, ind_id)
#
# DG_pankreas_2023 <- read.csv2("~/mydata/norgast/DG_Sykehus/Whipple_sh.csv") %>%
#   merge(dg_kobl_resh_orgnr, by.x = "AvdRESH", by.y = "resh") %>%
#   mutate(context = "caregiver",
#          var = Kun_norgast + Begge,
#          year = 2023,
#          ind_id = "norgast_dg_pankreas") %>%
#   rename(orgnr = orgnr_sh,
#          denominator = Total) %>%
#   select(context, orgnr, year, var, denominator, ind_id)
#
# DG_kolon_2023 <- read.csv2("~/mydata/norgast/DG_Sykehus/Kolon_sh.csv") %>%
#   merge(dg_kobl_resh_orgnr, by.x = "AvdRESH", by.y = "resh") %>%
#   mutate(context = "caregiver",
#          var = Kun_norgast + Begge,
#          year = 2023,
#          ind_id = "norgast_dg_tykktarm") %>%
#   rename(orgnr = orgnr_sh,
#          denominator = Total) %>%
#   select(context, orgnr, year, var, denominator, ind_id)
#
# DG_rektum_2023 <- read.csv2("~/mydata/norgast/DG_Sykehus/Rektum_sh.csv") %>%
#   merge(dg_kobl_resh_orgnr, by.x = "AvdRESH", by.y = "resh") %>%
#   mutate(context = "caregiver",
#          var = Kun_norgast + Begge,
#          year = 2023,
#          ind_id = "norgast_dg_endetarm") %>%
#   rename(orgnr = orgnr_sh,
#          denominator = Total) %>%
#   select(context, orgnr, year, var, denominator, ind_id)
#
# DG_lever_2023 <- read.csv2("~/mydata/norgast/DG_Sykehus/Lever_sh.csv") %>%
#   merge(dg_kobl_resh_orgnr, by.x = "AvdRESH", by.y = "resh") %>%
#   mutate(context = "caregiver",
#          var = Kun_norgast + Begge,
#          year = 2023,
#          ind_id = "norgast_dg_lever") %>%
#   rename(orgnr = orgnr_sh,
#          denominator = Total) %>%
#   select(context, orgnr, year, var, denominator, ind_id)
#
# DG_osofagus_2023 <- read.csv2("~/mydata/norgast/DG_Sykehus/Øsofagus_sh.csv") %>%
#   merge(dg_kobl_resh_orgnr, by.x = "AvdRESH", by.y = "resh") %>%
#   mutate(context = "caregiver",
#          var = Kun_norgast + Begge,
#          year = 2023,
#          ind_id = "norgast_dg_spiseroer") %>%
#   rename(orgnr = orgnr_sh,
#          denominator = Total) %>%
#   select(context, orgnr, year, var, denominator, ind_id)
#
# DG_ventrikkel_2023 <- read.csv2("~/mydata/norgast/DG_Sykehus/Ventrikkel_sh.csv") %>%
#   merge(dg_kobl_resh_orgnr, by.x = "AvdRESH", by.y = "resh") %>%
#   mutate(context = "caregiver",
#          var = Kun_norgast + Begge,
#          year = 2023,
#          ind_id = "norgast_dg_magesekk") %>%
#   rename(orgnr = orgnr_sh,
#          denominator = Total) %>%
#   select(context, orgnr, year, var, denominator, ind_id)
#
# dg_samlet <- bind_rows(dg_samlet, DG_tot_2023) %>%
#   bind_rows(DG_pankreas_2023) %>%
#   bind_rows(DG_kolon_2023) %>%
#   bind_rows(DG_rektum_2023) %>%
#   bind_rows(DG_lever_2023) %>%
#   bind_rows(DG_osofagus_2023) %>%
#   bind_rows(DG_ventrikkel_2023)
#
# indikator <- bind_rows(indikator, dg_samlet)
#
# write.csv2(indikator, paste0("~/mydata/norgast/norgast_indikator_", lubridate::today(), ".csv"),
#            row.names = F, fileEncoding = 'UTF-8')
#

# DG_2022 <- read.csv2('~/mydata/norgast/dg_opgr_shus.csv')
#
# DG_tot_2022 <- DG_2022 %>%
#   summarise(n_norgast = sum(n_norgast),
#             n_npr = sum(n_npr),
#             DG = n_norgast/n_npr*100,
#             AvdRESH = first(AvdRESH),
#             Sykehusnavn = first(Sykehusnavn),
#             .by = sh) %>%
#   mutate(Op_gr = 7) %>%
#   select(sh, n_norgast, n_npr, Op_gr)
#
# DG_pankreas <- DG_2022 %>%
#   filter(Op_gr %in% 6:8) %>%
#   summarise(n_norgast = sum(n_norgast),
#             n_npr = sum(n_npr),
#             DG = n_norgast/n_npr*100,
#             AvdRESH = first(AvdRESH),
#             Sykehusnavn = first(Sykehusnavn),
#             .by = sh) %>%
#   mutate(Op_gr = 6) %>%
#   select(sh, n_norgast, n_npr, Op_gr)
#
# DG_2022 <- DG_2022 %>%
#   filter(Op_gr %in% 1:5) %>%
#   select(sh, n_norgast, n_npr, Op_gr) %>%
#   dplyr::bind_rows(DG_pankreas) %>%
#   dplyr::bind_rows(DG_tot_2022) %>%
#   mutate(context = "caregiver",
#          year = 2022) %>%
#   rename(orgnr = sh,
#          var = n_norgast,
#          denominator = n_npr) %>%
#   mutate(ind_id = case_when(Op_gr == 1 ~ "norgast_dg_tykktarm",
#                             Op_gr == 2 ~ "norgast_dg_endetarm",
#                             Op_gr == 3 ~ "norgast_dg_spiseroer",
#                             Op_gr == 4 ~ "norgast_dg_magesekk",
#                             Op_gr == 5 ~ "norgast_dg_lever",
#                             Op_gr == 6 ~ "norgast_dg_pankreas",
#                             Op_gr == 7 ~ "norgast_dg_total")) %>%
#   select(-Op_gr)
#
# dg_samlet <- bind_rows(dg_samlet, DG_2022)
# dg_samlet$var[which(dg_samlet$var > dg_samlet$denominator)] <-
#   dg_samlet$denominator[which(dg_samlet$var > dg_samlet$denominator)]
#
# indikator <- bind_rows(indikator, dg_samlet)
#
# write.csv2(indikator, paste0("~/mydata/norgast/norgast_indikator_", lubridate::today(), ".csv"),
#            row.names = F, fileEncoding = 'UTF-8')

