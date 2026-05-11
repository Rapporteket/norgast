
filsti <- "C:/Users/kth200/regdata/norgast/utleveringer/stig/"

crc_kir_mld_variabler_utlevering_2_4356 <-
  read.csv2(paste0(filsti, "crc_kir_mld_variabler_utlevering_2_4356.csv"))

crc_var_pat_utlevering_2_4356 <-
  read.csv2(paste0(filsti, "crc_var_pat_utlevering_2_4356.csv"))

crc_variabler_utlevering_2_4356 <-
  read.csv2(paste0(filsti, "crc_variabler_utlevering_2_4356.csv"))

NORGAST_datasett <-
  read.csv2(paste0(filsti, "NORGAST_datasett.csv"))

post_op_strale_var_utlevering_2_4356 <-
  read.csv2(paste0(filsti, "post_op_strale_var_utlevering_2_4356.csv"))

tabell_fordeling_cT_utlevering_2_4356 <-
  read.csv2(paste0(filsti, "tabell_fordeling_cT_utlevering_2_4356.csv"))

crc_kir_mld_variabler_utlevering_2_4356 <-
  read.csv2(paste0(filsti, "crc_kir_mld_variabler_utlevering_2_4356.csv"))


kobletdata <- merge(crc_kir_mld_variabler_utlevering_2_4356,
                    crc_var_pat_utlevering_2_4356,
                    by = c("P_pidKrg", "S_sidKrg"), all = TRUE) |>
  merge(crc_variabler_utlevering_2_4356,
        by = c("P_pidKrg", "S_sidKrg"), all = TRUE) |>
  merge(post_op_strale_var_utlevering_2_4356,
        by = c("P_pidKrg", "S_sidKrg"), all = TRUE) |>
  merge(crc_kir_mld_variabler_utlevering_2_4356,
        by = c("P_pidKrg", "S_sidKrg"), all = TRUE) #|>
  # merge(NORGAST_datasett,
  #       by = "P_pidKrg", all = TRUE)


write.csv2(kobletdata, paste0(filsti, "kobletdata_krg_jan26.csv"),
           row.names = F,
           fileEncoding = "Latin1")


kobletdata |> summarise(N=n(), .by = P_pidKrg) |> filter(N>1)
tmp <- NORGAST_datasett |> summarise(N=n(), .by = P_pidKrg) |> filter(N>1) |>
  arrange(N)

tmp2 <- NORGAST_datasett |> filter(P_pidKrg %in% tmp$P_pidKrg) |>
  summarise(N = n(), .by = Hovedoperasjon) |> arrange(-N)

