
library(norgast)
library(dplyr)

testdata <- read.csv2("C:/regdata/norgast/NORGAST_skjemaoversikt_datadump_28.10.2025.csv")

tabell <- testdata |>
  summarise(status = unique(SkjemaStatus),
            .by = c(OpprettetAv, Skjemanavn, HovedDato)) |>
  mutate(HovedDato = case_when(
    HovedDato == "2025-06-12" ~ "Case2",
    HovedDato == "2025-06-24" ~ "Case3",
    HovedDato == "2025-07-01" ~ "Case1",
    HovedDato == "2025-07-25" ~ "Case4",
    .default = "Feil.dato"),
    Skjemanavn = case_when(
      Skjemanavn == "Registrering" ~ "Skjema1",
      Skjemanavn == "Oppfolging/Innleggelse" ~ "Skjema2",
      .default = "")
  ) |>
  arrange(HovedDato, Skjemanavn) |>
  tidyr::pivot_wider(names_from = c(Skjemanavn, HovedDato),
                     values_from = status) |>
  mutate(AlleFerdig = as.numeric(rowSums(across(2:9), na.rm = T) == 8 ))


write.csv2(
  tabell,
  paste0("C:/regdata/norgast/status_datakvalreg_norgast_", Sys.Date(), ".csv"),
  na = "", fileEncoding = "Latin1", row.names = F)





