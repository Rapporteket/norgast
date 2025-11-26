
library(norgast)
library(dplyr)
rm(list = ls())

testdata <- read.csv2("C:/regdata/norgast/reliabilitet/NORGAST_skjemaoversikt_datadump_20.11.2025.csv")

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


# write.csv2(
#   tabell,
#   paste0("C:/regdata/norgast/status_datakvalreg_norgast_", Sys.Date(), ".csv"),
#   na = "", fileEncoding = "Latin1", row.names = F)


# registration <- read.csv2(
#   "C:/regdata/norgast/reliabilitet/NORGAST_registration_datadump_20.11.2025.csv")
# readmission <- read.csv2(
#   "C:/regdata/norgast/reliabilitet/NORGAST_readmission_datadump_20.11.2025.csv")


# =============================
# FULL IRR ANALYSIS WITH KRIPPENDORFF'S ALPHA
# =============================

library(tidyr)
library(irr)

# 1. Load and clean data
data_reg <- read.csv2(
  "C:/regdata/norgast/reliabilitet/NORGAST_registration_datadump_20.11.2025.csv") %>%
  filter(STATUS == 1) |>
  filter(!duplicated(paste(CASENUMBER, CREATEDBY))) |>
  filter(CREATEDBY != "krlass@ous-hf.no") %>%
  select(where(~ !all(is.na(.)))) %>%
  mutate(across(c(ROBOTASSISTANCE, TATME, RELAPAROTOMY_YES,
                  RELAPAROTOMY_NO, PERCUTANEOUS_DRAINAGE, LEAK_INTERVENTION,
                  BLEED_INTERVENTION, ANGIO_INTERVENTION, LIQUID_DRAINAGE),
                ~replace_na(., -1))) %>%
  mutate(across(c(PREVIOUS_WEIGHT, ADMISSION_WEIGHT, HEIGHT,
                  ALBUMIN, CRP),
                ~replace_na(., -1)))
data_readm <- read.csv2("C:/regdata/norgast/reliabilitet/NORGAST_readmission_datadump_20.11.2025.csv") %>%
  filter(STATUS == 1) |>
  merge(data_reg |> select(MCEID, CASENUMBER), by = "MCEID") |>
  filter(!duplicated(paste(CASENUMBER, CREATEDBY))) |>
  filter(CREATEDBY != "krlass@ous-hf.no") %>%
  select(where(~ !all(is.na(.)))) %>%
  mutate(across(c(RELAPAROTOMY_YES,
                  RELAPAROTOMY_NO, PERCUTANEOUS_DRAINAGE, LEAK_INTERVENTION,
                  BLEED_INTERVENTION, ANGIO_INTERVENTION, LIQUID_DRAINAGE,
                  PHYSICAL_CONTROL, PHONE_CONTROL, RELAPAROTOMY,
                  INTERVENTION_WITHOUT_ANESTHESIA, SINGLE_ORGAN_FAILURE,
                  MULTI_ORGAN_FAILURE, IN_HOUSE_DEATH),
                ~replace_na(., -1)))

klokebok <- read.csv2("C:/regdata/norgast/klokebok/NORGAST_klokeboken_02.04.2025.csv")

# 2. Define variables of interest
categorical_vars_reg <- klokebok |> filter(skjemanavn == "Registrering",
                                           type %in% c("Listevariabel", "Avkrysningsboks")) |>
  select(fysisk_feltnavn) |> unique() |> unlist() |> intersect(names(data_reg)) |>
  c("NCSP", "ICD10")
categorical_vars_readm <- klokebok |> filter(skjemanavn == "Reinnleggelse/oppfølging",
                                             type %in% c("Listevariabel", "Avkrysningsboks")) |>
  select(fysisk_feltnavn) |> unique() |> unlist() |> intersect(names(data_readm))
numeric_vars_reg <- klokebok |> filter(skjemanavn == "Registrering",
                                       type == "Tallvariabel") |>
  select(fysisk_feltnavn) |> unique() |> unlist() |> intersect(names(data_reg))
numeric_vars_readm <- klokebok |> filter(skjemanavn == "Reinnleggelse/oppfølging",
                                         type == "Tallvariabel") |>
  select(fysisk_feltnavn) |> unique() |> unlist() |> intersect(names(data_readm))

# Function to filter columns based on first n rows having no NA
keep_non_na_columns <- function(df, n) {
  # Check each column: are the first n elements all non-NA?
  cols_to_keep <- sapply(df, function(col) all(!is.na(head(col, n))))

  # Subset the data frame to keep only those columns
  df_filtered <- df[, cols_to_keep, drop = FALSE]

  return(df_filtered)
}

# 3. Function to compute IRR for categorical variables
compute_categorical_IRR <- function(data, var, n) {
  cat("\n=== Inter-Rater Reliability for", var, "===\n")

  irr_data <- data %>%
    select(CASENUMBER, CREATEDBY, all_of(var)) %>%
    pivot_wider(names_from = CREATEDBY, values_from = all_of(var)) |>
    keep_non_na_columns(n)

  ratings <- irr_data[,-1]

  # Drop rows with missing values
  ratings <- na.omit(ratings)
  #
  # Fleiss' or Cohen's Kappa
  if (ncol(ratings) > 2) {
    kappa_result <- kappam.fleiss(ratings)
  } else {
    kappa_result <- kappa2(ratings)
  }

  samsvar = agree(ratings)

  # Krippendorff's alpha (transpose required)
  kripp_result <- kripp.alpha(t(ratings), method = "nominal")

  list(kappa_result = kappa_result,
       kripp_result = kripp_result,
       samsvar = samsvar)
}

# 4. Function to compute IRR for numeric variables
compute_numeric_IRR <- function(data, var, n) {
  cat("\n=== Inter-Rater Reliability for", var, "===\n")

  irr_data <- data %>%
    select(CASENUMBER, CREATEDBY, all_of(var)) %>%
    pivot_wider(names_from = CREATEDBY, values_from = all_of(var)) |>
    keep_non_na_columns(n)

  ratings <- irr_data[,-1]

  # Drop rows with missing values
  ratings <- na.omit(ratings)

  # ICC
  icc_result <- icc(ratings, model = "twoway", type = "agreement", unit = "single")

  # Krippendorff's alpha for interval data
  kripp_result <- kripp.alpha(t(ratings), method = "interval")

  list(icc_result = icc_result,
       kripp_result = kripp_result)
}

# 5. Run analysis
tabell_kategorisk <- data.frame(Tabell = NULL,
                                Variabel = NULL,
                                Kappa = NULL,
                                Krippendorff = NULL,
                                Samsvar = NULL,
                                ant_ratere = NULL,
                                ant_caser = NULL)
k <- 0
for (var in categorical_vars_reg) {
  k <- k+1
  analyse <- compute_categorical_IRR(data_reg, var, 3)
  tabell_kategorisk <- bind_rows(
    tabell_kategorisk,
    data.frame(Tabell = "Registrering",
               Variabel = var,
               Kappa = analyse$kappa_result$value,
               Krippendorff = analyse$kripp_result$value,
               Samsvar = analyse$samsvar$value,
               ant_ratere = analyse$samsvar$raters,
               ant_caser = analyse$samsvar$subjects)
  )
  analyse <- compute_categorical_IRR(data_reg, var, 4)
  tabell_kategorisk <- bind_rows(
    tabell_kategorisk,
    data.frame(Tabell = "Registrering",
               Variabel = var,
               Kappa = analyse$kappa_result$value,
               Krippendorff = analyse$kripp_result$value,
               Samsvar = analyse$samsvar$value,
               ant_ratere = analyse$samsvar$raters,
               ant_caser = analyse$samsvar$subjects)
  )
}
# k <- 0
for (var in categorical_vars_readm) {
  k <- k+1
  analyse <- compute_categorical_IRR(data_readm, var, 3)
  tabell_kategorisk <- bind_rows(
    tabell_kategorisk,
    data.frame(Tabell = "Oppfølging",
               Variabel = var,
               Kappa = analyse$kappa_result$value,
               Krippendorff = analyse$kripp_result$value,
               Samsvar = analyse$samsvar$value,
               ant_ratere = analyse$samsvar$raters,
               ant_caser = analyse$samsvar$subjects)
  )
  analyse <- compute_categorical_IRR(data_readm, var, 4)
  tabell_kategorisk <- bind_rows(
    tabell_kategorisk,
    data.frame(Tabell = "Oppfølging",
               Variabel = var,
               Kappa = analyse$kappa_result$value,
               Krippendorff = analyse$kripp_result$value,
               Samsvar = analyse$samsvar$value,
               ant_ratere = analyse$samsvar$raters,
               ant_caser = analyse$samsvar$subjects)
  )
}



tabell_numerisk <- data.frame(Tabell = NULL,
                              Variabel = NULL,
                              ICC = NULL,
                              Krippendorff = NULL,
                              ant_ratere = NULL,
                              ant_caser = NULL)
k <- 0
for (var in numeric_vars_reg[1:5]) {
  k <- k+1
  analyse <- compute_numeric_IRR(data_reg, var, 3)
  tabell_numerisk <- bind_rows(
    tabell_numerisk,
    data.frame(Tabell = "Registrering",
               Variabel = var,
               ICC = analyse$icc_result$value,
               Krippendorff = analyse$kripp_result$value,
               ant_ratere = analyse$icc_result$raters,
               ant_caser = analyse$icc_result$subjects)
  )
  analyse <- compute_numeric_IRR(data_reg, var, 4)
  tabell_numerisk <- bind_rows(
    tabell_numerisk,
    data.frame(Tabell = "Registrering",
               Variabel = var,
               ICC = analyse$icc_result$value,
               Krippendorff = analyse$kripp_result$value,
               ant_ratere = analyse$icc_result$raters,
               ant_caser = analyse$icc_result$subjects)
  )
}
# for (var in numeric_vars_readm) {
#   k <- k+1
#   analyse <- compute_numeric_IRR(data_readm, var, 3)
#   tabell_numerisk <- bind_rows(
#     tabell_numerisk,
#     data.frame(Tabell = "Oppfølging",
#                Variabel = var,
#                ICC = analyse$icc_result$value,
#                Krippendorff = analyse$kripp_result$value,
#                ant_ratere = analyse$icc_result$raters,
#                ant_caser = analyse$icc_result$subjects)
#   )
#   analyse <- compute_numeric_IRR(data_readm, var, 4)
#   tabell_numerisk <- bind_rows(
#     tabell_numerisk,
#     data.frame(Tabell = "Oppfølging",
#                Variabel = var,
#                ICC = analyse$icc_result$value,
#                Krippendorff = analyse$kripp_result$value,
#                ant_ratere = analyse$icc_result$raters,
#                ant_caser = analyse$icc_result$subjects)
#   )
# }

write.csv2(
  tabell_kategorisk,
  "C:/regdata/norgast/reliabilitet/tabell_kategorisk.csv",
  row.names = F, na = "",
  fileEncoding = "Latin1")

write.csv2(
  tabell_numerisk,
  "C:/regdata/norgast/reliabilitet/tabell_numerisk.csv",
  row.names = F, na = "",
  fileEncoding = "Latin1")

# =============================
# Interpretation:
# Kappa: <0 Poor, 0-0.20 Slight, 0.21-0.40 Fair, 0.41-0.60 Moderate, 0.61-0.80 Substantial, 0.81-1 Almost perfect
# ICC: <0.5 Poor, 0.5-0.75 Moderate, 0.75-0.9 Good, >0.9 Excellent
# Krippendorff's alpha: >0.8 good, >0.667 acceptable
# =============================



