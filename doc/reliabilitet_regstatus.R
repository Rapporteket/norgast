
library(norgast)
library(dplyr)

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


registration <- read.csv2(
  "C:/regdata/norgast/reliabilitet/NORGAST_registration_datadump_20.11.2025.csv")
readmission <- read.csv2(
  "C:/regdata/norgast/reliabilitet/NORGAST_readmission_datadump_20.11.2025.csv")


# =============================
# R TEMPLATE FOR INTER-RATER RELIABILITY
# =============================

# Load required packages
library(tidyr)
library(dplyr)
library(irr)

# 1. Load your data
data <- read.csv2(
  "C:/regdata/norgast/reliabilitet/NORGAST_registration_datadump_20.11.2025.csv") |>
  filter(STATUS == 1) %>%
  filter(!duplicated(select(., CASENUMBER, CREATEDBY))) |>
  filter(CREATEDBY != "krlass@ous-hf.no")


# 2. Choose variable of interest (example: BMI_CATEGORY)
variable_of_interest <- "BMI_CATEGORY"

# 3. Reshape data to wide format
irr_data <- data %>%
  select(CASENUMBER, CREATEDBY, !!sym(variable_of_interest)) %>%
  pivot_wider(names_from = CREATEDBY, values_from = !!sym(variable_of_interest))


# Function to filter columns based on first n rows having no NA
keep_non_na_columns <- function(df, n) {
  # Check each column: are the first n elements all non-NA?
  cols_to_keep <- sapply(df, function(col) all(!is.na(head(col, n))))

  # Subset the data frame to keep only those columns
  df_filtered <- df[, cols_to_keep, drop = FALSE]

  return(df_filtered)
}

irr_data2 <- keep_non_na_columns(irr_data, 3)
irr_data3 <- keep_non_na_columns(irr_data, 4)

# 4. Prepare ratings matrix (remove CASENUMBER)
ratings <- irr_data3[,-1]


# ratings[] <- lapply(ratings, function(x) {
#   if (is.numeric(x)) {
#     x[is.na(x)] <- mean(x, na.rm = TRUE)
#   } else {
#     x[is.na(x)] <- names(sort(table(x), decreasing = TRUE))[1]
#   }
#   x
# })


# 5. Compute IRR
# Fleiss' Kappa for >2 raters (nominal data)
kappa_result <- kappam.fleiss(ratings)
print(kappa_result)

# Cohen's Kappa for 2 raters
# kappa_result <- kappa2(ratings)

# ICC for continuous data
# icc_result <- icc(ratings, model = "twoway", type = "agreement", unit = "single")
# print(icc_result)

# =============================
# Interpretation:
# Kappa: <0 Poor, 0-0.20 Slight, 0.21-0.40 Fair, 0.41-0.60 Moderate, 0.61-0.80 Substantial, 0.81-1 Almost perfect
# ICC: <0.5 Poor, 0.5-0.75 Moderate, 0.75-0.9 Good, >0.9 Excellent
# =============================

# =============================
# FULL IRR ANALYSIS WITH KRIPPENDORFF'S ALPHA
# =============================

library(dplyr)
library(tidyr)
library(irr)

# 1. Load and clean data
data <- read.csv2("C:/regdata/norgast/reliabilitet/NORGAST_registration_datadump_20.11.2025.csv") %>%
  filter(STATUS == 1) |>
  filter(!duplicated(paste(CASENUMBER, CREATEDBY))) |>
  filter(CREATEDBY != "krlass@ous-hf.no") %>%
  select(where(~ !all(is.na(.)))) |>
  mutate(ROBOTASSISTANCE = ifelse(is.na(ROBOTASSISTANCE), 9, ROBOTASSISTANCE),
         TATME = ifelse(is.na(TATME), 9, TATME),
         RELAPAROTOMY_YES = ifelse(is.na(RELAPAROTOMY_YES), 9, RELAPAROTOMY_YES),
         RELAPAROTOMY_NO = ifelse(is.na(RELAPAROTOMY_NO), 9, RELAPAROTOMY_NO),
         PERCUTANEOUS_DRAINAGE = ifelse(is.na(PERCUTANEOUS_DRAINAGE), 9, PERCUTANEOUS_DRAINAGE),
         LEAK_INTERVENTION = ifelse(is.na(LEAK_INTERVENTION), 9, LEAK_INTERVENTION),
         BLEED_INTERVENTION = ifelse(is.na(BLEED_INTERVENTION), 9, BLEED_INTERVENTION),
         ANGIO_INTERVENTION = ifelse(is.na(ANGIO_INTERVENTION), 9, ANGIO_INTERVENTION),
         LIQUID_DRAINAGE = ifelse(is.na(LIQUID_DRAINAGE), 9, LIQUID_DRAINAGE)
         )
klokebok <- read.csv2("C:/regdata/norgast/klokebok/NORGAST_klokeboken_02.04.2025.csv")

# 2. Define variables of interest
categorical_vars <- klokebok |> filter(skjemanavn == "Registrering",
                                       type == "Listevariabel") |>
  select(fysisk_feltnavn) |> unique() |> unlist() |> intersect(names(data))
# categorical_vars <- c("BMI_CATEGORY", "ASA")       # Example categorical
numeric_vars <- c("ADMISSION_WEIGHT", "HEIGHT")    # Example numeric

# Function to filter columns based on first n rows having no NA
keep_non_na_columns <- function(df, n) {
  # Check each column: are the first n elements all non-NA?
  cols_to_keep <- sapply(df, function(col) all(!is.na(head(col, n))))

  # Subset the data frame to keep only those columns
  df_filtered <- df[, cols_to_keep, drop = FALSE]

  return(df_filtered)
}

# 3. Function to compute IRR for categorical variables
compute_categorical_IRR <- function(var, n) {
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
compute_numeric_IRR <- function(var, n) {
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
  # kripp_result <- kripp.alpha(t(ratings), method = "interval")

  print(icc_result)
  # print(kripp_result)
}

# 5. Run analysis
tabell_kategorisk <- data.frame(Variabel = NULL,
                                Kappa = NULL,
                                Krippendorff = NULL,
                                Samsvar = NULL,
                                ant_ratere = NULL,
                                ant_caser = NULL)
k <- 0
for (var in categorical_vars) {
  k <- k+1
  analyse <- compute_categorical_IRR(var, 3)
  tabell_kategorisk <- bind_rows(
    tabell_kategorisk,
    data.frame(Variabel = var,
               Kappa = analyse$kappa_result$value,
               Krippendorff = analyse$kripp_result$value,
               Samsvar = analyse$samsvar$value,
               ant_ratere = analyse$samsvar$raters,
               ant_caser = analyse$samsvar$subjects)
    )
  }
for (var in numeric_vars) compute_numeric_IRR(var, 3)

# =============================
# Interpretation:
# Kappa: <0 Poor, 0-0.20 Slight, 0.21-0.40 Fair, 0.41-0.60 Moderate, 0.61-0.80 Substantial, 0.81-1 Almost perfect
# ICC: <0.5 Poor, 0.5-0.75 Moderate, 0.75-0.9 Good, >0.9 Excellent
# Krippendorff's alpha: >0.8 good, >0.667 acceptable
# =============================



irr_data <- data %>%
  select(CASENUMBER, CREATEDBY, CHEMOTHERAPY_ONLY) %>%
  pivot_wider(names_from = CREATEDBY, values_from = CHEMOTHERAPY_ONLY) |>
  keep_non_na_columns(n)

