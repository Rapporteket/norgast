library(dplyr)

RegData<-RegDataAll; valgtVar <- 'LapTilgang2'; tittel='';
width=800; height=700;
sideTxt='Boområde/opptaksområde';
decreasing=F; terskel=10; minstekrav = NA;
maal = NA; skriftStr=1.3; pktStr=1.4;
legPlass='top'; minstekravTxt='Akseptabelt';
maalTxt='Mål'; graaUt=NA; inkl_konf=T;
datoFra='2014-01-01'; datoTil='2050-12-31';
minald=0; maxald=130; erMann=99; outfile='';
preprosess=F; malign=99; elektiv=99;
hastegrad=99; BMI=''; tilgang=''; minPRS=0;
maxPRS=2.2; ASA=''; whoEcog= '';
forbehandling=''; dagtid =99; hentData=0;
op_gruppe=''; ncsp=''; maalretn='hoy';
lavDG=''; lavDGtekst='Dekningsgrad < 60 %';
hastegrad_hybrid=99; icd_kode='';
robotassiastanse=99; kun_ferdigstilte=TRUE;
prikktall=FALSE; pst_kolonne=TRUE;
ny_anastomose=99; desimaler_pst = 0;
kun_oblig = FALSE; mrom=0.3
tittel=tittel; width=width;
height=height; decreasing=decreasing;
terskel=terskel; minstekrav = 60; maal = 70;
skriftStr=skriftStr; lavDG = graaUt_kolon;
lavDGtekst = dg_tekst; pktStr=pktStr;
legPlass='topleft'; minstekravTxt=minstekravTxt;
maalTxt=maalTxt; graaUt=graaUt;
whoEcog= c('0', '1'); inkl_konf=inkl_konf;
op_gruppe=op_gruppe; datoFra=datoFra;
datoTil=datoTil; hastegrad_hybrid=hastegrad_hybrid;
malign=1; prikktall = FALSE; pst_kolonne = TRUE






RegData <- regdatagml
datoFra='2014-01-01'; datoTil='2050-12-31';
minald=0; maxald=130; erMann=99;
outfile=''; hastegrad_hybrid=99;
preprosess=F; malign=99; Ngrense=30;
lavDG=''; ny_anastomose = 99;
lavDGtekst='Dekningsgrad < 60 %';
hastegrad = 99; icd_kode='';
elektiv=99; BMI=''; tilgang='';
valgtShus=c(''); minPRS=0; modGlasgow='';
maxPRS=2.2; ASA=''; whoEcog= '';
forbehandling=''; hentData=0; op_gruppe='';
ncsp=''; robotassiastanse=99;
kun_ferdigstilte=TRUE; skriftStr=1;
tilgang_utvidet=''; accordion='';
snufarger = TRUE; krgfarger = FALSE
valgtVar='AccordionGrad_drenasje';
op_gruppe=1;
hastegrad_hybrid=1; outfile=""; Ngrense=10;
malign = 0; lavDG = graaUt_kolon; krgfarger = TRUE






if (valgtShus[1] != '') {RegData <-
  RegData[which(RegData$AvdRESH %in% as.numeric(valgtShus)), ]}

grVar <- 'Sykehusnavn'
if (valgtVar == 'AccordionGrad_drenasje') {
  RegData$AccordionGrad_drenasje <- RegData$AccordionGrad
  RegData$AccordionGrad_drenasje[
    which(RegData$AccordionGrad_drenasje==3 & RegData$KunDrenasje ==1)] <- 2
}

RegData$Variabel <- RegData[, valgtVar]
RegData <- RegData[!is.na(RegData$Variabel), ]
RegData$Variabel <- as.factor(RegData$Variabel)

if (valgtVar == 'Tilgang') {
  RegData <- RegData[which(RegData$Tilgang %in% 1:3), ]}

NorgastUtvalg <- NorgastUtvalg(
  RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
  maxald=maxald, erMann=erMann, elektiv=elektiv, hastegrad = hastegrad,
  hastegrad_hybrid=hastegrad_hybrid, ny_anastomose = ny_anastomose,
  BMI=BMI, valgtShus=valgtShus, tilgang=tilgang, minPRS=minPRS,
  maxPRS=maxPRS, modGlasgow=modGlasgow,
  ASA=ASA, whoEcog=whoEcog, forbehandling=forbehandling, malign=malign,
  op_gruppe=op_gruppe, ncsp=ncsp,
  robotassiastanse=robotassiastanse, kun_ferdigstilte=kun_ferdigstilte,
  tilgang_utvidet=tilgang_utvidet, accordion=accordion, icd_kode=icd_kode)
RegData <- NorgastUtvalg$RegData
utvalgTxt <- NorgastUtvalg$utvalgTxt

RegData[ ,grVar] <- as.factor(as.character(RegData[ ,grVar]))
N <- dim(RegData)[1]

tittel <- switch (
  valgtVar,
  'ModGlasgowScore' = 'Modified Glasgow score',
  'AccordionGrad' = 'Komplikasjoner (Accordion score)',
  'AccordionGrad_drenasje' = 'Komplikasjoner (Accordion score)',
  'Tilgang' = 'Tilgang i abdomen',
  'ThoraxTilgang' = 'Tilgang i thorax',
  'AvstandAnalVerge_kat' = 'Avstand tumors nedre margin til analkanten '
)
legendTxt <- switch (
  valgtVar,
  'ModGlasgowScore' = c('0','1', '2'),
  'AccordionGrad' = c('3','4', '5', '6'),
  'AccordionGrad_drenasje' = c(
    '3 (kun drenasje av \n pleuravæske/ascites)',
    '3 (resten)','4', '5', '6'),
  'Tilgang' = c('Åpen', 'Laparoskopi', 'Konvertert'),
  'ThoraxTilgang' = c('Thoracotomi', 'Thorakoskopi',
                      'Ingen (transhiatal)', 'Konvertert til åpen'),
  'AvstandAnalVerge_kat' = levels(RegData$AvstandAnalVerge_kat)
)
legendTitle <- switch (
  valgtVar,
  'ModGlasgowScore' = NULL,
  'AccordionGrad' = 'Accordiongrad',
  'AccordionGrad_drenasje' = 'Accordiongrad',
  'Tilgang' = NULL,
  'ThoraxTilgang' = NULL,
  'AvstandAnalVerge_kat' = NULL
)


tmp <- RegData |>
  dplyr::mutate(Variabel = factor(
    Variabel,
    levels = 1:6,
    labels = c('<3',
               '3 (kun drenasje av \n pleuravæske/ascites)',
               '3 (resten)','4', '5', '6'))) |>
  dplyr::count(Sykehusnavn, Variabel, .drop = FALSE) |>
  janitor::adorn_totals() |>
  dplyr::group_by(Sykehusnavn) |>
  dplyr::group_modify(~ .x |> janitor::adorn_totals(name = "Sum")) |>
  dplyr::filter(Variabel != "-")
# |>
#   tidyr::pivot_wider(names_from = Variabel,
#                      values_from = n, values_fill = 0)
#


write.csv2(tmp, "tmp.csv", row.names = FALSE, fileEncoding = "Latin1")


library(tidyverse)
library(readr)
library(forcats)

lav_dekning <- c(
  "NS-Lofoten",
  "NS-Vesterålen",
  "OUS-Rikshospitalet",
  "Ålesund"
)

dat <- tmp |>
  filter(Variabel != "Sum") |>
  group_by(Sykehusnavn) |>
  mutate(
    total = sum(n),
    prosent = n / total * 100
  ) |>
  ungroup() |>

  # ✅ flagg
  mutate(
    lav_dekning_flag = Sykehusnavn %in% lav_dekning,
    n_lt10_flag = total < 10
  )

dat_plot <- dat |>
  filter(Variabel != "<3") |>

  mutate(
    gruppe = case_when(
      lav_dekning_flag ~ "Dekningsgrad < 60 %",
      n_lt10_flag ~ "N < 10",
      TRUE ~ Sykehusnavn
    ),

    prosent_plot = if_else(
      lav_dekning_flag | n_lt10_flag,
      NA_real_,
      prosent
    )
  )


skjulte_rader <- dat |>
  distinct(Sykehusnavn, total, lav_dekning_flag, n_lt10_flag) |>
  filter(lav_dekning_flag | n_lt10_flag) |>
  mutate(
    gruppe = case_when(
      lav_dekning_flag ~ "Dekningsgrad < 60 %",
      n_lt10_flag ~ "N < 10"
    ),
    prosent_plot = NA_real_
  ) |>
  tidyr::expand(
    Sykehusnavn,
    Variabel = c(
      "3 (kun drenasje av \n pleuravæske/ascites)",
      "3 (resten)",
      "4",
      "5",
      "6"
    )
  )
dat_final <- bind_rows(
  dat_plot |> filter(!lav_dekning_flag & !n_lt10_flag),
  skjulte_rader
)
dat_final <- dat_final |>
  group_by(gruppe, Sykehusnavn) |>
  mutate(
    soylelengde = sum(prosent_plot, na.rm = TRUE)
  ) |>
  ungroup() |>

  # 🔑 3-nivå sortering
  mutate(
    gruppe_type = case_when(
      gruppe == "Dekningsgrad < 60 %" ~ 1,
      gruppe == "N < 10" ~ 2,
      TRUE ~ 3
    )
  ) |>

  arrange(gruppe_type, soylelengde) |>

  mutate(
    y_label = factor(
      paste0(gruppe, "_", row_number()),
      levels = paste0(gruppe, "_", row_number())
    ),
    y_vis = gruppe
  )

accordion_farger <- c(
  "3 (kun drenasje av \n pleuravæske/ascites)" = "#67B6C8",
  "3 (resten)" = "#EBBE33",
  "4" = "#F29D71",
  "5" = "#C44E52",
  "6" = "#EDEDED"
)

x11()
ggplot(dat_final,
       aes(x = prosent_plot, y = y_label, fill = Variabel)) +
  geom_col(width = 0.8, colour = "white", na.rm = TRUE) +

  scale_fill_manual(values = accordion_farger) +

  scale_y_discrete(
    labels = dat_final$y_vis
  ) +

  labs(
    x = "Prosent av alle operasjoner (%)",
    y = NULL,
    title = "Komplikasjoner (Accordion score ≥ 3)",
    subtitle = "Elektiv benign kolonreseksjon\n(<3 i nevner, ikke vist)"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )





lav_dekning <- c(
  "NS-Lofoten",
  "NS-Vesterålen",
  "OUS-Rikshospitalet",
  "Ålesund"
)

dat <- tmp |>
  filter(Variabel != "Sum") |>
  group_by(Sykehusnavn) |>
  mutate(
    total = sum(n),
    prosent = n / total * 100,
    lav_dekning = Sykehusnavn %in% lav_dekning
  ) |>
  ungroup()


dat_plot <- dat |>
  filter(Variabel != "<3") |>
  group_by(Sykehusnavn) |>
  mutate(
    soylelengde = sum(prosent)
  ) |>
  ungroup() |>
  mutate(
    Sykehusnavn = forcats::fct_reorder(Sykehusnavn, soylelengde)
  ) |>
  mutate(
    Variabel = factor(
      Variabel,
      levels = c(
        "3 (kun drenasje av \n pleuravæske/ascites)",
        "3 (resten)",
        "4",
        "5",
        "6"
      )
    )
  )


x11()
ggplot(dat_plot, aes(x = prosent, y = Sykehusnavn, fill = Variabel)) +
  geom_col(width = 0.8, colour = "white") +
  scale_fill_manual(values = accordion_farger, name = "Accordion score") +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Prosent av alle operasjoner (%)",
    y = NULL,
    title = "Komplikasjoner (Accordion score ≥ 3)",
    subtitle = "Elektiv benign kolonreseksjon\n(<3 inngår i nevner, men er ikke vist)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(
      colour = ifelse(
        levels(dat_plot$Sykehusnavn) %in% lav_dekning,
        "red",
        "black"
      )
    )
  )

dat_plot <- dat |>
  mutate(
    n_total = total,
    n_lt10 = n_total < 10,

    label = case_when(
      lav_dekning ~ "Dekningsgrad < 60 %",
      n_lt10 ~ "N < 10",
      TRUE ~ Sykehusnavn
    ),

    # ❗ skjul data i plottet for disse radene
    prosent_plot = case_when(
      lav_dekning ~ NA_real_,
      n_lt10 ~ NA_real_,
      TRUE ~ prosent
    )
  ) |>
  group_by(label, Variabel) |>
  summarise(
    prosent_plot = sum(prosent_plot, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(label) |>
  mutate(
    soylelengde = sum(prosent_plot, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    label = forcats::fct_reorder(label, soylelengde)
  )


ggplot(dat_plot, aes(x = prosent_plot, y = label, fill = Variabel)) +
  geom_col(width = 0.8, colour = "white", na.rm = TRUE) +
  scale_fill_manual(values = accordion_farger) +
  labs(
    x = "Prosent av alle operasjoner (%)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )









dat <- dat |>
  mutate(
    Variabel = factor(
      Variabel,
      levels = c(
        "<3",
        "3 (kun drenasje av \n pleuravæske/ascites)",
        "3 (resten)",
        "4",
        "5",
        "6"
      )
    )
  )

accordion_farger <- c(
  "<3"                                      = "#4A7695",
  "3 (kun drenasje av \n pleuravæske/ascites)" = "#67B6C8",
  "3 (resten)"                              = "#EBBE33",
  "4"                                       = "#F29D71",
  "5"                                       = "#C44E52",
  "6"                                       = "#EDEDED"
)

dat <- dat |>
  mutate(
    Sykehusnavn = fct_reorder(
      Sykehusnavn,
      total
    )
  )

ggplot(dat, aes(x = prosent, y = Sykehusnavn, fill = Variabel)) +
  geom_col(width = 0.8, colour = "white") +
  scale_fill_manual(values = accordion_farger, name = "Accordion score") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "Prosent (%)",
    y = NULL,
    title = "Komplikasjoner (Accordion score)",
    subtitle = "Elektiv benign kolonreseksjon"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(
      colour = ifelse(
        levels(dat$Sykehusnavn) %in% lav_dekning,
        "red",
        "black"
      )
    )
  )





appdata <- norgast::NorgastHentData()
RegData <- appdata$RegData |> norgast::NorgastPreprosess()

table(RegData[RegData$Aar == 2025,c("Sykehusnavn", "Hastegrad")]) |>
  arrange()

RegData |> filter(Hastegrad==2,
                  !(Op_gr %in% 1:8)) |>
  summarise(N = n(), .by = Sykehusnavn) |>
  arrange(-N)


valgtVar='ModGlasgowScore';
datoFra='2014-01-01'; datoTil='2050-12-31';
minald=0; maxald=130; erMann=99; outfile=''; hastegrad_hybrid=99;
preprosess=F; malign=99; Ngrense=30; lavDG=''; ny_anastomose = 99;
lavDGtekst='Dekningsgrad < 60 %'; hastegrad = 99; icd_kode='';
elektiv=99; BMI=''; tilgang=''; valgtShus=c(''); minPRS=0; modGlasgow='';
maxPRS=2.2; ASA=''; whoEcog= ''; forbehandling=''; hentData=0; op_gruppe='';
ncsp=''; robotassiastanse=99; kun_ferdigstilte=TRUE; skriftStr=1;
tilgang_utvidet=''; accordion=''; snufarger = TRUE

appdata <- norgast::NorgastHentData()
RegData <- appdata$RegData |> norgast::NorgastPreprosess()




appdata <- norgast::NorgastHentData()

regdata_gml <- norgast::NorgastHentRegData()

query <- "SELECT
	mce.MCEID AS ForlopsID,
	mce.PATIENT_ID AS PasientId,
	mce.CENTREID AS AvdRESH,

	-- Patient stuff
	patient.SSN AS Fodselsnummer,
	patient.DECEASED AS Avdod,
	patient.DECEASED_DATE AS AvdodDato,
	centre.CENTRENAME AS SenterNavn,

	-- Registration stuff
	-- Innleggelse
	registration.PREVIOUS_WEIGHT AS Vekt6MndFoer,
	registration.PREVIOUS_WEIGHT_MISS AS Vekt6MndFoerUkjent,
	registration.ADMISSION_WEIGHT AS VektVedInnleggelse,
	registration.ADMISSION_WEIGHT_MISS AS VektVedInnleggelseUkjent,
	registration.HEIGHT AS Hoyde,
	registration.HEIGHT_MISS AS HoydeUkjent,
	registration.BMI AS BMI,
	registration.BMI_CATEGORY AS BMIKategori,
	registration.WEIGHTLOSS AS VekttapProsent,
	registration.DIABETES AS MedDiabetes,
	registration.CHEMOTHERAPY_ONLY AS KunCytostatika,
	registration.RADIATION_THERAPY_ONLY AS KunStraaleterapi,
	registration.CHEMORADIOTHERAPY AS KjemoRadioKombo,
	registration.WHO_ECOG_SCORE AS WHOECOG,
	registration.ALBUMIN AS Albumin,
	registration.CRP AS CRP,
	registration.GLASGOW_SCORE AS GlasgowScore,
	registration.MODIFIED_GLASGOW_SCORE AS ModGlasgowScore,

	-- Anestesi
	registration.ASA AS ASA,
	registration.LUNG_DISEASE AS Lungesykdom,
	registration.HEART_DISEASE AS Hjertesykdom,
	registration.URGENCY AS Hastegrad,
	registration.ANESTHESIA_START AS AnestesiStartKl,
	registration.PRS_SCORE AS PRSScore,

	-- Intervensjonen
	registration.OPERATION_DATE AS OpDato,
	registration.NCSP AS Hovedoperasjon,
    registration.ABLATION AS LeverAblasjon,
	registration.RECONSTRUCTION AS Rekonstruksjon,
	registration.RECONSTRUCTION_TYPE AS Rekonstruksjonstype,
	registration.ANASTOMOSIS_LEVEL AS Anastomoseniva,
	registration.ANASTOMOSIS AS NyAnastomose,
    registration.ANAL_GUARD_DISTANCE AS AvstandAnalVerge,
    registration.ANAL_GUARD_DISTANCE_MISS AS AvstandAnalVergeIkkeAkt,
	registration.TATME AS TaTME,
	registration.OSTOMY AS NyStomi,
	registration.ABDOMINAL_ACCESS AS Tilgang,
	registration.ROBOTASSISTANCE AS Robotassistanse,
	registration.THORAX_ACCESS AS ThoraxTilgang,

	-- Komplikasjoner
	registration.RELAPAROTOMY AS ReLapNarkose,
	registration.RELAPAROTOMY_YES AS ViktigsteFunn,
    registration.FINDINGS_SPESIFISER AS FunnSpesifiser,
	registration.RELAPAROTOMY_NO AS AnnenOpIAnestsi,
	registration.INTERVENTION_WITHOUT_ANESTHESIA AS IntUtenAnestesi,
	registration.PERCUTANEOUS_DRAINAGE AS PerkDrenasje,
	registration.HIGH_AMYLASE_CONCENTRATION AS HoyAmylaseKons,
	registration.LEAK_INTERVENTION AS EndoInterLekkasje,
	registration.BLEED_INTERVENTION AS EndoInterBlod,
	registration.ANGIO_INTERVENTION AS AngioInter,
	registration.LIQUID_DRAINAGE AS KunDrenasje,
	registration.SINGLE_ORGAN_FAILURE AS EttOrganSvikt,
	registration.MULTI_ORGAN_FAILURE AS MultiOrganSvikt,
	registration.IN_HOUSE_DEATH AS DodUnderOpphold,
	registration.IN_HOUSE_DEATH_DATE AS DodUnderOppholdDato,
	registration.ACCORDION_SCORE AS AccordionGrad,

	-- Utskrivelse
	registration.DISCHARGE_DATE AS UtskrivelseDato,
	registration.BED_DAYS AS PostopLiggedogn,
	registration.ICD10 AS Hoveddiagnose,
	registration.DISCHARGE_TO AS UtskrevetTil,

	-- Generelt
	registration.FIRST_TIME_CLOSED AS ForstLukket,
	registration.FIRST_TIME_CLOSED_BY AS ForstLukketAv,
	registration.STATUS AS RegistreringStatus,

	-- Oppfølging
	-- Reinnleggelse/oppfølging
	readmission.OWN_INSTITUTION AS ReinnlEgenInst,
	readmission.OTHER_INSTITUTIONS AS ReinnlAndreInst,
	readmission.CONTROL AS AktivKontroll,
	readmission.PHYSICAL_CONTROL AS FysiskKontroll,
	readmission.PHONE_CONTROL AS TelefonKontroll,

	-- Komplikasjoner
	readmission.RELAPAROTOMY AS OppfReLapNarkose,
	readmission.RELAPAROTOMY_YES AS OppfViktigsteFunn,
    readmission.FINDINGS_SPESIFISER AS OppfFunnSpesifiser,
	readmission.RELAPAROTOMY_NO AS OppfAnnenOpIAnestsi,
	readmission.INTERVENTION_WITHOUT_ANESTHESIA AS OppfIntUtenAnestesi,
	readmission.PERCUTANEOUS_DRAINAGE AS OppfPerkDrenasje,
	readmission.HIGH_AMYLASE_CONCENTRATION AS OppfHoyAmylaseKons,
	readmission.LEAK_INTERVENTION AS OppfEndoInterLekkasje,
	readmission.BLEED_INTERVENTION AS OppfEndoInterBlod,
	readmission.ANGIO_INTERVENTION AS OppfAngioInter,
	readmission.LIQUID_DRAINAGE AS OppfKunDrenasje,
	readmission.SINGLE_ORGAN_FAILURE AS OppfEttOrganSvikt,
	readmission.MULTI_ORGAN_FAILURE AS OppfMultiOrganSvikt,
	readmission.IN_HOUSE_DEATH AS OppfDodUnderOpphold,
	readmission.IN_HOUSE_DEATH_DATE AS OppfDodUnderOppholdDato,
	readmission.FIRST_TIME_CLOSED AS OppfForstLukket,
	readmission.FIRST_TIME_CLOSED_BY AS OppfForstLukketAv,
	readmission.ACCORDION_SCORE AS OppfAccordionGrad
	-- getStatusText(readmission.STATUS) AS OppfStatus

FROM
	mce INNER JOIN patient ON mce.PATIENT_ID = patient.ID
    INNER JOIN registration on mce.MCEID = registration.MCEID
    INNER JOIN centre on centre.ID=mce.CENTREID
    LEFT OUTER JOIN readmission on mce.MCEID = readmission.MCEID
WHERE
	registration.OPERATION_DATE > '2014.01.01';   -- Konsesjonsdato"

tictoc::tic()
allevarnum <- rapbase::loadRegData("data", query)
tictoc::toc()

tictoc::tic()
allevarnum_gml <- rapbase::loadRegData("data", "SELECT * FROM allevarnum")
tictoc::toc()

tictoc::tic()
registryName <- "data"
datoFra = '2014-01-01'; datoTil = '2099-01-01'
registration <- rapbase::loadRegData(registryName, "SELECT * FROM registration")
readmission <- rapbase::loadRegData(registryName, "SELECT * FROM readmission")
centre <- rapbase::loadRegData(registryName, "SELECT * FROM centre")
user <- rapbase::loadRegData(registryName, "SELECT * FROM user")
centreattribute <- rapbase::loadRegData(registryName, "SELECT * FROM centreattribute") |>
  dplyr::filter(ATTRIBUTENAME == "FRIENDLYNAME")
mce <- rapbase::loadRegData(registryName, "SELECT * FROM mce")
patient <- rapbase::loadRegData(registryName, "SELECT * FROM patient")

varnavn_kobl <-
  data.frame(
    kol =
      c("mce.MCEID AS ForlopsID",
        "mce.PATIENT_ID AS PasientID",
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
        "registration.ANASTOMOTIC_LEAK AS ANASTOMOTIC_LEAK",
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
        "registration.FIRST_TIME_CLOSED AS ForstLukket",
        "registration.FIRST_TIME_CLOSED_BY AS ForstLukketAv",
        "registration.STATUS AS RegistreringStatus",
        "readmission.OWN_INSTITUTION AS ReinnlEgenInst",
        "readmission.OTHER_INSTITUTIONS AS ReinnlAndreInst",
        "readmission.CONTROL AS AktivKontroll",
        "readmission.PHYSICAL_CONTROL AS FysiskKontroll",
        "readmission.PHONE_CONTROL AS TelefonKontroll",
        "readmission.RELAPAROTOMY AS OppfReLapNarkose",
        "readmission.RELAPAROTOMY_YES AS OppfViktigsteFunn",
        "readmission.ANASTOMOTIC_LEAK AS OppfANASTOMOTIC_LEAK",
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
        "readmission.FIRST_TIME_CLOSED AS OppfForstLukket",
        "readmission.FIRST_TIME_CLOSED_BY AS OppfForstLukketAv",
        "readmission.ACCORDION_SCORE AS OppfAccordionGrad",
        "readmission.STATUS AS OppfStatus")
  )|>
  tidyr::separate(col="kol",
                  into=c("dbnavn", "rapporteket"),
                  sep = " AS ") |>
  tidyr::separate(col="dbnavn",
                  into=c("tabell", "var_navn"),
                  extra = "merge")

varnavn_mce <-
  setNames(varnavn_kobl$var_navn[varnavn_kobl$tabell == "mce"],
           varnavn_kobl$rapporteket[varnavn_kobl$tabell == "mce"])
varnavn_patient <-
  setNames(varnavn_kobl$var_navn[varnavn_kobl$tabell == "patient"],
           varnavn_kobl$rapporteket[varnavn_kobl$tabell == "patient"])
varnavn_registration <-
  setNames(varnavn_kobl$var_navn[varnavn_kobl$tabell == "registration"],
           varnavn_kobl$rapporteket[varnavn_kobl$tabell == "registration"])
varnavn_centre <-
  setNames(varnavn_kobl$var_navn[varnavn_kobl$tabell == "centre"],
           varnavn_kobl$rapporteket[varnavn_kobl$tabell == "centre"])
varnavn_readmission <-
  setNames(varnavn_kobl$var_navn[varnavn_kobl$tabell == "readmission"],
           varnavn_kobl$rapporteket[varnavn_kobl$tabell == "readmission"])

allevarnum <- merge(
  mce |>
    dplyr::select(varnavn_kobl$var_navn[varnavn_kobl$tabell == "mce"]) |>
    dplyr::rename(!!!varnavn_mce),
  patient |>
    dplyr::select(varnavn_kobl$var_navn[varnavn_kobl$tabell == "patient"], ID) |>
    dplyr::rename(!!!varnavn_patient),
  by.x = "PasientID", by.y = "ID", all.x = TRUE
) |> merge(
  registration |>
    dplyr::select(varnavn_kobl$var_navn[varnavn_kobl$tabell == "registration"],
                  MCEID, ICD10_VERSION, NCSP_VERSION) |>
    dplyr::rename(!!!varnavn_registration),
  by.x = "ForlopsID", by.y = "MCEID", all.y = TRUE
) |> dplyr::mutate(SenterNavn = centre$CENTRENAME[match(AvdRESH, centre$ID)]) |>
  merge(
    readmission |>
      dplyr::select(varnavn_kobl$var_navn[varnavn_kobl$tabell == "readmission"],
                    MCEID) |>
      dplyr::rename(!!!varnavn_readmission),
    by.x = "ForlopsID", by.y = "MCEID", all.x = TRUE
  ) |>
  dplyr::filter(OpDato >= datoFra,
                OpDato <= datoTil) |>
  dplyr::arrange(ForlopsID)


tictoc::toc()







rm(list = ls())

appdata <- norgast::NorgastHentData()
RegData_ny <- appdata$RegData
RegData_gml <- norgast::NorgastHentRegData()

tmp <- RegData_gml |>
  select(all_of(setdiff(names(RegData_gml), names(RegData_ny))))
tmp2 <- RegData_ny |>
  select(all_of(setdiff(names(RegData_ny), names(RegData_gml))))

table(RegData_gml$Avdod, useNA = 'ifany')
table(RegData_ny$Avdod, useNA = 'ifany')


lekk <- RegData |>
  filter(NyAnastomose == 1,
         Aar == 2026) |>
  summarise(anastomelekk = sum(Anastomoselekkasje),
            N = n(),
            pst = anastomelekk/N*100,
            .by = Mnd)



query <- "SELECT
	mce.MCEID AS ForlopsID,
	mce.PATIENT_ID AS PasientId,
	mce.CENTREID AS AvdRESH,

	-- Patient stuff
	patient.DECEASED AS Avdod,
	patient.DECEASED_DATE AS AvdodDato,
	registration.OPERATION_DATE AS OpDato,

	-- Registration stuff

	-- Komplikasjoner
	registration.RELAPAROTOMY_YES AS ViktigsteFunn,
    registration.ANASTOMOTIC_LEAK AS ANASTOMOTIC_LEAK,
    CASE
		WHEN registration.ANASTOMOTIC_LEAK = 1 THEN 1
        WHEN registration.ANASTOMOTIC_LEAK = 0 THEN registration.RELAPAROTOMY_YES
        WHEN registration.ANASTOMOTIC_LEAK IS NULL THEN registration.RELAPAROTOMY_YES
	END AS ViktigsteFunn2,
    CASE
		WHEN registration.RELAPAROTOMY_YES IS NULL THEN registration.ANASTOMOTIC_LEAK
        ELSE registration.RELAPAROTOMY_YES
	END AS ViktigsteFunn3,

	readmission.RELAPAROTOMY_YES AS OppfViktigsteFunn,
    readmission.ANASTOMOTIC_LEAK AS OppfANASTOMOTIC_LEAK,
        CASE
		WHEN readmission.RELAPAROTOMY_YES IS NULL THEN readmission.ANASTOMOTIC_LEAK
        ELSE readmission.RELAPAROTOMY_YES
	END AS OppfViktigsteFunn2
FROM
	mce INNER JOIN patient ON mce.PATIENT_ID = patient.ID
    INNER JOIN registration on mce.MCEID = registration.MCEID
    INNER JOIN centre on centre.ID=mce.CENTREID
    LEFT OUTER JOIN readmission on mce.MCEID = readmission.MCEID
WHERE
	registration.OPERATION_DATE > '2014.01.01' AND
    (registration.RELAPAROTOMY_YES IS NOT NULL OR
    readmission.RELAPAROTOMY_YES IS NOT NULL OR
    registration.ANASTOMOTIC_LEAK IS NOT NULL OR
    readmission.ANASTOMOTIC_LEAK IS NOT NULL);"

tmpdata <- rapbase::loadRegData(
  "data", query)


appdata <- norgast::NorgastHentData()

RegData_ufilt <- appdata$RegData
RegData <- RegData_ufilt |>
  NorgastPreprosess() #|>
# filter(PasientID %in% c(75149, 83001, 83153, 83234)) |>
# select(PasientID, OpDato, ViktigsteFunn,
#        OppfViktigsteFunn, Anastomoselekkasje,
#        ANASTOMOTIC_LEAK, OppfANASTOMOTIC_LEAK,
#        Avdod, AvdodDato)

ufilt <- merge(RegData_ufilt |>
                 select(ForlopsID, PasientID, OpDato, ViktigsteFunn,
                        OppfViktigsteFunn,
                        ANASTOMOTIC_LEAK, OppfANASTOMOTIC_LEAK,
                        Avdod, AvdodDato),
               RegData |> select(ForlopsID, Anastomoselekkasje),
               by = "ForlopsID") |>
  filter(Anastomoselekkasje == 1)

ufilt2 <- merge(RegData_ufilt |>
                  select(ForlopsID, PasientID, OpDato, ViktigsteFunn,
                         OppfViktigsteFunn,
                         ANASTOMOTIC_LEAK, OppfANASTOMOTIC_LEAK,
                         Avdod, AvdodDato, NyAnastomose),
                RegData |> select(ForlopsID, Anastomoselekkasje),
                by = "ForlopsID") |>
  rowwise() |>
  filter(1 %in% c(Anastomoselekkasje, ViktigsteFunn, OppfViktigsteFunn,
                  ANASTOMOTIC_LEAK, OppfANASTOMOTIC_LEAK)) %>%
  filter(ForlopsID %in% setdiff(ForlopsID, ufilt$ForlopsID))





registration <- rapbase::loadRegData(
  "data", "SELECT * FROM registration")
readmission <- rapbase::loadRegData(
  "data", "SELECT * FROM readmission")
allevarnum_hnikt <- rapbase::loadRegData(
  "data", "SELECT * FROM allevarnum")



# RegData <- merge(
#   RegData,
#   registration |> select(MCEID, ANASTOMOTIC_LEAK),
#   by.x = "ForlopsID", by.y = "MCEID") |>
#   merge(readmission |> select(MCEID, ANASTOMOTIC_LEAK),
#         by.x = "ForlopsID", by.y = "MCEID", suffixes = c("", "_Oppf"),
#         all.x = TRUE)


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


reg_manglerpas <- registration |>
  filter(MCEID %in% avvik1$MCEID)
readm_manglerpas <- readmission |>
  filter(MCEID %in% avvik1$MCEID)
table(reg_manglerpas$STATUS, useNA = 'ifany')
table(readm_manglerpas$STATUS, useNA = 'ifany')

manglerpas <- merge(reg_manglerpas,
                    readm_manglerpas,
                    by = "MCEID", all = TRUE,
                    suffixes = c("", "_readm"))

table(manglerpas[,c("STATUS", "STATUS_readm")], useNA = 'ifany')

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
