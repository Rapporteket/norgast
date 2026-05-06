library(dplyr)

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
