# registration <- rapbase::loadRegData(
#   "data",
#   paste0("SELECT * FROM registration WHERE OPERATION_DATE >= \'",
#          datoFra, "\' AND OPERATION_DATE <= \'", datoTil, "\' ")
#   )
registration <- rapbase::loadRegData("data", "SELECT * FROM registration")
readmission <- rapbase::loadRegData("data", "SELECT * FROM readmission")
centre <- rapbase::loadRegData("data", "SELECT * FROM centre")
reg_questionnaires <- rapbase::loadRegData("data", "SELECT * FROM reg_questionnaires")
user <- rapbase::loadRegData("data", "SELECT * FROM user")
centreattribute <- rapbase::loadRegData("data", "SELECT * FROM centreattribute") |>
  dplyr::filter(ATTRIBUTENAME == "FRIENDLYNAME")
mce <- rapbase::loadRegData("data", "SELECT * FROM mce")
patient <- rapbase::loadRegData("data", "SELECT * FROM patient")

skjemaoversikt <- registration |>
  dplyr::mutate(Skjemanavn = "Registrering") |>
  dplyr::select(Skjemanavn, STATUS, MCEID, CREATEDBY, OPERATION_DATE,
                UPDATEDBY, TSCREATED, TSUPDATED, CENTREID) |>
  dplyr::mutate(
    Sykehusnavn = centreattribute$ATTRIBUTEVALUE[
      match(CENTREID, centreattribute$ID)],
    OpprettetAv = paste0(user$FIRSTNAME[match(CREATEDBY, user$ID)], " ",
                         user$LASTNAME[match(CREATEDBY, user$ID)]),
    SistLagretAv = paste0(user$FIRSTNAME[match(UPDATEDBY, user$ID)], " ",
                          user$LASTNAME[match(UPDATEDBY, user$ID)])
  ) |>
  dplyr::rename(SkjemaStatus = STATUS,
                ForlopsID = MCEID,
                OpprettetDato = TSCREATED,
                SistLagretDato = TSUPDATED,
                HovedDato = OPERATION_DATE,
                AvdRESH = CENTREID) |>
  dplyr::select(Skjemanavn, SkjemaStatus, ForlopsID, OpprettetAv,
                OpprettetDato, SistLagretAv, SistLagretDato, Sykehusnavn,
                HovedDato, AvdRESH) |>
  dplyr::bind_rows(
    readmission |>
      dplyr::mutate(Skjemanavn = "Oppfolging/Innleggelse") |>
      dplyr::select(Skjemanavn, STATUS, MCEID, CREATEDBY,
                    UPDATEDBY, TSCREATED, TSUPDATED, CENTREID) |>
      dplyr::mutate(
        Sykehusnavn = centreattribute$ATTRIBUTEVALUE[
          match(CENTREID, centreattribute$ID)],
        HovedDato = registration$OPERATION_DATE[
          match(MCEID, registration$MCEID)],
        OpprettetAv = paste0(user$FIRSTNAME[match(CREATEDBY, user$ID)], " ",
                             user$LASTNAME[match(CREATEDBY, user$ID)]),
        SistLagretAv = paste0(user$FIRSTNAME[match(UPDATEDBY, user$ID)], " ",
                              user$LASTNAME[match(UPDATEDBY, user$ID)])
      ) |>
      dplyr::rename(SkjemaStatus = STATUS,
                    ForlopsID = MCEID,
                    OpprettetDato = TSCREATED,
                    SistLagretDato = TSUPDATED,
                    AvdRESH = CENTREID) |>
      dplyr::select(Skjemanavn, SkjemaStatus, ForlopsID, OpprettetAv,
                    OpprettetDato, SistLagretAv, SistLagretDato, Sykehusnavn,
                    HovedDato, AvdRESH)
  )

# skjemaoversikt_ny = dplyr::bind_rows(aux1, aux2)
skjemaoversikt_gml <- rapbase::loadRegData("data", "SELECT * FROM skjemaoversikt") |>
  dplyr::mutate(ForlopsID = as.numeric(ForlopsID))

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

allevarnum_ny <- merge(
  mce |> dplyr::select(varnavn_kobl$var_navn[varnavn_kobl$tabell == "mce"]) |>
    dplyr::rename(!!!varnavn_mce),
  patient |> dplyr::select(varnavn_kobl$var_navn[varnavn_kobl$tabell == "patient"], ID) |>
    dplyr::rename(!!!varnavn_patient),
  by.x = "PasientID", by.y = "ID"
) |> merge(
  registration |> dplyr::select(varnavn_kobl$var_navn[varnavn_kobl$tabell == "registration"], MCEID) |>
    dplyr::rename(!!!varnavn_registration),
  by.x = "ForlopsID", by.y = "MCEID", all = TRUE
) |> dplyr::mutate(SenterNavn = centre$CENTRENAME[match(AvdRESH, centre$ID)]) |>
  merge(
    readmission |> dplyr::select(varnavn_kobl$var_navn[varnavn_kobl$tabell == "readmission"], MCEID) |>
      dplyr::rename(!!!varnavn_readmission),
    by.x = "ForlopsID", by.y = "MCEID", all.x = TRUE
  ) |>
  dplyr::filter(OpDato >= "2014-01-01") |>
  dplyr::mutate(ForstLukket = as.Date(ForstLukket),
                OppfForstLukket = as.Date(OppfForstLukket)) |>
  dplyr::arrange(ForlopsID)

allevarnum_gml <- rapbase::loadRegData("data", "SELECT * FROM allevarnum") |>
  dplyr::mutate(ForstLukket = as.Date(ForstLukket),
                OppfForstLukket = as.Date(OppfForstLukket)) |>
  dplyr::arrange(ForlopsID)
forlopsoversikt_gml <- rapbase::loadRegData("data", "SELECT * FROM forlopsoversikt") |>
  dplyr::arrange(ForlopsID)

gml <- lapply(allevarnum_gml, class) |> unlist()
gml <- gml[order(names(gml))]
ny <- lapply(allevarnum_ny, class) |> unlist()
ny <- ny[order(names(ny))]

ulik_format <- data.frame(gml = gml, ny = ny) |> dplyr::filter(gml != ny)

ny <- allevarnum_ny[, row.names(ulik_format)]
gml <- allevarnum_gml[, row.names(ulik_format)]











