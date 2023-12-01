#' Beregn NORGAST sine indikatorer
#'
#' Denne funksjonen beregner NORGAST sine offisielle kvalitetsindikatorer
#'
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param ind_id Indikatornavn
#'
#' @export
norgastBeregnIndikator <- function(RegData, ind_id) {

  map_resh_orgnr <- data.frame(
    orgnr_sh = c(974733013, 974631407, 974557746, 974632535,
                 974795787, 974705788, 974633574, 974795639,
                 974724960, 974795361, 993467049, 974631326,
                 974749025, 974706490, 974703300, 974633752,
                 874632562, 974743272, 974795515, 974116804,
                 974747138, 974745569, 974795833, 974633191,
                 974724774, 974631091, 974795477, 974329506,
                 974316285, 974753898, 974631407, 974795558,
                 974795574, 874716782, 974707152, 974589095,
                 974754118, 974747545, 983974732),
    resh = c(100353,4204126, 700922, 108355,
             601225, 103091, 100100, 601231,
             108354, 706264, 700413, 4204082,
             107440, 108162, 114271,4209222,
             108357, 102939, 102141, 107505,
             708761,4204500, 101823, 102037,
             701402, 100354, 102145,4211928,
             100170,4212917, 4204084, 700840,
             700841, 103312, 4205289, 106168,
             4207594, 4216823, 100315))
  map_resh_orgnr$Sykehus <- RegData$Sykehusnavn[match(map_resh_orgnr$resh,
                                                      RegData$AvdRESH)]

  maalretn <- 'hoy'

  if (ind_id == "norgast_vekt_reg") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 1:8, # Kun obligatoriske
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    Hastegrad_hybrid==1) %>%
      dplyr::mutate(var = ifelse(is.na(VekttapProsent), 0, 1),
                    context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_vekt_reg",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::rename(year = Aar) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    terskel <- 10
    minstekrav <- 80
    maal <- 90
    decreasing <- F
    tittel <- "Andel med premorbid vekttap registrert"
    utvalgTxt <- c("Hastegrad: Elektiv")
  }

  if (ind_id == "norgast_aktivkontroll") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 1:8, # Kun obligatoriske
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    Hastegrad_hybrid==1) %>%
      dplyr::mutate(var = case_when(TelefonKontroll==1 | FysiskKontroll==1 ~ 1,
                                    TelefonKontroll==0 & FysiskKontroll==0 ~ 0),
                    context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_aktivkontroll",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::rename(year = Aar) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing <- F
    terskel <- 10
    minstekrav <- 70
    maal <- 90
    tittel <- "Andel med aktiv kontroll"
    utvalgTxt <- c("Hastegrad: Elektiv")
  }


  if (ind_id == "norgast_saarruptur") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 1:8, # Kun obligatoriske
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    Hastegrad_hybrid==1, # Kun elektive
                    Tilgang %in% c(1, 3),
                    Saarruptur %in% c(0,1)) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_saarruptur",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Saarruptur) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 4
    maal = 3
    tittel <- "Andel med sårruptur"
    utvalgTxt <- c("Hastegrad: Elektiv", "Tilgang: Åpen, Konvertert")
  }

  if (ind_id == "norgast_lekkasje_tykktarm") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 1, # Kolon
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    WHOECOG %in% 0:1,
                    Malign == 1,
                    Hastegrad_hybrid == 1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_lekkasje_tykktarm",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Anastomoselekkasje) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 6
    maal = 4
    tittel <- "Andel anastomoselekkasjer, ny anastomose"
    utvalgTxt <- c("Operasjonsgruppe: Kolonreseksjoner", "WHO ECOG score: 0, 1",
                   "Hastegrad: Elektiv", "Diagnose: Malign")
  }


  if (ind_id == "relap_kolon") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 1, # Kolon
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    WHOECOG %in% 0:1,
                    Malign == 1,
                    Hastegrad_hybrid == 1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "relap_kolon",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = ReLapNarkose) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = NA
    maal = 12
    maalretn <- "lav"
    tittel <- "Andel relaparotomi/laparoskopi"
    utvalgTxt <- c("Operasjonsgruppe: Kolonreseksjoner", "WHO ECOG score: 0, 1",
                   "Hastegrad: Elektiv", "Diagnose: Malign")
  }

  if (ind_id == "konv_rate_kolon") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 1, # Kolon
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    WHOECOG %in% 0:1,
                    Malign == 1,
                    Hastegrad_hybrid == 1,
                    Tilgang %in% 2:3) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "konv_rate_kolon",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)],
                    var = case_when(
                      Tilgang == 2 ~ 0,
                      Tilgang == 3 ~ 1
                    )) %>%
      dplyr::rename(year = Aar) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 15
    maal = 10
    tittel <- "Andel laparoskopiske inngrep konvertert til åpen kirurgi"
    utvalgTxt <- c("Operasjonsgruppe: Kolonreseksjoner", "WHO ECOG score: 0, 1",
                   "Hastegrad: Elektiv", "Diagnose: Malign")
  }

  if (ind_id == "norgast_kikkhullsteknikk_tykktarm") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 1, # Kolon
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    WHOECOG %in% 0:1,
                    Malign == 1,
                    Hastegrad_hybrid == 1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_kikkhullsteknikk_tykktarm",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = LapTilgang2) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=F
    terskel=10
    minstekrav = 60
    maal = 70
    tittel <- "Andel laparoskopi (konverterte inngrep inkludert)"
    utvalgTxt <- c("Operasjonsgruppe: Kolonreseksjoner", "WHO ECOG score: 0, 1",
                   "Hastegrad: Elektiv", "Diagnose: Malign")
  }

  if (ind_id == "norgast_lekkasje_endetarm") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 2, # Rektum
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    WHOECOG %in% 0:1,
                    Malign == 1,
                    Hastegrad_hybrid == 1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_lekkasje_endetarm",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Anastomoselekkasje) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 7
    maal = 5
    tittel <- "Andel anastomoselekkasjer, ny anastomose"
    utvalgTxt <- c("Operasjonsgruppe: Rektumreseksjoner", "WHO ECOG score: 0, 1",
                   "Hastegrad: Elektiv", "Diagnose: Malign")
  }

  if (ind_id == "relap_rektum") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 2, #  rektum
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    WHOECOG %in% 0:1,
                    Malign == 1,
                    Hastegrad_hybrid == 1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "relap_rektum",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = ReLapNarkose) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = NA
    maal = 12
    maalretn <- "lav"
    tittel <- "Andel relaparotomi/laparoskopi"
    utvalgTxt <- c("Operasjonsgruppe: Rektumreseksjoner", "WHO ECOG score: 0, 1",
                   "Hastegrad: Elektiv", "Diagnose: Malign")
  }


  if (ind_id == "konv_rate_rektum") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 2, # Rektum
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    WHOECOG %in% 0:1,
                    Malign == 1,
                    Hastegrad_hybrid == 1,
                    Tilgang %in% 2:3) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "konv_rate_rektum",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)],
                    var = case_when(
                      Tilgang == 2 ~ 0,
                      Tilgang == 3 ~ 1
                    )) %>%
      dplyr::rename(year = Aar) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 15
    maal = 10
    tittel <- "Andel laparoskopiske inngrep konvertert til åpen kirurgi"
    utvalgTxt <- c("Operasjonsgruppe: Rektumreseksjoner", "WHO ECOG score: 0, 1",
                   "Hastegrad: Elektiv", "Diagnose: Malign")
  }

  if (ind_id == "norgast_kikkhullsteknikk_endetarm") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 2, # Rektum
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    WHOECOG %in% 0:1,
                    Malign == 1,
                    Hastegrad_hybrid == 1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_kikkhullsteknikk_endetarm",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = LapTilgang2) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=F
    terskel=10
    minstekrav = 60
    maal = 70
    tittel <- "Andel laparoskopi (konverterte inngrep inkludert)"
    utvalgTxt <- c("Operasjonsgruppe: Rektumreseksjoner", "WHO ECOG score: 0, 1",
                   "Hastegrad: Elektiv", "Diagnose: Malign")
  }


  if (ind_id == "norgast_avdoede_spiseroer") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 3, # spiserør
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    Hastegrad_hybrid==1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_avdoede_spiseroer",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Mort90) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 8
    maal = 5
    tittel <- "Andel avdøde innen 90 dager etter operasjon"
    utvalgTxt <- c("Operasjonsgruppe: Øsofagusreseksjoner", "Hastegrad: Elektiv")
  }

  if (ind_id == "anastomoselekk_osofagus") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 3, # spiserør
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    Hastegrad_hybrid==1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "anastomoselekk_osofagus",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Anastomoselekkasje) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = NA
    maal = 20
    maalretn <- "lav"
    tittel <- "Andel anastomoselekkasjer, ny anastomose"
    utvalgTxt <- c("Operasjonsgruppe: Øsofagusreseksjoner", "Hastegrad: Elektiv")
  }

  if (ind_id == "norgast_avdoede_magesekk") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 4, # mage
                    Hastegrad_hybrid==1,
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_avdoede_magesekk",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Mort90) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 8
    maal = 5
    tittel <- "Andel avdøde innen 90 dager etter operasjon"
    utvalgTxt <- c("Operasjonsgruppe: Ventrikkelreseksjoner", "Hastegrad: Elektiv")
  }


  if (ind_id == "anastomoselekk_ventrikkel") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 4, # mage
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    Hastegrad_hybrid==1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "anastomoselekk_ventrikkel",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Anastomoselekkasje) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 8
    maal = 5
    maalretn <- "lav"
    tittel <- "Andel anastomoselekkasjer, ny anastomose"
    utvalgTxt <- c("Operasjonsgruppe: Ventrikkelreseksjoner", "Hastegrad: Elektiv")
  }


  if (ind_id == "relap_ventrikkel") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 4, #  ventrikkel
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    Hastegrad_hybrid == 1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "relap_ventrikkel",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = ReLapNarkose) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = NA
    maal = 15
    maalretn <- "lav"
    tittel <- "Andel relaparotomi/laparoskopi"
    utvalgTxt <- c("Operasjonsgruppe: Ventrikkelreseksjoner",
                   "Hastegrad: Elektiv")
  }


  if (ind_id == "norgast_avdoede_bukspytt_tolv") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 6, # whipple
                    Hastegrad_hybrid==1,
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_avdoede_bukspytt_tolv",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Mort90) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 8
    maal = 5
    tittel <- "Andel avdøde innen 90 dager etter operasjon"
    utvalgTxt <- c("Operasjonsgruppe: Whipples operasjon", "Hastegrad: Elektiv")
  }


  if (ind_id == "CR_POPF_whipple") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 6, # whipple
                    Hastegrad_hybrid==1,
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "CR_POPF_whipple",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)],
                    var = ifelse((ReLapNarkose==1 & ViktigsteFunn %in% 1:2) |
                                   (ReLapNarkose==1 & (EndoInterBlod | EndoInterLekkasje)) |
                                   (PerkDrenasje==1 & HoyAmylaseKons==1), 1, 0)) %>%
      dplyr::rename(year = Aar) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 20
    maal = 15
    tittel <- "Klinisk relevant postoperativ pankreasfistel"
    utvalgTxt <- c("Operasjonsgruppe: Whipples operasjon", "Hastegrad: Elektiv")
  }

  if (ind_id == "relap_whipple") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 6, #  whipple
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    Hastegrad_hybrid == 1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "relap_whipple",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = ReLapNarkose) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = NA
    maal = 20
    maalretn <- "lav"
    tittel <- "Andel relaparotomi/laparoskopi"
    utvalgTxt <- c("Operasjonsgruppe: Whipples operasjon",
                   "Hastegrad: Elektiv")
  }

  if (ind_id == "CR_POPF_distal") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 7, # distale pankreas
                    Hastegrad_hybrid==1,
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "CR_POPF_whipple",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)],
                    var = ifelse((ReLapNarkose==1 & ViktigsteFunn %in% 1:2) |
                                   (ReLapNarkose==1 & (EndoInterBlod | EndoInterLekkasje)) |
                                   (PerkDrenasje==1 & HoyAmylaseKons==1), 1, 0)) %>%
      dplyr::rename(year = Aar) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 35
    maal = 25
    tittel <- "Klinisk relevant postoperativ pankreasfistel"
    utvalgTxt <- c("Operasjonsgruppe: Distale pankreasreseksjoner", "Hastegrad: Elektiv")
  }


  if (ind_id == "kikkhullsteknikk_distal") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 7, # Distale pankreas
                    Hastegrad_hybrid == 1,
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "kikkhullsteknikk_distal",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = LapTilgang2) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=F
    terskel=10
    minstekrav = NA
    maal = 35
    tittel <- "Andel laparoskopi (konverterte inngrep inkludert)"
    utvalgTxt <- c("Operasjonsgruppe: Distale pankreasreseksjoner",
                   "Hastegrad: Elektiv")
  }

  if (ind_id == "norgast_avdoede_lever") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 5, # lever
                    Hastegrad_hybrid==1,
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_avdoede_lever",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Mort90) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = 5
    maal = 3
    tittel <- "Andel avdøde innen 90 dager etter operasjon"
    utvalgTxt <- "Operasjonsgruppe: Leverreseksjoner"
  }

  if (ind_id == "relap_lever") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 5, #  lever
                    OppfStatus == 1 | is.na(OppfStatus), # Kun ferdige
                    Hastegrad_hybrid == 1) %>%
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "relap_lever",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = ReLapNarkose) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=T
    terskel=10
    minstekrav = NA
    maal = 15
    maalretn <- "lav"
    tittel <- "Andel relaparotomi/laparoskopi"
    utvalgTxt <- c("Operasjonsgruppe: Leverreseksjoner",
                   "Hastegrad: Elektiv")
  }

  if (ind_id == "norgast_kikkhullsteknikk_lever") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 5, # Lever
                    Hastegrad_hybrid == 1,
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_kikkhullsteknikk_lever",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = LapTilgang2) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)
    decreasing=F
    terskel=10
    minstekrav = NA
    maal = 30
    tittel <- "Andel laparoskopi (konverterte inngrep inkludert)"
    utvalgTxt <- c("Operasjonsgruppe: Leverreseksjoner",
                   "Hastegrad: Elektiv")
  }


  Indikator$Sykehus <- map_resh_orgnr$Sykehus[match(Indikator$orgnr, map_resh_orgnr$orgnr_sh)]
  tabell1 <- Indikator %>%
    dplyr::filter(year %in% (max(year)-2):max(year)) %>%
    dplyr::summarise(antall = sum(var),
                     N = dplyr::n(),
                     .by = c(Sykehus)) %>%
    janitor::adorn_totals(name = "Norge") %>%
    dplyr::mutate(andel=antall/N*100)
  tabell2 <- Indikator %>%
    dplyr::filter(year %in% max(year)) %>%
    dplyr::summarise(antall = sum(var),
                     N = dplyr::n(),
                     .by = c(Sykehus)) %>%
    janitor::adorn_totals(name = "Norge") %>%
    dplyr::mutate(andel=antall/N*100)

  tabell <- merge(tabell2, tabell1, by = "Sykehus", suffixes = c("", "_saml"),
                  all.y = T)
  tabell[is.na(tabell)] <- 0
  rownames(tabell) <- tabell$Sykehus
  AntTilfeller <- tabell[ , c("antall", "antall_saml")]
  names(AntTilfeller) <- c(max(Indikator$year), paste0(max(Indikator$year)-2, "-", max(Indikator$year)) )
  N <- tabell[ , c("N", "N_saml")]
  names(N) <- names(AntTilfeller)
  andeler <- tabell[ , c("andel", "andel_saml")]
  names(andeler) <- names(AntTilfeller)

  utdata <- list(Indikator=Indikator, tittel=tittel, utvalgTxt=utvalgTxt,
                 minstekrav=minstekrav, maal=maal, decreasing=decreasing,
                 terskel=terskel, tabell=tabell, AntTilfeller=AntTilfeller,
                 N=N, andeler=andeler, maalretn=maalretn)
  return(invisible(utdata))
}



#' Plot NORGAST sine indikatorer
#'
#' Denne funksjonen plotter NORGAST sine offisielle kvalitetsindikatorer
#' basert på beregningene i norgastBeregnIndikator
#'
#' @param Indikator indikatordata som beregnet i norgastBeregnIndikator
#' @param tittel Tittel på plot
#'
#' @export
norgastPlotIndikator <- function(AntTilfeller, N, andeler, tittel="",
                                 decreasing=FALSE, terskel=10, lavDG='',
                                 lavDGtekst='Dekningsgrad < 60 %',
                                 width=600, height=700, outfile="",
                                 graaUt=NA, skriftStr=1.2, utvalgTxt="",
                                 minstekrav = NA, maal = NA, pktStr=1.4,
                                 legPlass='top', minstekravTxt='Akseptabelt',
                                 maalTxt='Mål', maalretn='hoy', prikktall=TRUE,
                                 pst_kolonne = T) {

  tittel <- c(tittel, 'inkl. 95% konf. int.')

  if (!decreasing) {
    andeler[N < terskel] <- -1
  } else {
    andeler[N < terskel] <- max(andeler[, dim(andeler)[2]])+1
  }
  andeler[rownames(andeler) %in% lavDG, ] <- NA
  rekkefolge <- order(andeler[[dim(andeler)[2]]], decreasing = decreasing, na.last = F)

  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]
  andeler[N < terskel] <- NA
  andeler[N[, dim(andeler)[2]]<terskel, 1:2] <- NA
  KI <- binomkonf(AntTilfeller[rekkefolge, dim(andeler)[2]], N[, dim(andeler)[2]])*100
  KI[, is.na(andeler[, dim(andeler)[2]])] <- NA
  pst_txt <- paste0(sprintf('%.0f', andeler[, dim(andeler)[2]]), ' %')
  pst_txt[N[, dim(andeler)[2]]<terskel] <- paste0('N<', terskel)
  pst_txt[rownames(andeler) %in% lavDG] <- lavDGtekst
  pst_txt <- c(NA, pst_txt, NA, NA)
  pst_txt_prikk <- paste0(sprintf('%.0f', andeler[, 1]), ' %')
  pst_txt_prikk[N[, 1]<terskel] <- NA
  pst_txt_prikk[rownames(andeler) %in% lavDG] <- NA
  pst_txt_prikk <- c(NA, pst_txt_prikk, NA, NA)

  FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height,
                                  pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], length(andeler[,dim(andeler)[2]]))
  soyleFarger[which(rownames(andeler)=='Norge')] <- farger[4]
  if (!is.na(graaUt[1])) {soyleFarger[which(rownames(andeler) %in% graaUt)] <- 'gray88'}
  soyleFarger <- c(NA, soyleFarger)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr
  rownames(andeler) <- paste0(rownames(andeler), ' (', N[, dim(N)[2]], ')')
  andeler <- rbind(andeler, c(NA,NA))
  rownames(andeler)[dim(andeler)[1]] <- paste0('(N, ', names(andeler)[dim(andeler)[2]], ')')
  KI <- cbind(c(NA, NA), KI, c(NA, NA))

  vmarg <- max(0, strwidth(rownames(andeler), units='figure', cex=cexgr)*0.75)
  par('fig'=c(vmarg, 1, 0, 1))
  # par('mar'=c(5.1, 4.1, 5.1, 9.1))
  par('oma'=c(0,1,length(utvalgTxt),0))

  par('mar'=c(5.1, 4.1, 5.1, 2.1))
  if (pst_kolonne) {par('mar'=c(5.1, 4.1, 5.1, 9.1)) }

  xmax <- min(max(KI, max(andeler, na.rm = T), na.rm = T)*1.15,100)
  andeler <- rbind(c(NA,NA), andeler, c(NA,NA))
  rownames(andeler)[dim(andeler)[1]] <- '  '
  rownames(andeler)[1] <- ' '

  ypos <- barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Andel (%)') # '#96BBE7'

  fargerMaalNiva <-  c('aquamarine3','#fbf850', 'red')

  if (maal > minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=minstekrav, ybottom=1, xright=maal, ytop=max(ypos)-1.6,
         col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6,
         col = fargerMaalNiva[1], border = NA)}
  if (maal < minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=maal, ybottom=1, xright=minstekrav, ytop=max(ypos)-1.6,
         col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6,
         col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='lav') {
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6,
         col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='hoy') {
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6,
         col = fargerMaalNiva[1], border = NA)}

  barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
           names.arg=rep('',dim(andeler)[1]),
           horiz=T, axes=F, space=c(0,0.3),
           col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)

  # title(main = tittel, outer=T)
  title(main = tittel, xpd=TRUE)
  ypos <- as.numeric(ypos) #as.vector(ypos)
  yposOver <- max(ypos)-2 + 0.5*diff(ypos)[1]
  if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, yposOver), col=fargerMaalNiva[2], lwd=2)
    par(xpd=TRUE)
    text(x=minstekrav, y=yposOver, labels = minstekravTxt,
         pos = 4, cex=cexgr*0.65, srt = 90)
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, yposOver), col=fargerMaalNiva[1], lwd=2)
    barplot( t(andeler[, dim(andeler)[2]]), beside=T, las=1,
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=yposOver, labels = maalTxt, pos = 4, cex=cexgr*0.65, srt = 90) #paste0(maalTxt,maal,'%')
    par(xpd=FALSE)
  }
  arrows(x0 = KI[1,], y0 = ypos, x1 = KI[2,], y1 = ypos,
         length=0.5/max(ypos), code=3, angle=90, lwd=1.8, col='gray') #, col=farger[1])
  legend('bottom', cex=0.9*cexgr, bty='n',
         lwd=1.8, lty = 1, pt.cex=1.8, col='gray',
         legend=paste0('Konfidensintervall ', names(N)[dim(N)[2]]))

  axis(1,cex.axis=0.9)
  grtxt_bold <- rownames(andeler)
  grtxt_bold[which(substr(grtxt_bold, 1, 5) =='Norge')] <-
    paste0("$\\textbf{", grtxt_bold[which(substr(grtxt_bold, 1, 5) =='Norge')], "}")
  mtext(latex2exp::TeX(grtxt_bold), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  antAar <- dim(andeler)[2]

  par(xpd=TRUE)
  points(y=ypos, x=andeler[,1],cex=pktStr, pch= 19)
  par(xpd=FALSE)

  if (legPlass=='nede'){
    legend('bottomright', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',
           lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
           legend=names(N), ncol = 1)}
  if (legPlass=='top'){
    legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
           lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
           legend=names(N), ncol = dim(andeler)[2])}
  if (legPlass=='topleft'){
    legend('topleft', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
           lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
           legend=names(N), ncol = dim(andeler)[2])}
  if (legPlass=='topright'){
    legend('topright', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
           lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
           legend=names(N), ncol = dim(andeler)[2])}

  if (prikktall) {
    text(x=0, y=ypos, labels = pst_txt, cex=0.75, pos=4)#
    text(x=andeler[,1], y=ypos, labels = pst_txt_prikk, cex=0.75, pos=4, xpd = T)
    }

  if (pst_kolonne) {
    mtext( pst_txt_prikk, side=4, line=3.5, las=1, at=ypos, col=1, cex=cexgr*0.75, adj = 1)
    mtext( pst_txt, side=4, line=7.5, las=1, at=ypos, col=1, cex=cexgr*0.75, adj = 1)
    mtext( names(N)[1], side=4, line=3.5, las=1, at=max(ypos), col=1, cex=cexgr*0.75, adj = 1, font = 2)
    mtext( names(N)[2], side=4, line=7.5, las=1, at=max(ypos), col=1, cex=cexgr*0.75, adj = 1, font = 2)
  }

  #Tekst som angir hvilket utvalg som er gjort
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=(length(utvalgTxt)-1):0, outer=TRUE)

  # par('mar'= oldpar_mar)
  # par('fig'= oldpar_fig)
  # par('oma'= oldpar_oma)

  if ( outfile != '') {dev.off()}

}


