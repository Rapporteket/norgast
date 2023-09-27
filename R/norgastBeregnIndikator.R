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
  }

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
  }


  if (ind_id == "norgast_avdoede_spiseroer") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 3, # spiserør
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
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
  }

  if (ind_id == "norgast_avdoede_magesekk") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 4, # mage
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_avdoede_magesekk",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Mort90) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)

  }

  if (ind_id == "norgast_avdoede_bukspytt_tolv") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 6, # whipple
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_avdoede_bukspytt_tolv",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Mort90) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)

  }

  if (ind_id == "norgast_avdoede_lever") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 5, # lever
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_avdoede_lever",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = Mort90) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)

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

  }

  if (ind_id == "norgast_kikkhullsteknikk_lever") {
    Indikator <- RegData %>%
      dplyr::filter(Op_gr %in% 5, # Lever
                    OppfStatus == 1 | is.na(OppfStatus)) %>% # Kun ferdige
      dplyr::mutate(context = "caregiver",
                    denominator = 1,
                    ind_id = "norgast_kikkhullsteknikk_lever",
                    orgnr = map_resh_orgnr$orgnr_sh[match(AvdRESH, map_resh_orgnr$resh)]) %>%
      dplyr::rename(year = Aar,
                    var = LapTilgang2) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)

  }

  Indikator$Sykehus <- map_resh_orgnr$Sykehus[match(Indikator$orgnr, map_resh_orgnr$orgnr_sh)]
  return(invisible(Indikator))
}


norgastPlotIndikator <- function(Indikator, tittel="") {

  tabell <- Indikator %>%
    dplyr::filter(year %in% (max(year)-2):max(year)) %>%
    dplyr::summarise(antall = sum(var),
                     N = dplyr::n(),
                     .by = c(Sykehus, year))


}


