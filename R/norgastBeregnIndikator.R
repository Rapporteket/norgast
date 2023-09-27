#' Beregn NORGAST sine indikatorer
#'
#' Denne funksjonen beregner NORGAST sine offisielle kvalitetsindikatorer
#'
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param ind_id Indikatornavn
#'
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



# "norgast2"] <- "norgast_avdoede_spiseroer"
# "norgast3"] <- "norgast_avdoede_magesekk"
# "norgast4"] <- "norgast_avdoede_bukspytt_tolv"
# "norgast5"] <- "norgast_avdoede_lever"
# "norgast6"] <- "norgast_lekkasje_tykktarm"
# "norgast7"] <- "norgast_lekkasje_endetarm"
# "norgast8"] <- "norgast_kikkhullsteknikk_lever"
# "norgast9"] <- "norgast_kikkhullsteknikk_tykktarm"
# "norgast10"] <- "norgast_kikkhullsteknikk_endetarm"



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


}





}
