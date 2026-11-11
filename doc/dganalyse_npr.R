
## Analysegrunnlang NPR: dga_npr

## Analysegrunnlag norgast: dga_norgast


# Dekningsgradsanalyser per organgruppe, landet, sykehus, virksomhet,
# diagnosegruppe

dga_standard <- function(
    a,
    mappe1="Tabeller/Landet/",
    mappe2="Tabeller/Sykehus/",
    mappe3="Tabeller/Virksomhet/",
    mappe4="Tabeller/Diagnose/",
    tabellnavn
) {

  dga_norgast1 <- dga_norgast %>% filter(hierarki %in% a)
  dga_npr1 <- dga_npr %>% filter(hierarki %in% a)

  begge_innen_7dg <-
    inner_join(dga_norgast1, dga_npr1,
               by =c("NPRid", "hf_standard"),
               relationship = "many-to-many") %>%
    mutate(diff = abs(difftime(dato.x,dato.y, units = 'days'))) %>%
    filter(diff < 8) %>%
    arrange(som_k, diff) %>%
    group_by(som_k) %>%
    slice(1) %>%
    ungroup %>%
    arrange(id, diff) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup %>%
    mutate(resultat='Begge')

  begge_innen_7dg %>% count(diff)

  ## Landet

  # Kun norgast

  kun_norgast <- anti_join(dga_norgast1, begge_innen_7dg,
                           by = c("id")) %>%
    mutate(resultat='Kun_norgast') %>%
    count(resultat)

  # Kun NPR
  kun_npr <- anti_join(dga_npr1,begge_innen_7dg,
                       by = c("som_k")) %>%
    mutate(resultat='Kun_NPR') %>%
    count(resultat)

  # Begge
  begge <- begge_innen_7dg %>%
    count(resultat)

  resultat_landet_norgast <-
    bind_rows(begge,kun_norgast,kun_npr) %>%
    pivot_wider(names_from = resultat,
                values_from = n, values_fill = 0) %>%
    mutate(Total=Begge + Kun_norgast + Kun_NPR) %>%
    mutate(DG_norgast = ((Begge + Kun_norgast) / Total)*100,
           DG_norgast=round(DG_norgast,1)) %>%
    mutate(DG_NPR = ((Begge + Kun_NPR) / Total)*100,
           DG_NPR=round(DG_NPR,1))

  write.csv2(resultat_landet_norgast,
             paste0(mappe1,tabellnavn,"_landet.csv"),
             row.names = FALSE, fileEncoding = 'WINDOWS-1252')


  ## Virksomhet

  # Kun norgast
  kun_norgast_hf <- anti_join(dga_norgast1,begge_innen_7dg,
                              by = c("id")) %>%
    mutate(resultat='Kun_norgast') %>%
    count(hf_nr, hf_standard, resultat)

  # Kun NPR
  kun_npr_hf <- anti_join(dga_npr1,begge_innen_7dg,
                          by = c("som_k")) %>%
    mutate(resultat='Kun_NPR') %>%
    count(hf_nr, hf_standard, resultat)

  # Begge
  begge_hf <- begge_innen_7dg %>%
    rename(hf_nr = hf_nr.x) %>%
    count(hf_nr, hf_standard, resultat)

  resultat_norgast_hf <-
    bind_rows(begge_hf,kun_norgast_hf,kun_npr_hf) %>%
    pivot_wider(names_from = resultat, values_from = n,
                values_fill = 0) %>%
    mutate(Total=Begge + Kun_norgast + Kun_NPR) %>%
    mutate(DG_norgast = ((Begge + Kun_norgast) / Total)*100,
           DG_norgast=round(DG_norgast,1)) %>%
    mutate(DG_NPR = ((Begge + Kun_NPR) / Total)*100,
           DG_NPR=round(DG_NPR,1)) %>%
    bind_rows(resultat_landet_norgast) %>%
    mutate(hf_standard = case_when(
      is.na(hf_standard) ~'Total',
      TRUE ~ hf_standard)) %>%
    arrange(hf_nr) %>%
    select(-hf_nr)

  write.csv2(resultat_norgast_hf,
             paste0(mappe3,tabellnavn,"_hf.csv"),
             row.names = FALSE, fileEncoding = 'WINDOWS-1252')


  ## Sykehus

  # Kun norgast
  kun_norgast_sh <- anti_join(dga_norgast1,begge_innen_7dg,
                              by = c("id")) %>%
    mutate(resultat='Kun_norgast') %>%
    count(hf_nr, sh_standard, AvdRESH, resultat)

  # Kun NPR
  kun_npr_sh <- anti_join(dga_npr1,begge_innen_7dg,
                          by = c("som_k")) %>%
    mutate(resultat='Kun_NPR') %>%
    count(hf_nr, sh_standard, AvdRESH, resultat)

  # Begge
  begge_sh <- begge_innen_7dg %>%
    rename(hf_nr = hf_nr.x) %>% #
    rename(sh_standard = sh_standard.x, AvdRESH = AvdRESH.x) %>%
    count(hf_nr, sh_standard, AvdRESH, resultat)

  resultat_norgast_sh <-
    bind_rows(begge_sh,kun_norgast_sh,kun_npr_sh) %>%
    pivot_wider(names_from = resultat, values_from = n,
                values_fill = 0) %>%
    mutate(Total=Begge + Kun_norgast + Kun_NPR) %>%
    mutate(DG_norgast = ((Begge + Kun_norgast) / Total)*100,
           DG_norgast=round(DG_norgast,1)) %>%
    mutate(DG_NPR = ((Begge + Kun_NPR) / Total)*100,
           DG_NPR=round(DG_NPR,1)) %>%
    bind_rows(resultat_landet_norgast) %>%
    mutate(sh_standard = case_when(is.na(sh_standard) ~'Total',
                                   TRUE ~ sh_standard)) %>%
    arrange(hf_nr, sh_standard) %>%
    select(-hf_nr)

  write.csv2(resultat_norgast_sh, paste0(mappe2,tabellnavn,"_sh.csv"), row.names = FALSE, fileEncoding = 'WINDOWS-1252')

  # Kun norgast
  kun_norgast_diag <- anti_join(dga_norgast1,begge_innen_7dg,
                                by = c("id")) %>%
    mutate(resultat='Kun_norgast') %>%
    count(Malign_benign, resultat)

  # Kun NPR
  kun_npr_diag <- anti_join(dga_npr1,begge_innen_7dg,
                            by = c("som_k")) %>%
    mutate(resultat='Kun_NPR') %>%
    count(Malign_benign, resultat)

  # Begge
  begge_diag <- begge_innen_7dg %>%
    rename(Malign_benign = Malign_benign.y) %>%
    count(Malign_benign, resultat)

  resultat_norgast_diag <-
    bind_rows(begge_diag,kun_norgast_diag,kun_npr_diag) %>%
    pivot_wider(names_from = resultat, values_from = n,
                values_fill = 0) %>%
    mutate(Total=Begge + Kun_norgast + Kun_NPR) %>%
    mutate(DG_norgast = ((Begge + Kun_norgast) / Total)*100,
           DG_norgast=round(DG_norgast,1)) %>%
    mutate(DG_NPR = ((Begge + Kun_NPR) / Total)*100,
           DG_NPR=round(DG_NPR,1)) %>%
    bind_rows(resultat_landet_norgast) %>%
    mutate(Malign_benign = case_when(is.na(Malign_benign) ~'Total',
                                     TRUE ~ Malign_benign))

  write.csv2(resultat_norgast_diag,
             paste0(mappe4,tabellnavn,"_diag.csv"),
             row.names = FALSE, fileEncoding = 'WINDOWS-1252')

}

dga_standard('Lever',tabellnavn = 'Lever')
dga_standard('Whipple',tabellnavn = 'Whipple')
dga_standard('Rektum',tabellnavn = 'Rektum')
dga_standard('Ventrikkel',tabellnavn = 'Ventrikkel')
dga_standard('Distal_pankreas',tabellnavn = 'Distal_pankreas')
dga_standard('Andre_pankreas',tabellnavn = 'Andre_pankreas')
dga_standard('Kolon',tabellnavn = 'Kolon')
dga_standard('Øsofagus',tabellnavn = 'Øsofagus')
dga_standard('Stomi',tabellnavn = 'Stomi')
dga_standard(c('Øsofagus','Lever','Whipple','Rektum','Ventrikkel',
               'Distal_pankreas','Andre_pankreas','Kolon'),
             tabellnavn = 'Alle')
