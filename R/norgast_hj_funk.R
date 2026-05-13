#' R-shiny alternativ til dateInput med mulighet til  bare år, måned og år
#'
#' @param inputId Definerer Namespace
#' @param label Visningstekst til brukerkontrollen
#' @param minview Fineste nivå tidsoppløsning
#' @param maxview Groveste nivå tidsoppløsning
#'
#' @return En brukerkontroll for bruk i R-shiny som tillater valg av f.eks. kun år og måned
#'        uten å angi dag.
#'
#' @export
dateInput2 <- function(inputId, label, minview = "months", maxview = "years", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}

#' Skriv til csv i nordisk format med Latin1 som default tegnsetting
#'
#' @export
write.csv3 <- function(x, file = "", tegnsetting = 'latin1', ...) {
  write.csv2(x, file = file, fileEncoding = tegnsetting, ...)
}

#' Generer kvartalsrapport og returner filnavn og sti til fil.
#'
#' @export
abonnement_kvartal_norgast <- function(baseName, reshID=0, valgtShus='', brukernavn='Pjotr') {

  # rapbase::autLogger(user = brukernavn, registryName = 'NORGAST',
  #                   reshId = reshID[[1]], msg = "Abonnement: kvartalsrapport")

  src <- system.file(paste0(baseName, '.Rnw'), package="norgast")
  tmpFile <- tempfile(paste0(baseName, Sys.Date()), fileext = '.Rnw')

  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  file.copy(src, tmpFile, overwrite = TRUE)

  # texfil <- knitr::knit(tmpFile, encoding = 'UTF-8')
  # tools::texi2pdf(texfil, clean = TRUE)


  pdfFile <- knitr::knit2pdf(tmpFile)
  utfil <- paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')

  file.copy(pdfFile, utfil)

  # rapbase::subLogger(author = brukernavn, registryName = 'NORGAST',
  #                   reshId = reshID[[1]], msg = paste("Sendt: ", utfil))

  return(utfil)
}

#' Hvis NULL erstatt med valgt verdi, default ''
#'
#' @export
fiksNULL <- function(x, erstatt='') {
  if (!is.null(x)) {x} else {erstatt}
}


#' Identifiserer pasienter med flere enn ett forløp med samme operasjonsdato
#'
#' @param RegData Datasettet som kan inneholde dobbeltregistreringer
#'
#' @return En dataramme med utvalgte variabler for potensielt dobbeltregistrerte forløp
#'
#' @export
dobbelreg <- function(RegData, skjemaoversikt, usrRole = "LU", reshID) {
  flere_sammedato <- RegData %>%
    dplyr::group_by(PasientID, HovedDato) %>%
    dplyr::summarise(Op_pr_dag = dplyr::n())
  flere_sammedato <- flere_sammedato[flere_sammedato$Op_pr_dag > 1, ]

  flere_sammedato <- merge(flere_sammedato, RegData,
                           by = c('PasientID', 'HovedDato'), all.x = T)
  flere_sammedato <- flere_sammedato[ , c("PasientID", "ForlopsID", "OperasjonsDato",
                                          "AvdRESH", "Sykehusnavn","Hovedoperasjon",
                                          "Operasjonsgrupper", "Hoveddiagnose",
                                          "OppfStatus", "ForstLukketAv")]
  skjemaoversikt <- skjemaoversikt %>%
    dplyr::summarise(OpprettetAv = paste(unique(OpprettetAv), collapse = ", "),
                     SistLagretAv = paste(unique(SistLagretAv), collapse = ", "),
                     .by = ForlopsID) %>%
    dplyr::filter(ForlopsID %in% flere_sammedato$ForlopsID)
  flere_sammedato <- merge(flere_sammedato, skjemaoversikt, by = "ForlopsID")
  flere_sammedato$OperasjonsDato <- format(flere_sammedato$OperasjonsDato, format="%Y-%m-%d")
  flere_sammedato$PasientID <- as.numeric(flere_sammedato$PasientID)
  flere_sammedato$ForlopsID <- as.numeric(flere_sammedato$ForlopsID)
  flere_sammedato$AvdRESH <- as.numeric(flere_sammedato$AvdRESH)
  flere_sammedato <- flere_sammedato[order(flere_sammedato$OperasjonsDato,
                                           flere_sammedato$PasientID, decreasing = T), ]
  if (usrRole != 'SC') {
    flere_sammedato <- flere_sammedato[flere_sammedato$AvdRESH == reshID, ]
  }
  # flere_sammedato <- flere_sammedato %>%
  #   dplyr::select(-c(OpprettetAv, SistLagretAv)) # Foreløpig variant i påvente av at brukernavn fikses
  return(flere_sammedato)
}


#' Make staging data for norgast
#'
#' This function makes queries and pre-processing of registry data before
#' storing relevant staging data. Running this function may take a while so use
#' with care!
#'
#' @return Character vector of staging files, invisibly
#' @export
norgastMakeStagingData <- function() {

  RegData <-  norgast::NorgastHentRegData()
  skjemaoversikt <- norgast::NorgastHentskjemaoversikt()
  skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
  RegData <- norgast::NorgastPreprosess(RegData, behold_kladd = TRUE)
  skjemaoversikt <- merge(skjemaoversikt, RegData[,c("ForlopsID", "Op_gr", "Hovedoperasjon")], by = "ForlopsID", all.x = T)
  RegData <- RegData[which(RegData$RegistreringStatus==1),]
  RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
  rapbase::saveStagingData("norgast", "RegData", RegData)
  rapbase::saveStagingData("norgast", "skjemaoversikt", skjemaoversikt)

  invisible(rapbase::listStagingData("norgast"))
}


#' Transponer output fra tidyr::summarize
#'
#' Denne funksjonen tar som input resultatet av tidyr::summarize og returnerer dens
#' transponerte uten at formatene endres.
#'
#' Her kan detaljer skrives
#'
#' @return tr_frame Den transponerte av inputen
#'
#' @export
#'
tr_summarize_output <- function(x, kolnavn1 = ""){

  rekkefolge <- names(x)[-1]
  y <- x %>% tidyr::gather(names(x)[-1], key=nokkel, value = verdi) %>%
    tidyr::spread(key=names(x)[1], value = verdi)
  y <- y[match(rekkefolge, y$nokkel), ]
  names(y)[1] <- kolnavn1

  return(y)
}

#' Bryt tekst til ønsket lengde
#'
#' @export
#'
wrap.it <- function(x, len)
{
  sapply(x, function(y) paste(strwrap(y, len),
                              collapse = "\n"),
         USE.NAMES = FALSE)
}

#' Calculate age
#'
#' By default, calculates the typical "age in years", with a
#' floor applied so that you are, e.g., 5 years old from
#' 5th birthday through the day before your 6th birthday. Set
#' \code{floor = FALSE} to return decimal ages, and change units
#' for units other than years.
#' @param dob date-of-birth, the day to start calculating age.
#' @param age.day the date on which age is to be calculated.
#' @param units unit to measure age in. Defaults to "years". Passed to duration.
#' @param floor boolean for whether or not to floor the result. Defaults to TRUE.
#' @return Age in units. Will be an integer if floor = TRUE.
#' @examples
#' my.dob <- as.Date('1983-10-20')
#' age(my.dob)
#' age(my.dob, units = "minutes")
#' age(my.dob, floor = FALSE)
#'
#' @export
#'
age <- function(dob, age.day = lubridate::today(), units = "years", floor = TRUE) {

  calc.age = lubridate::interval(dob, age.day) /
    lubridate::duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

#' Lag tabell over komplikasjoner til bruk i årsrapport
#'
#' @export
#'
lag_kompl_tabell <- function(regdata) {
  regdata |> summarise(
    N = n(),
    reop_rate = sum(ReLapNarkose),
    anastomoselekk = sum(ViktigsteFunn==1, na.rm = T),
    dyp_infek = sum(ViktigsteFunn==2, na.rm = T),
    bloedning = sum(ViktigsteFunn==3, na.rm = T),
    saarrupt = sum(ViktigsteFunn==4, na.rm = T),
    annet = sum(ViktigsteFunn==5, na.rm = T),
    ingen = sum(ViktigsteFunn==6, na.rm = T),
    .by = c(Tilgang, Robot)) |>
    janitor::adorn_totals() |>
    dplyr::group_by(Robot) |>
    dplyr::group_modify(
      ~ .x |> janitor::adorn_totals(name = "samlet")) |>
    ungroup() |>
    filter( !(Robot == "-" & Tilgang == "samlet"),
            !(is.na(Robot) & Tilgang == "samlet")) |>
    mutate(Tilgang_ny = case_when(
      Tilgang == 1 ~ "\\textbf{Åpen}",
      Tilgang == "Total" ~ "\\textbf{Totalt}",
      Robot == "Ikke-robot" & Tilgang == "samlet" ~
        "\\quad Ikke-robot",
      Robot == "Ikke-robot" & Tilgang == 2 ~
        "\\quad \\quad \\textit{Fullført laparoskopisk}",
      Robot == "Ikke-robot" & Tilgang == 3 ~
        "\\quad \\quad \\textit{Konvertert}",
      Robot == "Robot" & Tilgang == "samlet" ~
        "\\quad Robotassistert",
      Robot == "Robot" & Tilgang == 2 ~
        "\\quad \\quad \\textit{Fullført laparoskopisk}",
      Robot == "Robot" & Tilgang == 3 ~
        "\\quad \\quad \\textit{Konvertert}")
    ) |>
    relocate(Tilgang_ny) %>%
    bind_rows(
      . |>
        filter(Tilgang_ny %in% c(
          "\\quad Ikke-robot",
          "\\quad Robotassistert"
        ))
      |>
        summarise(
          across(where(is.numeric), sum, na.rm = TRUE),
          Tilgang_ny = "\\textbf{Laparoskopisk (ITT)}"
        )
    ) |>
    # mutate(reop_rate = reop_rate/N*100) |>
    mutate(
      .ord = case_when(
        Tilgang_ny == "\\textbf{Åpen}" ~ 1,
        Tilgang_ny == "\\textbf{Laparoskopisk (ITT)}" ~ 2,
        Tilgang_ny == "\\quad Robotassistert" ~ 3,
        Tilgang_ny ==
          "\\quad \\quad \\textit{Fullført laparoskopisk}" &
          Robot == "Robot" ~ 4,
        Tilgang_ny == "\\quad \\quad \\textit{Konvertert}" &
          Robot == "Robot" ~ 5,
        Tilgang_ny == "\\quad Ikke-robot" ~ 6,
        Tilgang_ny ==
          "\\quad \\quad \\textit{Fullført laparoskopisk}" &
          Robot == "Ikke-robot" ~ 7,
        Tilgang_ny == "\\quad \\quad \\textit{Konvertert}" &
          Robot == "Ikke-robot" ~ 8,
        Tilgang_ny == "\\textbf{Totalt}" ~ 9,
        TRUE ~ 99
      )
    ) |>
    arrange(.ord) |>
    select(-c(.ord, Robot, Tilgang)) |>
    mutate(
      across(
        where(is.numeric) & !all_of("N"),
        ~ .x / N * 100
      )
    ) |>
    mutate(N = ifelse(
      Tilgang_ny %in% c(
        "\\quad \\quad \\textit{Fullført laparoskopisk}",
        "\\quad \\quad \\textit{Konvertert}"),
      paste0("\\textit{", N, "}"), N
    ))
}

