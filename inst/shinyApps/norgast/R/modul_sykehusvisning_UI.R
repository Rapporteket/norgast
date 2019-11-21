#' @inheritParams nraFigAndeler
#'
#' @return UI-delen av sykehusvisningsfigurer
#'
sykehusvisning_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      selectInput(inputId = ns("valgtVar"), label = "Velg variabel",
                  choices = BrValg$varvalg_andel),
      selectInput(inputId = ns("valgtVar_gjsn"), label = "Velg variabel",
                  choices = BrValg$varvalg_gjsn),
      selectInput(inputId = ns("valgtVar_andel_stabel"), label = "Velg variabel",
                  choices = BrValg$varvalg_andel_stabel),
      # shinyjs::hidden(selectInput(inputId = ns("valgtVar_gjsn"), label = "Velg variabel",
      #                             choices = BrValg$varvalg_gjsn)),
      # shinyjs::hidden(selectInput(inputId = ns("valgtVar_andel_stabel"), label = "Velg variabel",
      #                             choices = BrValg$varvalg_andel_stabel)),
      dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01',
                     max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), separator = " til "),
      selectInput(inputId = ns("enhetsUtvalg"), label = "Kjør rapport for",
                  choices = c('Egen avd. mot landet forøvrig'=1, 'Hele landet'=0, 'Egen avd.'=2)),
      # selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
      #             choices = BrValg$sykehus, multiple = TRUE),
      sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                  max = 120, value = c(0, 120)),
      selectInput(inputId = ns("erMann"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
                  choices = BrValg$reseksjonsgrupper, multiple = TRUE),
      uiOutput(outputId = ns('ncsp')),
      selectInput(inputId = ns("inkl_konf"), label = "Inkluder konfidensintervall",
                  choices = c(' '=99, 'Ja'=1, 'Nei'=0)),
      selectInput(inputId = ns("elektiv"), label = "Operasjonstid",
                  choices = c('Ikke valgt'=99, 'Innenfor normalarbeidstid'=1, 'Utenfor normalarbeidstid'=0)),
      selectInput(inputId = ns("hastegrad"), label = "Hastegrad",
                  choices = c('Ikke valgt'=99, 'Elektiv'=0, 'Akutt'=1)),
      selectInput(inputId = ns("BMI"), label = "BMI", choices = BrValg$bmi_valg, multiple = TRUE),
      selectInput(inputId = ns("tilgang"), label = "Tilgang i abdomen", choices = BrValg$tilgang_valg, multiple = TRUE),
      sliderInput(inputId = ns("PRS"), label = "mE-PASS", min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
      selectInput(inputId = ns("ASA"), label = "ASA-grad", choices = BrValg$ASA_valg, multiple = TRUE),
      selectInput(inputId = ns("whoEcog"), label = "WHO ECOG score", choices = BrValg$whoEcog_valg, multiple = TRUE),
      selectInput(inputId = ns("forbehandling"), label = "Onkologisk forbehandling", multiple = TRUE,
                  choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
      selectInput(inputId = ns("malign"), label = "Diagnose", choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
      # selectInput(inputId = ns("tidsenhet"), label = "Velg tidsenhet", choices = c('Aar', 'Mnd', 'Kvartal', 'Halvaar')),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
    ),
    mainPanel(tabsetPanel(id = ns("tabs_sykehusvisning"),
                          tabPanel("Figur, andeler",
                                   plotOutput(ns("fig_andel_grvar"), height="auto"),
                                   downloadButton(ns("lastNedBilde_sykehus_andel"), "Last ned figur")),
                          tabPanel("Tabell, andeler",
                                   uiOutput(ns("utvalg_sykehus_andel")),
                                   tableOutput(ns("Tabell_sykehus_andel")),
                                   downloadButton(ns("lastNed_sykehus_andel"), "Last ned tabell")),
                          tabPanel("Figur, gjennomsnitt",
                                   plotOutput(ns("fig_gjsn_grvar"), height="auto"),
                                   downloadButton(ns("lastNedBilde_sykehus_gjsn"), "Last ned figur")),
                          tabPanel("Tabell, gjennomsnitt",
                                   uiOutput(ns("utvalg_sykehus_gjsn")),
                                   tableOutput(ns("Tabell_sykehus_gjsn")),
                                   downloadButton(ns("lastNed_sykehus_gjsn"), "Last ned tabell")),
                          tabPanel("Figur, andeler i stabel",
                                   plotOutput(ns("fig_andel_grvar_stabel"), height="auto"),
                                   downloadButton(ns("lastNedBilde_sykehus_andel_stabel"), "Last ned figur")),
                          tabPanel("Tabell, andeler i stabel",
                                   uiOutput(ns("utvalg_sykehus_andel_stabel")),
                                   tableOutput(ns("Tabell_sykehus_andel_stabel")),
                                   downloadButton(ns("lastNedStabelTabell"), "Last ned tabell"))
    )
    )

  )
}


