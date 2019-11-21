#' @inheritParams nraFigAndeler
#'
#' @return UI-delen av fordelingsfigur
#'
fordelingsfig_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      selectInput(inputId = ns("valgtVar"), label = "Velg variabel",
                  choices = BrValg$varvalg),
      dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01',
                     max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), language = "nb", separator = " til "),
      selectInput(inputId = ns("enhetsUtvalg"), label = "Kjør rapport for",
                  choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1, 'Egen avd.'=2)),
      selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                  choices = BrValg$sykehus, multiple = TRUE),
      selectInput(inputId = ns("tilgang"), label = "Tilgang i abdomen", choices = BrValg$tilgang_valg, multiple = TRUE),
      sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                  max = 120, value = c(0, 120)),
      selectInput(inputId = ns("erMann"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("elektiv"), label = "Tidspunkt for operasjon",
                  choices = c('Ikke valgt'=99, 'Innenfor normalarbeidstid'=1, 'Utenfor normalarbeidstid'=0)),
      selectInput(inputId = ns("hastegrad"), label = "Hastegrad",
                  choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=2)),
      shinyjs::hidden(
        div(
          id = ns("avansert"),
            selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
                        choices = BrValg$reseksjonsgrupper, multiple = TRUE),
            uiOutput(outputId = ns('ncsp')),
            selectInput(inputId = ns("BMI"), label = "BMI", choices = BrValg$bmi_valg, multiple = TRUE),
            sliderInput(inputId=ns("PRS"), label = "mE-PASS", min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
            selectInput(inputId = ns("ASA"), label = "ASA-grad", choices = BrValg$ASA_valg, multiple = TRUE),
            selectInput(inputId = ns("whoEcog"), label = "WHO ECOG score", choices = BrValg$whoEcog_valg, multiple = TRUE),
            selectInput(inputId = ns("forbehandling"), label = "Onkologisk forbehandling", multiple = TRUE,
                        choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
            selectInput(inputId = ns("malign"), label = "Diagnose", choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0))
            )
        ),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
      a(id = ns("toggleAdvanced"), "Skjul/vis flere valg", href = "#")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Figur",
                 plotOutput(ns("Figur1"), height="auto"), downloadButton(ns("lastNedBilde"), "Last ned figur")),
        tabPanel("Tabell",
                 uiOutput(ns("utvalg")),
                 # textOutput(ns("utvalg")),
                 br(),
                 tableOutput(ns("Tabell1")),
                 downloadButton(ns("lastNed"), "Last ned tabell"))
      )
    )
  )

}
