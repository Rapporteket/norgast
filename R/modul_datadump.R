#' UI-modul for datadump-fane i NoRGast sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til datadump-fane
#'
#' @export
datadump_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(width = 3,
                 id = ns("id_dump_panel"),
                 shinyjs::hidden(uiOutput(outputId = ns('valgtevar_dump'))),
                 dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01', language = "nb",
                                max = Sys.Date(),
                                start  = lubridate::floor_date(lubridate::today() - lubridate::years(1), unit = "year"),
                                end = Sys.Date(), separator = " til "),
                 selectInput(inputId = ns("dumptype"), label = "Velg type datadump",
                             choices = c('AlleVar', 'AlleVarNum', 'ForlopsOversikt', 'SkjemaOversikt')),
                 shinyjs::hidden(selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
                             choices = BrValg$reseksjonsgrupper, multiple = TRUE)),
                 shinyjs::hidden(uiOutput(outputId = ns('ncsp'))),
                 selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                             choices = BrValg$sykehus, multiple = TRUE),
                 tags$hr(),
                 actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(
      tabsetPanel(id= ns("datadump"),
                  tabPanel("Rådata", value = "datadump_raa",
                           h2('Datadump med rådata', align='center'),
                           br(),
                           h4('Her kan du laste ned forskjellige varianter av datadump for NoRGast. LU-brukere kan kun laste ned data for egen avdeling.
                              Merk at også registreringer i kladd er inkludert i datadump.'),
                           br(),
                           h4(tags$b(tags$u('Forklaring til de ulike datadump-typene:'))),
                           h4(tags$b('AlleVar '), 'inneholder alle kliniske variabler i registeret og benytter etikettene til kategoriske variabler.'),
                           h4(tags$b('AlleVarNum '), 'inneholder alle kliniske variabler i registeret og benytter tallkodene til kategoriske variabler.'),
                           h4(tags$b('ForlopsOversikt '), 'inneholder en del administrative data relevant for forløpene.'),
                           h4(tags$b('SkjemaOversikt '), 'er en oversikt over status til alle registreringer i registreret, også uferdige.'),
                           DTOutput(ns("Tabell_datadum_raa")), downloadButton(ns("lastNed_dump_raa"), "Last ned datadump")
                  ),
                  tabPanel("Prosessert data", value = "datadump_pros",
                           h2('Datadump prosessert - NoRGast', align='center'),
                           br(),
                           h4('Her kan du laste ned datadump basert på prosessert og koblet data som brukes på Rapporteket. Du kan velge hvilke variabler
                              du vil inkludere, samt filtrere på operasjonstype i tillegg til dato. Kun ferdigstilte registreringer er inkludert.'),
                           DTOutput(ns("Tabell_adm2")), downloadButton(ns("lastNed_dump"), "Last ned datadump")
                  )
      )
    )
  )
}

#' Server-modul for datadump-fane i NoRGast sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til datadump-fane
#'
#' @export
datadump <- function(input, output, session, reshID, RegData, userRole, hvd_session){


  observe(
    if (input$datadump == "datadump_raa") {
      shinyjs::hide(id = 'valgtevar_dump')
      shinyjs::hide(id = 'op_gruppe')
      shinyjs::hide(id = 'ncsp')
      shinyjs::show(id = 'dumptype')
    } else if (input$datadump == "datadump_pros") {
      shinyjs::hide(id = 'dumptype')
      shinyjs::show(id = 'valgtevar_dump')
      shinyjs::show(id = 'op_gruppe')
      shinyjs::show(id = 'ncsp')
    }
  )

  observeEvent(input$reset_input, {
    shinyjs::reset("id_dump_panel")
  })

  observe(
    if (userRole != 'SC') {
      shinyjs::hide(id = 'valgtShus')
    })

  output$ncsp <- renderUI({
    ns <- session$ns
    if (!is.null(input$op_gruppe)) {
      selectInput(inputId = ns("ncsp_verdi"), label = "NCSP koder (velg en eller flere)",
                  choices = if (!is.null(input$op_gruppe)) {setNames(substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in%
                                                                                                                 as.numeric(input$op_gruppe)])), 1, 5),
                                                                     sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe)])))
                  }, multiple = TRUE)
    }
  })

  output$valgtevar_dump <- renderUI({
    ns <- session$ns
    if (!is.null(names(RegData))) {
      selectInput(inputId = ns("valgtevar_dump_verdi"), label = "Velg variabler å inkludere (ingen valgt er lik alle)",
                  choices = names(RegData), multiple = TRUE)
    }
  })



  output$lastNed_dump <- downloadHandler(
    filename = function(){
      paste0('Datadump_NoRGast', Sys.time(), '.csv')
    },
    content = function(file){
      dumpdata <- RegData[RegData$HovedDato >= input$datovalg[1] &
                            RegData$HovedDato <= input$datovalg[2], ]
      if (userRole != 'SC') {
        dumpdata <- dumpdata[dumpdata$AvdRESH == reshID, ]
      } else {
        if (!is.null(input$valgtShus)) {dumpdata <- dumpdata[dumpdata$AvdRESH %in% as.numeric(input$valgtShus), ]}
      }

      if (!is.null(input$op_gruppe)) {dumpdata <- dumpdata[which(dumpdata$Op_gr %in% as.numeric(input$op_gruppe)), ]}
      if (!is.null(input$ncsp_verdi)) {dumpdata <- dumpdata[which(substr(dumpdata$Hovedoperasjon, 1, 5) %in% input$ncsp_verdi), ]}
      if (!is.null(input$valgtevar_dump_verdi)) {dumpdata <- dumpdata[, input$valgtevar_dump_verdi]}

      write.csv3(dumpdata, file, row.names = F, na = '')
    }
  )

  output$lastNed_dump_raa <- downloadHandler(
    filename = function(){
      paste0(input$dumptype, '_NoRGast', Sys.time(), '.csv')
    },
    content = function(file){
      if (rapbase::isRapContext()) {
        query <- paste0("SELECT * FROM ", input$dumptype)
        tmpData <- rapbase::loadRegData("norgast", query, "mysql")

        # tmpData <- norgastHentTabell(input$dumptype)
      } else {
        tmpData <- read.table(paste0('I:/norgast/', input$dumptype, '2021-06-02 08-20-32.txt'), header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
      }
      if (input$dumptype %in% c('AlleVar', 'AlleVarNum')) {
        tmpData$HovedDato <- tmpData$OpDato
      }
      dumpdata <- tmpData[as.Date(tmpData$HovedDato) >= input$datovalg[1] &
                            as.Date(tmpData$HovedDato) <= input$datovalg[2], ]
      if (userRole != 'SC') {
        dumpdata <- dumpdata[dumpdata$AvdRESH == reshID, ]
      } else {
        if (!is.null(input$valgtShus)) {dumpdata <- dumpdata[dumpdata$AvdRESH %in% as.numeric(input$valgtShus), ]}
      }

      write.csv2(dumpdata, file, row.names = F, na = '', fileEncoding = 'Latin1')
    }
  )


  shiny::observe({
    if (rapbase::isRapContext()) {

      shinyjs::onclick(
        "lastNed_dump",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste0("NoRGast: nedlasting prosessert datadump")
        )
      )

      shinyjs::onclick(
        "lastNed_dump_raa",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste0("NoRGast: nedlasting ", input$dumptype)
        )
      )
    }
  })

}
