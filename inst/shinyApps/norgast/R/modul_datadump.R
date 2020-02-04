# Modul for datadump-fane i NoRGast sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @inheritParams norgastFigAndeler
#
# @return Modulfunksjoner til datadump-fane


datadump_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
      sidebarPanel(
        id = ns("id_dump_panel"),
        uiOutput(outputId = ns('valgtevar_dump')),
        dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01', language = "nb",
                       max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), separator = " til "),
        selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
                    choices = BrValg$reseksjonsgrupper, multiple = TRUE),
        uiOutput(outputId = ns('ncsp')),
        selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                    choices = BrValg$sykehus, multiple = TRUE),
        tags$hr(),
        actionButton(ns("reset_input"), "Nullstill valg")
      ),
    mainPanel(
      downloadButton(ns("lastNed_dump"), "Last ned datadump")
    )
  )
}


datadump <- function(input, output, session, reshID, RegData, userRole, hvd_session){

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
      if (!is.null(input$ncsp_verdi)) {dumpdata <- dumpdata[which(substr(dumpdata$Hovedoperasjon, 1, 5) %in% ncsp_verdi), ]}
      if (!is.null(input$valgtevar_dump_verdi)) {dumpdata <- dumpdata[, input$valgtevar_dump_verdi]}

      write.csv3(dumpdata, file, row.names = F, na = '')
    }
  )

  shiny::observe({
    if (rapbase::isRapContext()) {

      shinyjs::onclick(
        "lastNed_dump",
        raplog::repLogger(
          session = hvd_session,
          msg = paste0("NoRGast: nedlasting datadump")
        )
      )
    }
  })

}
