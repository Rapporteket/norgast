# Modul for Samledokumenter-fane i NoRGast sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @inheritParams norgastFigAndeler
#
# @return Modulfunksjoner til Samledokumenter


samledok_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(width = 3,
      id = ns("id_samledok_panel"),
      conditionalPanel(condition = paste0("input['", ns('tabs'), "'] == 'samledok_egen' |
                                          input['", ns('tabs'), "'] == 'samledok_landet'"),
                       dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01', language = "nb",
                                      max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), separator = " til ")),
      conditionalPanel(condition = paste0("input['", ns('tabs'), "'] == 'kvartalsrapport'"),
                       # dateInput(inputId=ns("datovalg_kv"), label = "Dato til", min = '2015-01-01',
                       #                max = Sys.Date(), value  = Sys.Date(), language = "nb"),
                       # helpText("Velg en dato etter avslutningen av det siste",
                       #          "kvartalet som ønskes inkludert i rapporten"),
                       selectInput(inputId = ns("valgtAar"), label = "Frem til år",
                                   choices = if (Sys.Date() %>% as.character() %>% substr(6,7) %>% as.numeric() >= 4) {
                                     rev(2014:as.numeric(format(Sys.Date(), '%Y')))
                                   } else {
                                     rev(2014:(as.numeric(format(Sys.Date(), '%Y'))-1))
                                   }),
                       uiOutput(outputId = ns('kvartal'))),
      selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                  choices = BrValg$sykehus, multiple = TRUE),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(
      tabsetPanel(id= ns("tabs"),
                  tabPanel("Samledokument med egen avd. mot landet forøvrig", value = "samledok_egen",
                           downloadButton(ns("lastNed_saml"), "Last ned samledokument")),
                  tabPanel("Samledokument med nasjonale tall", value = "samledok_landet",
                           downloadButton(ns("lastNed_saml_land"), "Last ned samledokument")),
                  tabPanel("Kvartalsrapport for din avdeling", value = "kvartalsrapport",
                           downloadButton(ns("lastNed_kvartal"), "Last ned kvartalsrapport")))
    )
  )
}


samledok <- function(input, output, session, reshID, RegData, userRole, hvd_session){

  observeEvent(input$reset_input, {
    shinyjs::reset("id_samledok_panel")
  })

  observe(
    if (userRole != 'SC') {
      shinyjs::hide(id = 'valgtShus')
    })

  output$kvartal <- renderUI({
    ns <- session$ns
    if (!is.null(input$valgtAar)) {
      selectInput(inputId = ns("kvartal_verdi"), label = "Til og med (avsluttet) kvartal",
                  choices = if (input$valgtAar == format(Sys.Date(), '%Y')) {
                    ant_kvartal <- match(Sys.Date() %>% as.Date() %>% lubridate::floor_date(unit = 'quarter') %>% as.character() %>% substr(6,10),
                          c('04-01', '07-01', '10-01'))
                    # rev(setNames(1:ant_kvartal, paste0(1:ant_kvartal, '. kvartal')))
                    rev(setNames(paste0(input$valgtAar, c('-04-01', '-07-01', '-10-01'))[1:ant_kvartal], paste0(1:ant_kvartal, '. kvartal')))
                  } else {
                    # rev(setNames(1:4, paste0(1:4, '. kvartal')))
                    rev(setNames(paste0(c(input$valgtAar, input$valgtAar, input$valgtAar, as.numeric(input$valgtAar) + 1),
                                        c('-04-01', '-07-01', '-10-01', '-01-01')), paste0(1:4, '. kvartal')))
                  })
    }
  })

  contentFile2 <- function(file, baseName, datoFra, datoTil, reshID=0,
                           valgtShus='') {

    src <- system.file(paste0(baseName, ".Rnw"), package = "norgast")
    tmpFile <- tempfile(paste0(baseName, Sys.Date()), fileext = '.Rnw')

    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFile, overwrite = TRUE)

    pdfFile <- knitr::knit2pdf(tmpFile)
    file.copy(pdfFile, file)
  }

  output$lastNed_saml <- downloadHandler(
    filename = function(){
      paste0("samleDok", format(Sys.time(), format = "%Y-%m-%d-%H%M%S"), ".pdf")
    },
    content = function(file){
      contentFile2(
        file, "NorgastSamleDokShiny", input$datovalg[1],
        input$datovalg[2], reshID = reshID,
        valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {""})
    }
  )

  output$lastNed_saml_land <- downloadHandler(
    filename = function(){
      paste0("samleDokLandet", format(Sys.time(), format = "%Y-%m-%d-%H%M%S"),
             ".pdf")
    },
    content = function(file){
      contentFile2(file, "NorgastSamleDokLandetShiny", input$datovalg[1],
                  input$datovalg[2], reshID = reshID)
    }
  )

  output$lastNed_kvartal <- downloadHandler(
    filename = function(){
      paste0("Kvartalsrapp",
             RegData$Sykehusnavn[match(reshID, RegData$AvdRESH)],
             format(Sys.time(), format = "%Y-%m-%d-%H%M%S"), ".pdf")
    },
    content = function(file){
      contentFile2(
        file, "NorgastKvartalsrapportShiny",
        datoTil = input$kvartal_verdi, reshID = reshID,
        valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {""})

    }
  )
}
