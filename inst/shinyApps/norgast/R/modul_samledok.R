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
    sidebarPanel(
      id = ns("id_samledok_panel"),
      conditionalPanel(condition = paste0("input['", ns('tabs'), "'] == 'samledok_egen' |
                                          input['", ns('tabs'), "'] == 'samledok_landet'"),
                       dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01', language = "nb",
                                      max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), separator = " til ")),
      conditionalPanel(condition = paste0("input['", ns('tabs'), "'] == 'kvartalsrapport'"),
                       dateInput(inputId=ns("datovalg_kv"), label = "Dato til", min = '2015-01-01',
                                      max = Sys.Date(), value  = Sys.Date(), language = "nb"),
                       helpText("Velg en dato etter avslutningen av det siste",
                                "kvartalet som ønskes inkludert i rapporten")),
      selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                  choices = BrValg$sykehus, multiple = TRUE),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(
      tabsetPanel(id= ns("tabs"),
                  tabPanel("Samledokument med egen avd. mot landet forøvrig", value = "samledok_egen",
                           h4("Kristoffer og Linn lager en tekst som skal inn her."),
                           downloadButton(ns("lastNed_saml"), "Last ned samledokument")),
                  tabPanel("Samledokument med nasjonale tall", value = "samledok_landet",
                           h4("Kristoffer og Linn lager en tekst som skal inn her."),
                           downloadButton(ns("lastNed_saml_land"), "Last ned samledokument")),
                  tabPanel("Kvartalsrapport for din avdeling", value = "kvartalsrapport",
                           h4("Kristoffer og Linn lager en tekst som skal inn her."),
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

  # render file function for re-use
  contentFile <- function(file, srcFile, tmpFile, datoFra, datoTil, reshID=0, valgtShus='') {
    src <- normalizePath(system.file(srcFile, package="norgast"))

    # temporarily switch to the temp dir, in case we do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFile, overwrite = TRUE)

    texfil <- knitr::knit(tmpFile, encoding = 'UTF-8')
    tools::texi2pdf(texfil, clean = TRUE)

    gc()
    file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
    # file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
  }

  output$lastNed_saml <- downloadHandler(
    filename = function(){
      paste0('samleDok', Sys.time(), '.pdf')
    },

    content = function(file){
      contentFile(file, "NorgastSamleDokShiny.Rnw", "tmpNorgastSamle.Rnw", input$datovalg[1],
                  input$datovalg[2], reshID=reshID,
                  valgtShus=if (!is.null(input$valgtShus)) {input$valgtShus} else {''})
    }
  )

  output$lastNed_saml_land <- downloadHandler(
    filename = function(){
      paste0('samleDokLandet', Sys.time(), '.pdf')
    },

    content = function(file){
      contentFile(file, "NorgastSamleDokLandetShiny.Rnw", "tmpNorgastSamleLandet.Rnw", input$datovalg[1],
                  input$datovalg[2], reshID=reshID)
    }
  )

  output$lastNed_kvartal <- downloadHandler(
    filename = function(){
      paste0('Kvartalsrapp', RegData$Sykehusnavn[match(reshID, RegData$AvdRESH)], Sys.time(), '.pdf')
    },

    content = function(file){
      contentFile(file, "NorgastKvartalsrapportShiny.Rnw", "tmpNorgastKvartalsrapportShiny.Rnw",
                  datoTil=input$datovalg_kv, reshID=reshID,
                  valgtShus=if (!is.null(input$valgtShus)) {input$valgtShus} else {''})
    }
  )

}
