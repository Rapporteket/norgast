# Modul for abonnement i NoRGast sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @return Modul fordelingsfigur
#
abonnement_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(ns("subscriptionRep"), "Rapport:",
                  c("Kvartalsrapport")),
      selectInput(ns("subscriptionFreq"), "Frekvens:",
                  list(Årlig="Årlig-year",
                        Kvartalsvis="Kvartalsvis-quarter",
                        Månedlig="Månedlig-month",
                        Ukentlig="Ukentlig-week",
                        Daglig="Daglig-DSTday"),
                  selected = "Månedlig-month"),
      #selectInput("subscriptionFileFormat", "Format:",
      #            c("html", "pdf")),
      actionButton(ns("subscribe"), "Bestill!")
    ),
    mainPanel(
      uiOutput(outputId = ns('subscriptionContent'))
    )
  )

}




abonnement <- function(input, output, session, reshID, userRole, hvd_session){

  # shinyjs::onclick("toggleAdvanced",
  #                  shinyjs::toggle(id = "avansert", anim = TRUE))

  # observeEvent(input$reset_input, {
  #   shinyjs::reset("id_fordeling_panel")
  # })
  #
  # observe(
  #   if (userRole != 'SC') {
  #     shinyjs::hide(id = 'valgtShus')
  #   })

  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(hvd_session))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    rownames = FALSE, options = list(dom = 't')
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    ns <- session$ns
    fullName <- rapbase::getUserFullName(hvd_session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", fullName))
    } else {
      tagList(
        p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
                rapbase::getUserEmail(hvd_session), ":")),
        DT::dataTableOutput(ns("activeSubscriptions"))
      )
    }
  })
  ## nye abonnement
  observeEvent (input$subscribe, { #MÅ HA
    owner <- rapbase::getUserName(hvd_session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    organization <- rapbase::getUserReshId(hvd_session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(interval = interval)
    email <- rapbase::getUserEmail(hvd_session)
    if (input$subscriptionRep == "Kvartalsrapport") {
      synopsis <- "norgast/Rapporteket: kvartalsrapport"
      baseName <- "NorgastKvartalsrapport_abonnement" #Navn på fila
      #print(rnwFil)
    }

    fun <- "abonnement_kvartal_norgast"  #"henteSamlerapporter"

    paramNames <- c('baseName', "reshID")
    paramValues <- c(baseName, reshID) #input$subscriptionFileFormat)

    rapbase::createAutoReport(synopsis = synopsis, package = 'norgast',
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear, interval = interval,
                              intervalName = intervalName)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(hvd_session)
  })


  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(hvd_session)
  })



  shiny::observe({
    if (rapbase::isRapContext()) {
      if (req(input$tabs_sykehusvisning) == "tab_gjsn") {
        mld_fordeling <- paste(
          "NoRGast: tabell - sykehusvisning gj.snitt. variabel - ",
          input$valgtVar_gjsn)
      }
      rapbase::repLogger(
        session = hvd_session,
        msg = mld_fordeling
      )

      shinyjs::onclick(
        "lastNedBilde_sykehus_andel",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NoRGast: nedlasting figur - sykehusvisning andel. variabel -",
            input$valgtVar
          )
        )
      )

    }
  })





}
