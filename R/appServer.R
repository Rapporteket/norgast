#' Server logic for the norgast app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {

  RegData <- rapbase::loadStagingData("norgast", "RegData") #Benyttes i appen
  skjemaoversikt <- rapbase::loadStagingData("norgast", "skjemaoversikt") #Benyttes i appen
  if (isFALSE(RegData) | isFALSE(skjemaoversikt)) {
    norgast::norgastMakeStagingData()
    RegData <- rapbase::loadStagingData("norgast", "RegData") #Benyttes i appen
    skjemaoversikt <- rapbase::loadStagingData("norgast", "skjemaoversikt") #Benyttes i appen
  }
  BrValg <- norgast::BrValgNorgastShiny(RegData)

  if (rapbase::isRapContext()) {
    rapbase::appLogger(session = session, msg = 'Starter NORGAST')
    reshID <- rapbase::getUserReshId(session)
    userRole <- rapbase::getUserRole(session)
  } else {
    reshID <- 601225
    userRole <- 'SC'
  }

  if (userRole != 'SC') {
    shiny::hideTab("norgast_app_id", target = "Sykehusvisning")
    shiny::hideTab("norgast_app_id", target = "Utsending")
    # shiny::hideTab("norgast_app_id", target = "Datakvalitet")
    shiny::hideTab("norgast_app_id", target = "Eksport")
    shiny::hideTab("norgast_app_id", target = "Traktplott")
    shiny::hideTab("norgast_app_id", target = "Indikatorer")
    shiny::hideTab("norgast_app_id", target = "Verktøy")
  }

  shiny::callModule(norgast::startside, "startside", usrRole=userRole)

  ##############################################################################
  ################ Fordelingsfigurer ###########################################

  shiny::callModule(norgast::fordelingsfig, "fordelingsfig_id", reshID = reshID,
                    RegData = RegData, userRole = userRole,
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Sykehusvisning ##############################################

  shiny::callModule(norgast::sykehusvisning, "sykehusvisning_id", reshID = reshID,
                    RegData = RegData, hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Traktplot ###################################################

  shiny::callModule(norgast::traktplot, "traktplot_id", reshID = reshID,
                    RegData = RegData, hvd_session = session, BrValg = BrValg)


  ##############################################################################
  ################ Tidsvisning #################################################

  shiny::callModule(norgast::tidsvisning, "tidsvisning_id", reshID = reshID,
                    RegData = RegData, userRole = userRole,
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Sammenlign utvalg ###########################################

  shiny::callModule(norgast::saml_andeler, "saml_andeler_id", reshID = reshID,
                    RegData = RegData, userRole = userRole,
                    hvd_session = session, BrValg = BrValg)


  ##############################################################################
  ################ Indikatorfigurer ############################################

  shiny::callModule(norgast::indikatorfig, "indikator_id", reshID = reshID,
                    RegData = RegData, userRole = userRole,
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Overlevelseskurver ##########################################

  shiny::callModule(norgast::overlevelse, "overlevelse_id", reshID = reshID,
                    RegData = RegData, userRole = userRole,
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Samledokumenter #############################################

  shiny::callModule(norgast::samledok, "samledok_id", reshID = reshID,
                    RegData = RegData, userRole = userRole,
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Datadump   ##################################################

  shiny::callModule(norgast::datadump, "datadump_id", reshID = reshID,
                    RegData = RegData, userRole = userRole,
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Adm. tabeller ###############################################

  shiny::callModule(norgast::admtab, "admtab_id", reshID = reshID,
                    RegData = RegData, userRole = userRole,
                    hvd_session = session, skjemaoversikt=skjemaoversikt,
                    BrValg = BrValg)

  ##############################################################################
  ################ Datakvalitet ################################################

  shiny::callModule(norgast::datakval_server, "datakval_id",
                    reshID = reshID, userRole = userRole,
                    RegData = RegData, SkjemaOversikt = skjemaoversikt,
                    hvd_session = session)

  ##############################################################################
  ################ Subscription, Dispatchment and Stats ########################

  ## Objects currently shared among subscription and dispathcment
  orgs <- as.list(BrValg$sykehus)
  reports <- list(
    Kvartalsrapport = list(
      synopsis = "NORGAST: Kvartalsrapport",
      fun = "abonnement_kvartal_norgast",
      paramNames = c("baseName", "reshID"),
      paramValues = c("NorgastKvartalsrapport_abonnement", reshID)
    )
  )

  ## Subscription
  rapbase::autoReportServer(
    id = "norgastSubscription", registryName = "norgast",
    type = "subscription", reports = reports, orgs = orgs, freq = "quarter"
  )

  ## Dispatchment
  org <- rapbase::autoReportOrgServer("norgastDispatch", orgs)

  paramNames <- shiny::reactive(c("reshID"))
  paramValues <- shiny::reactive(c(org$value()))

  rapbase::autoReportServer(
    id = "norgastDispatch", registryName = "norgast",
    type = "dispatchment", org = org$value, paramNames = paramNames,
    paramValues = paramValues, reports = reports, orgs = orgs,
    eligible = (userRole == "SC"), freq = "quarter"
  )

  ## Stats
  rapbase::statsServer("norgastStats", registryName = "norgast",
                       eligible = (userRole == "SC"))
  rapbase::statsGuideServer("norgastStatsGuide", registryName = "norgast")


  #Navbarwidget
  output$appUserName <- renderText(rapbase::getUserFullName(session))
  output$appOrgName <-
    shiny::renderText(
      names(BrValg$sykehus[BrValg$sykehus == rapbase::getUserReshId(session)])
    )

  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = "Den er grei!")
  })


  ##############################################################################
  # Eksport  ###################################################################
  # brukerkontroller
  rapbase::exportUCServer("norgastExport", "norgast")

  ## veileding
  rapbase::exportGuideServer("norgastExportGuide", "norgast")

  ##############################################################################



  }
