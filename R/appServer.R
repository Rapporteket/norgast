#' Server logic for the norgast app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {

  if (rapbase::isRapContext()) {
    rapbase::appLogger(session = session, msg = 'Starter NoRGast')
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

  shiny::callModule(startside, "startside", usrRole=userRole)

  #################################################################################################################################
  ################ Fordelingsfigurer ##############################################################################################

  shiny::callModule(fordelingsfig, "fordelingsfig_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Sykehusvisning #################################################################################################

  shiny::callModule(sykehusvisning, "sykehusvisning_id", reshID = reshID, RegData = RegData, hvd_session = session)

  #################################################################################################################################
  ################ Traktplot ######################################################################################################

  shiny::callModule(traktplot, "traktplot_id", reshID = reshID, RegData = RegData, hvd_session = session, BrValg = BrValg)


  #################################################################################################################################
  ################ Tidsvisning ####################################################################################################

  shiny::callModule(tidsvisning, "tidsvisning_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Indikatorfigurer ###############################################################################################

  shiny::callModule(indikatorfig, "indikator_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Overlevelseskurver #############################################################################################

  shiny::callModule(overlevelse, "overlevelse_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Sammenlign utvalg ##############################################################################################

  shiny::callModule(saml_andeler, "saml_andeler_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)


  #################################################################################################################################
  ################ Samledokumenter ################################################################################################

  shiny::callModule(samledok, "samledok_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Datadump   #####################################################################################################

  shiny::callModule(datadump, "datadump_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Adm. tabeller ##################################################################################################

  shiny::callModule(admtab, "admtab_id", reshID = reshID, RegData = RegData, userRole = userRole,
                    hvd_session = session, skjemaoversikt=skjemaoversikt)

  #################################################################################################################################
  ################ Datakvalitet ###################################################################################################

  shiny::callModule(datakval_server, "datakval_id", reshID = reshID, userRole = userRole,
                    RegData = RegData, hvd_session = session)

  #############################################################################
  ################ Subscription, Dispatchment and Stats #######################

  ## Objects currently shared among subscription and dispathcment
  orgs <- as.list(BrValg$sykehus)
  reports <- list(
    Kvartalsrapport = list(
      synopsis = "NoRGast: Kvartalsrapport",
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


  #################################################################################################################################
  # Eksport  ###################################################################################################################
  # brukerkontroller
  rapbase::exportUCServer("norgastExport", "norgast")

  ## veileding
  rapbase::exportGuideServer("norgastExportGuide", "norgast")

  #################################################################################################################################







  }
