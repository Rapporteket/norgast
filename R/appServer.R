#' Server logic for the norgast app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "NORGAST",
    caller = packageName()
  )

  RegData <-  norgast::NorgastHentRegData()
  skjemaoversikt <- norgast::NorgastHentSkjemaOversikt()
  skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
  RegData <- norgast::NorgastPreprosess(RegData, behold_kladd = TRUE)
  skjemaoversikt <- merge(skjemaoversikt, RegData[,c("ForlopsID", "Op_gr", "Hovedoperasjon")], by = "ForlopsID", all.x = T)
  RegData <- RegData[which(RegData$RegistreringStatus==1),]
  RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
  query <- "SELECT * FROM user"
  brukerinfo <- rapbase::loadRegData("data", query, "mysql") %>%
    dplyr::mutate(fullname = paste0(FIRSTNAME, " ", LASTNAME))
  RegData$ForstLukketAv <-
    brukerinfo$fullname[match(RegData$ForstLukketAv, brukerinfo$ID)]
  RegData$OppfForstLukketAv <-
    brukerinfo$fullname[match(RegData$OppfForstLukketAv, brukerinfo$ID)]
  BrValg <- norgast::BrValgNorgastShiny(RegData)

  # if (rapbase::isRapContext()) {
  #   rapbase::appLogger(session = session, msg = 'Starter NORGAST')
  #   reshID <- rapbase::getUserReshId(session)
  #   userRole <- rapbase::getUserRole(session)
  # } else {
  #   reshID <- 601225
  #   userRole <- 'SC'
  # }

  shiny::observeEvent(
    user$role(),
    if (user$role() != 'SC') {
      shiny::hideTab("norgast_app_id", target = "Sykehusvisning")
      shiny::hideTab("norgast_app_id", target = "Utsending")
      # shiny::hideTab("norgast_app_id", target = "Datakvalitet")
      shiny::hideTab("norgast_app_id", target = "Eksport")
      shiny::hideTab("norgast_app_id", target = "Traktplott")
      shiny::hideTab("norgast_app_id", target = "Indikatorer")
      shiny::hideTab("norgast_app_id", target = "Verktøy")
    } else {
      shiny::showTab("norgast_app_id", target = "Sykehusvisning")
      shiny::showTab("norgast_app_id", target = "Utsending")
      # shiny::showTab("norgast_app_id", target = "Datakvalitet")
      shiny::showTab("norgast_app_id", target = "Eksport")
      shiny::showTab("norgast_app_id", target = "Traktplott")
      shiny::showTab("norgast_app_id", target = "Indikatorer")
      shiny::showTab("norgast_app_id", target = "Verktøy")
    }
  )

  shiny::callModule(norgast::startside, "startside", usrRole=user$role())

  ##############################################################################
  ################ Fordelingsfigurer ###########################################

  shiny::callModule(norgast::fordelingsfig, "fordelingsfig_id", reshID = user$org(),
                    RegData = RegData, userRole = user$role(),
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Sykehusvisning ##############################################

  shiny::callModule(norgast::sykehusvisning, "sykehusvisning_id", reshID = user$org(),
                    RegData = RegData, hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Traktplot ###################################################

  shiny::callModule(norgast::traktplot, "traktplot_id", reshID = user$org(),
                    RegData = RegData, hvd_session = session, BrValg = BrValg)


  ##############################################################################
  ################ Tidsvisning #################################################

  shiny::callModule(norgast::tidsvisning, "tidsvisning_id", reshID = user$org(),
                    RegData = RegData, userRole = user$role(),
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Sammenlign utvalg ###########################################

  shiny::callModule(norgast::saml_andeler, "saml_andeler_id", reshID = user$org(),
                    RegData = RegData, userRole = user$role(),
                    hvd_session = session, BrValg = BrValg)


  ##############################################################################
  ################ Indikatorfigurer ############################################

  shiny::callModule(norgast::indikatorfig, "indikator_id", reshID = user$org(),
                    RegData = RegData, userRole = user$role(),
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Overlevelseskurver ##########################################

  shiny::callModule(norgast::overlevelse, "overlevelse_id", reshID = user$org(),
                    RegData = RegData, userRole = user$role(),
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Samledokumenter #############################################

  shiny::callModule(norgast::samledok, "samledok_id", reshID = user$org(),
                    RegData = RegData, userRole = user$role(),
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Datadump   ##################################################

  shiny::callModule(norgast::datadump, "datadump_id", reshID = user$org(),
                    RegData = RegData, userRole = user$role(), brukerinfo=brukerinfo,
                    hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Adm. tabeller ###############################################

  shiny::callModule(norgast::admtab, "admtab_id", reshID = user$org(),
                    RegData = RegData, userRole = user$role(),
                    hvd_session = session, skjemaoversikt=skjemaoversikt,
                    BrValg = BrValg)

  ##############################################################################
  ################ Datakvalitet ################################################

  shiny::callModule(norgast::datakval_server, "datakval_id",
                    reshID = user$org(), userRole = user$role(),
                    RegData = RegData, SkjemaOversikt = skjemaoversikt,
                    hvd_session = session)

  ##############################################################################
  ################ Subscription, Dispatchment and Stats ########################

  ## Objects currently shared among subscription and dispathcment
  orgs <- as.list(BrValg$sykehus)
  # reports <- list(
  #   Kvartalsrapport = list(
  #     synopsis = "NORGAST: Kvartalsrapport",
  #     fun = "abonnement_kvartal_norgast",
  #     paramNames = c("baseName", "reshID"),
  #     paramValues = c("NorgastKvartalsrapport_abonnement", user$org())
  #   )
  # )

  ## Subscription
  shiny::observe(

    rapbase::autoReportServer(
      id = "norgastSubscription",
      registryName = "norgast",
      type = "subscription",
      reports = list(
        Kvartalsrapport = list(
          synopsis = "NORGAST: Kvartalsrapport",
          fun = "abonnement_kvartal_norgast",
          paramNames = c("baseName", "reshID"),
          paramValues = c("NorgastKvartalsrapport_abonnement", user$org())
        )
      ),
      orgs = orgs,
      freq = "quarter"
    )
  )

  ## Dispatchment
  org <- rapbase::autoReportOrgServer("norgastDispatch", orgs)

  paramNames <- shiny::reactive(c("reshID"))
  paramValues <- shiny::reactive(c(org$value()))

  observe(
    rapbase::autoReportServer(
      id = "norgastDispatch",
      registryName = "norgast",
      type = "dispatchment",
      org = org$value,
      paramNames = paramNames,
      paramValues = paramValues,
      reports = list(
        Kvartalsrapport = list(
          synopsis = "NORGAST: Kvartalsrapport",
          fun = "abonnement_kvartal_norgast",
          paramNames = c("baseName", "reshID"),
          paramValues = c("NorgastKvartalsrapport_abonnement", user$org())
        )
      ),
      orgs = orgs,
      eligible = (user$org() == "SC"),
      freq = "quarter"
    )
  )

  ## Stats
  observe(
    rapbase::statsServer("norgastStats", registryName = "norgast",
                         eligible = (user$org() == "SC"))
  )
  rapbase::statsGuideServer("norgastStatsGuide", registryName = "norgast")


  # #Navbarwidget
  # output$appUserName <- renderText(rapbase::getUserFullName(session))
  # output$appOrgName <-
  #   shiny::renderText(
  #     names(BrValg$sykehus[BrValg$sykehus == user$org()])
  #   )

  # Brukerinformasjon
  # userInfo <- rapbase::howWeDealWithPersonalData(session)
  # shiny::observeEvent(input$userInfo, {
  #   shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
  #                          type = "", imageUrl = "rap/logo.svg",
  #                          closeOnEsc = TRUE, closeOnClickOutside = TRUE,
  #                          html = TRUE, confirmButtonText = "Den er grei!")
  # })


  ##############################################################################
  # Eksport  ###################################################################
  # brukerkontroller
  rapbase::exportUCServer("norgastExport", "norgast")

  ## veileding
  rapbase::exportGuideServer("norgastExportGuide", "norgast")

  ##############################################################################



}
