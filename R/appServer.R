#' Server logic for the norgast app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {
  rapbase::logShinyInputChanges(input)

  # RegData <-  norgast::NorgastHentRegData()
  appdata <- norgast::NorgastHentData()
  RegData <- appdata$RegData
  map_avdeling <- data.frame(
    UnitId = unique(RegData$AvdRESH),
    orgname = RegData$Sykehusnavn[match(unique(RegData$AvdRESH),
                                        RegData$AvdRESH)])

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "norgast",
    caller = "norgast",
    map_orgname = shiny::req(map_avdeling)
  )

  # skjemaoversikt <- norgast::NorgastHentskjemaoversikt()
  skjemaoversikt <- appdata$skjemaoversikt
  skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
  RegData <- norgast::NorgastPreprosess(RegData, behold_kladd = TRUE)
  skjemaoversikt <- merge(skjemaoversikt,
                          RegData[,c("ForlopsID", "Op_gr", "Hovedoperasjon")],
                          by = "ForlopsID", all.x = T)
  RegData <- RegData[which(RegData$RegistreringStatus==1),]
  RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
  # query <- "SELECT * FROM user"
  # brukerinfo <- rapbase::loadRegData("data", query, "mysql") |>
  #   dplyr::mutate(fullname = paste0(FIRSTNAME, " ", LASTNAME))
  # RegData$ForstLukketAv <-
  #   brukerinfo$fullname[match(RegData$ForstLukketAv, brukerinfo$ID)]
  # RegData$OppfForstLukketAv <-
  #   brukerinfo$fullname[match(RegData$OppfForstLukketAv, brukerinfo$ID)]
  BrValg <- norgast::BrValgNorgastShiny(RegData)

  rapbase::appLogger(session = session, msg = 'Starter NORGAST')

  shiny::observeEvent(
    shiny::req(user$role()), {
      # print(user$role())
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
    })

  norgast::startside_server("startside", usrRole=user$role)

  ##############################################################################
  ################ Fordelingsfigurer ###########################################

  norgast::fordelingsfig_server("fordelingsfig_id", reshID = user$org,
                                RegData = RegData, userRole = user$role,
                                hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Sykehusvisning ##############################################

  norgast::sykehusvisning_server("sykehusvisning_id", RegData = RegData,
                                 hvd_session = session,
                                 BrValg = BrValg)

  ##############################################################################
  ################ Traktplot ###################################################

  norgast::traktplot_server("traktplot_id", RegData = RegData,
                            hvd_session = session,
                            BrValg = BrValg)


  ##############################################################################
  ################ Tidsvisning #################################################

  norgast::tidsvisning_server("tidsvisning_id", reshID = user$org,
                              RegData = RegData, userRole = user$role,
                              hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Sammenlign utvalg ###########################################

  norgast::saml_andeler_server("saml_andeler_id", reshID = user$org,
                               RegData = RegData, userRole = user$role,
                               hvd_session = session, BrValg = BrValg)


  ##############################################################################
  ################ Indikatorfigurer ############################################

  norgast::indikatorfig_server("indikator_id", RegData = RegData,
                               userRole = user$role,
                               hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Overlevelseskurver ##########################################

  norgast::overlevelse_server("overlevelse_id", reshID = user$org,
                              RegData = RegData, userRole = user$role,
                              hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Samledokumenter #############################################

  norgast::samledok_server("samledok_id", reshID = user$org,
                           RegData = RegData, userRole = user$role,
                           hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Datadump   ##################################################

  norgast::datadump_server("datadump_id", reshID = user$org,
                           RegData = RegData, userRole = user$role,
                           brukerinfo=brukerinfo,
                           hvd_session = session, BrValg = BrValg)

  ##############################################################################
  ################ Adm. tabeller ###############################################

  norgast::admtab_server("admtab_id", RegData = RegData, userRole = user$role,
                         hvd_session = session, skjemaoversikt=skjemaoversikt,
                         BrValg = BrValg)

  ##############################################################################
  ################ Datakvalitet ################################################

  norgast::datakval_server("datakval_id",
                           reshID = user$org, userRole = user$role,
                           RegData = RegData, skjemaoversikt = skjemaoversikt,
                           hvd_session = session)

  ##############################################################################
  ################ Subscription, Dispatchment and Stats ########################

  ## Objects currently shared among subscription and dispathcment
  orgs <- as.list(BrValg$sykehus)
  org <- rapbase::autoReportOrgServer("norgastDispatch", orgs)

  subParamNames <- shiny::reactive(c("reshID"))
  subParamValues <- shiny::reactive(user$org())

  ## Subscription

  rapbase::autoReportServer(
    id = "norgastSubscription",
    registryName = "norgast",
    type = "subscription",
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = list(
      Kvartalsrapport = list(
        synopsis = "NORGAST: Kvartalsrapport",
        fun = "abonnement_kvartal_norgast",
        paramNames = c("baseName", "reshID"),
        paramValues = c("NorgastKvartalsrapport_abonnement", 999999)
      )
    ),
    orgs = orgs,
    freq = "quarter",
    user = user
  )

  ## Dispatchment


  vis_rapp <- reactiveVal(FALSE)
  observeEvent(user$role(), {
    vis_rapp(user$role() == "SC")
  })
  disParamNames <- shiny::reactive(c("reshID"))
  disParamValues <- shiny::reactive(c(org$value()))

  rapbase::autoReportServer(
    id = "norgastDispatch",
    registryName = "norgast",
    type = "dispatchment",
    org = org$value,
    paramNames = disParamNames,
    paramValues = disParamValues,
    reports = list(
      Kvartalsrapport = list(
        synopsis = "NORGAST: Kvartalsrapport",
        fun = "abonnement_kvartal_norgast",
        paramNames = c("baseName", "reshID"),
        paramValues = c("NorgastKvartalsrapport_abonnement", 999999)
      )
    ),
    orgs = orgs,
    eligible = vis_rapp,
    freq = "quarter",
    user = user
  )

  ## Metadata
  meta <- shiny::reactive({
    rapbase::describeRegistryDb("data")
  })

  output$metaControl <- shiny::renderUI({
    tabs <- names(meta())
    selectInput("metaTab", "Velg tabell:", tabs)
  })


  output$metaDataTable <- DT::renderDataTable(
    meta()[[input$metaTab]], rownames = FALSE,
    options = list(lengthMenu=c(25, 50, 100, 200, 400))
  )

  output$metaData <- shiny::renderUI({
    DT::dataTableOutput("metaDataTable")
  })

  ## Stats
  observe(
    rapbase::statsServer("norgastStats",
                         registryName = "norgast",
                         app_id = Sys.getenv("FALK_APP_ID"),
                         eligible = (user$role() == "SC"))
  )
  rapbase::statsGuideServer("norgastStatsGuide", registryName = "norgast")


  ##############################################################################
  # Eksport  ###################################################################
  # brukerkontroller
  rapbase::exportUCServer("norgastExport", "norgast")

  ## veileding
  rapbase::exportGuideServer("norgastExportGuide", "norgast")

  ##############################################################################


}

