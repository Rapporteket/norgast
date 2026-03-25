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

  skjemaoversikt <- appdata$skjemaoversikt
  skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
  RegData <- norgast::NorgastPreprosess(RegData, behold_kladd = TRUE)
  skjemaoversikt <- merge(skjemaoversikt,
                          RegData[,c("ForlopsID", "Op_gr", "Hovedoperasjon")],
                          by = "ForlopsID", all.x = T)
  RegData <- RegData[which(RegData$RegistreringStatus==1), ]
  RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
  BrValg <- norgast::BrValgNorgastShiny(RegData)

  rapbase::appLogger(session = session, msg = 'Starter NORGAST')

  # Legg til SC-spesifikke faner, og fjern dem for andre roller
  tabs_added <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(
    shiny::req(user$role()), {
      if (user$role() != 'SC') {
        if (tabs_added()) {
          shiny::removeTab("norgast_app_id", target = "Sykehusvisning")
          shiny::removeTab("norgast_app_id", target = "Traktplott")
          shiny::removeTab("norgast_app_id", target = "Indikatorer")
          tabs_added(FALSE)
        }
      } else {
        if (!tabs_added()) {
          shiny::insertTab(
            "norgast_app_id",
            tab = shiny::tabPanel("Sykehusvisning",
              norgast::sykehusvisning_ui("sykehusvisning_id")),
            target = "Fordeling", position = "after"
          )
          shiny::insertTab(
            "norgast_app_id",
            tab = shiny::tabPanel("Traktplott",
              norgast::traktplot_ui("traktplot_id")),
            target = "Sykehusvisning", position = "after"
          )
          shiny::insertTab(
            "norgast_app_id",
            tab = shiny::tabPanel("Indikatorer",
              norgast::indikatorfig_ui("indikator_id")),
            target = "Sammenlign utvalg", position = "after"
          )
          tabs_added(TRUE)
        }
      }
    }
  )
  
  # Legg til verktøy-fanen for SC-brukere, og fjern den for andre roller
  tool_tabs_added <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(user$role(), {
    if (user$role() == "SC") {
      if (!tool_tabs_added()) {
        shiny::insertTab(
          inputId = "norgast_app_id",
          tab = shiny::tabPanel(
            "Utsending",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                rapbase::autoReportOrgInput("norgastDispatch"),
                rapbase::autoReportInput("norgastDispatch")
              ),
              shiny::mainPanel(
                rapbase::autoReportUI("norgastDispatch")
              )
            )
          ),
          menuName = "Verktøy"
        )
        shiny::insertTab(
          inputId = "norgast_app_id",
          tab = shiny::tabPanel(
            "Metadata",
            shiny::sidebarLayout(
              shiny::sidebarPanel(shiny::uiOutput("metaControl")),
              shiny::mainPanel(shiny::htmlOutput("metaData"))
            )
          ),
          menuName = "Verktøy"
        )
        shiny::insertTab(
          inputId = "norgast_app_id",
          tab = shiny::tabPanel(
            "Eksport",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                rapbase::exportUCInput("norgastExport")
              ),
              shiny::mainPanel(
                rapbase::exportGuideUI("norgastExportGuide")
              )
            )
          ),
          menuName = "Verktøy"
        )
        shiny::insertTab(
          inputId = "norgast_app_id",
          tab = shiny::tabPanel(
            "Bruksstatistikk",
            shiny::sidebarLayout(
              shiny::sidebarPanel(rapbase::statsInput("norgastStats")),
              shiny::mainPanel(
                rapbase::statsUI("norgastStats"),
                rapbase::statsGuideUI("norgastStatsGuide")
              )
            )
          ),
          menuName = "Verktøy"
        )
        tool_tabs_added(TRUE)
      }
      shiny::showTab(inputId = "norgast_app_id", target = "Verktøy")
    } else {
      if (tool_tabs_added()) {
        shiny::removeTab(inputId = "norgast_app_id", target = "Utsending")
        shiny::removeTab(inputId = "norgast_app_id", target = "Metadata")
        shiny::removeTab(inputId = "norgast_app_id", target = "Eksport")
        shiny::removeTab(inputId = "norgast_app_id", target = "Bruksstatistikk")
        tool_tabs_added(FALSE)
      }
      shiny::hideTab(inputId = "norgast_app_id", target = "Verktøy")
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

