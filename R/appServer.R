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

  RegData <-  norgast::NorgastHentRegData()
  map_avdeling <- data.frame(
    UnitId = unique(RegData$AvdRESH),
    orgname = RegData$SykehusNavn[match(unique(RegData$AvdRESH),
                                        RegData$AvdRESH)])

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "norgast",
    caller = "norgast",
    map_orgname = shiny::req(map_avdeling)
  )

  skjemaoversikt <- norgast::NorgastHentskjemaoversikt()
  skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
  RegData <- norgast::NorgastPreprosess(RegData, behold_kladd = TRUE)
  skjemaoversikt <- merge(skjemaoversikt,
                          RegData[,c("ForlopsID", "Op_gr", "Hovedoperasjon")],
                          by = "ForlopsID", all.x = T)
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

  ## Subscription
  shiny::observe(

    rapbase::autoReportServer2(
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
      freq = "quarter",
      user = user
    )
  )

  ## Dispatchment
  org <- rapbase::autoReportOrgServer("norgastDispatch", orgs)

  paramNames <- shiny::reactive(c("reshID"))
  paramValues <- shiny::reactive(c(org$value()))

  observe(
    rapbase::autoReportServer2(
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
      eligible = (user$role() == "SC"),
      freq = "quarter",
      user = user
    )
  )

  # kjor_autorapport <- shiny::observeEvent(input$run_autoreport, {
  #   dato <- input$rapportdato
  #   dryRun <- !(input$dryRun)
  #   withCallingHandlers({
  #     shinyjs::html("sysMessage", "")
  #     shinyjs::html("funMessage", "")
  #     shinyjs::html("funMessage",
  #                   rapbase::runAutoReport(group = "norgast",
  #                                          dato = dato, dryRun = dryRun))
  #   },
  #   message = function(m) {
  #     shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
  #   })
  # })

  # output$confgreier1 <- shiny::renderText({
  #   paste("rapbase::getConfig(\"rapbaseConfig.yml\")$network$sender:",
  #         rapbase::getConfig("rapbaseConfig.yml")$network$sender)
  # })
  # output$confgreier2 <- shiny::renderText({
  #   paste("rapbase::getConfig(\"rapbaseConfig.yml\")$network$smtp$server:",
  #         rapbase::getConfig("rapbaseConfig.yml")$network$smtp$server)
  # })
  # output$confgreier3 <- shiny::renderText({
  #   paste("rapbase::getConfig(\"rapbaseConfig.yml\")$network$smtp$port:",
  #         rapbase::getConfig("rapbaseConfig.yml")$network$smtp$port)
  # })

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




  # autos <- rapbase::readAutoReportData(target = "db") %>%
  #   rapbase::filterAutoRep(by = "type", pass = type, target = "db") %>%
  #   rapbase::filterAutoRep(by = "package", pass = "norgast", target = "db") %>%
  #   dplyr::summarise(
  #     email = list(unique(email)),
  #     .by = c(owner, ownerName, package, organization, type, fun,
  #             params, startDate, terminateDate, interval, synopsis)
  #   )
  # output$autoraptab <- shiny::renderTable(autos)


}


# reports <- list(
#   Kvartalsrapport = list(
#     synopsis = "NORGAST: Kvartalsrapport",
#     fun = "abonnement_kvartal_norgast",
#     paramNames = c("baseName", "reshID"),
#     paramValues = c("NorgastKvartalsrapport_abonnement", user$org())
#   )
# )
# output$tabeller <- shiny::renderTable({
#   query <- paste0("SELECT table_name FROM information_schema.tables
#                   WHERE table_schema = '", Sys.getenv("MYSQL_DB_DATA"), "';")
#   tabell <- try(rapbase::loadRegData("data", query, "mysql"), TRUE)
# })
# # Environment
# output$user <- shiny::renderText({
#   paste("rapbase::getUserName(session):",
#         user$name())
# })
# output$group <- shiny::renderText({
#   paste("rapbase::getUserGroups(session):",
#         user$group())
# })
# output$resh_id <- shiny::renderText({
#   paste("rapbase::getUserReshId(session):",
#         user$org())
# })
# output$role <- shiny::renderText({
#   paste("rapbase::getUserRole(session):",
#         user$role())
# })
# output$database <- shiny::renderText({
#   Sys.getenv("MYSQL_DB_DATA")
# })
# output$full_name <- shiny::renderText({
#   paste("rapbase::getUserFullName(session):",
#         user$name())
# })
# output$instance <- shiny::renderText({
#   Sys.getenv("R_RAP_INSTANCE")
# })
# output$config_path <- shiny::renderText({
#   Sys.getenv("R_RAP_CONFIG_PATH")
# })
# output$sp_usergroups <- shiny::renderText({
#   paste("Sys.getenv('SHINYPROXY_USERGROUPS'):",
#         Sys.getenv("SHINYPROXY_USERGROUPS"))
# })
# output$locale <- shiny::renderText({
#   Sys.getlocale()
# })
#
# skjemaoversikt <- norgast::NorgastHentskjemaoversikt()
# skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
# output$skjemaoversikt <- shiny::renderTable({
#   head(skjemaoversikt)
# })
#
# query <- paste0("SELECT * FROM allevarnum")
# allevarnum <- rapbase::loadRegData("data", query, "mysql")
# output$allevarnum <- shiny::renderTable(
#   head(allevarnum[,1:10])
# )
#
# query <- paste0("SELECT * FROM forlopsoversikt")
# forlopsoversikt <- rapbase::loadRegData("data", query, "mysql")
# output$forlopsoversikt <- shiny::renderTable(
#   head(forlopsoversikt[,1:10])
# )
#
# query <- paste0("SELECT * FROM user")
# user_tab <- rapbase::loadRegData("data", query, "mysql")
# output$user_tab <- shiny::renderTable(
#   head(user_tab)
# )


