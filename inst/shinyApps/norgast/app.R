
######## Last data ########################################
library("norgast")
library("tidyverse")
library("kableExtra")
library("DT")
library("shiny")
library("shinyjs")
library("shinyalert")
library("lubridate")
library("survival")
library("survminer")
library("ggplot2")
library("funnelR")

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "NoRGast"

RegData <- rapbase::loadStagingData("norgast", "RegData") #Benyttes i appen
skjemaoversikt <- rapbase::loadStagingData("norgast", "skjemaoversikt") #Benyttes i appen
if (isFALSE(RegData) | isFALSE(skjemaoversikt)) {
  RegData <-  norgast::NorgastHentRegData()
  skjemaoversikt <- norgast::NorgastHentSkjemaOversikt()
  skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
  RegData <- norgast::NorgastPreprosess(RegData, behold_kladd = TRUE)
  skjemaoversikt <- merge(skjemaoversikt, RegData[,c("ForlopsID", "Op_gr", "Hovedoperasjon")], by = "ForlopsID", all.x = T)
  RegData <- RegData[which(RegData$RegistreringStatus==1),]
  RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
  rapbase::saveStagingData("norgast", "RegData", RegData)
  rapbase::saveStagingData("norgast", "skjemaoversikt", skjemaoversikt)
}

enhetsliste <- RegData[match(unique(RegData$AvdRESH), RegData$AvdRESH), c("AvdRESH", "Sykehusnavn")]
BrValg <- BrValgNorgastShiny(RegData)

source(system.file("shinyApps/norgast/R/modul_startside.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_fordelingsfig.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_sykehusvisning.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_tidsvisning.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_overlevelse.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_datadump.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_samledok.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_admtab.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_sammenlign_utvalg_tid.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_indikatorer.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_traktplot.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_datakvalitet.R", package = "norgast"), encoding = 'UTF-8')

######################################################################

# Define UI for application
ui <- navbarPage(id = "norgast_app_id",

                 title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                             regTitle),
                 windowTitle = regTitle,
                 theme = "rap/bootstrap.css",

                 shiny::tabPanel("Startside",
                                 shinyjs::useShinyjs(),
                                 rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                                              organization = uiOutput("appOrgName"),
                                                              addUserInfo = TRUE),
                                 tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"),
                                           includeCSS(system.file("shinyApps/norgast/www/yohannes.css", package = "norgast"))),
                                 startside_UI("startside")
                 ),

                 tabPanel("Fordelinger",
                          fordelingsfig_UI(id = "fordelingsfig_id", BrValg = BrValg)
                 ),

                 tabPanel("Sykehusvisning",
                          sykehusvisning_UI(id = "sykehusvisning_id", BrValg = BrValg)
                 ),

                 tabPanel("Traktplott",
                          traktplot_UI(id = "traktplot_id", BrValg = BrValg)
                 ),
                 shiny::navbarMenu("Tidsvisning",
                                   tabPanel("Andeler over tid",
                                            tidsvisning_UI(id = "tidsvisning_id", BrValg = BrValg)
                                   ),
                                   tabPanel("Sammenlign andeler",
                                            saml_andeler_UI(id = "saml_andeler_id", BrValg = BrValg)
                                   )
                 ),

                 tabPanel("Indikatorer",
                          indikatorfig_UI(id = "indikator_id", BrValg = BrValg)
                 ),

                 tabPanel("Overlevelse",
                          overlevelse_UI(id = "overlevelse_id", BrValg = BrValg)
                 ),

                 tabPanel("Samledokumenter",
                          h2("Samledokumenter", align='center'),
                          h4("Når du velger ", strong("Last ned samledokument"), " genereres en samlerapport bestående av figurer og tabeller.", align='center'),
                          # h4("Nærmere beskrivelse av de ulike samledokumentene finner du under de tilhørende fanene.", align='center'),
                          br(),
                          br(),
                          samledok_UI(id = "samledok_id", BrValg = BrValg)
                 ),

                 tabPanel("Datadump",
                          h2("Datadump", align='center'),
                          h4("Data på Rapporteket oppdateres én gang i døgnet. Følgelig kan det være små avvik i antall forløp
              som inkluderes i datadump på Rapporteket sammenlignet med datadump hentet fra registerets qreg-løsning.", align='center'),
                          br(),
                          br(),
                          datadump_UI(id = "datadump_id", BrValg = BrValg)
                 ),

                 tabPanel("Administrative tabeller",
                          admtab_UI(id = "admtab_id", BrValg = BrValg)
                 ),

                 shiny::tabPanel("Datakvalitet",
                                 datakval_ui("datakval_id")
                 ),

                 shiny::tabPanel(
                   shiny::span("Abonnement",
                               title="Bestill tilsending av rapporter på e-post"),
                   shiny::sidebarLayout(
                     shiny::sidebarPanel(
                       rapbase::autoReportInput("norgastSubscription")
                     ),
                     shiny::mainPanel(
                       rapbase::autoReportUI("norgastSubscription")
                     )
                   )
                 ),

                 shiny::navbarMenu("Verktøy",
                                   shiny::tabPanel(
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

                                   shiny::tabPanel(
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

                                   shiny::tabPanel(
                                     "Bruksstatistikk",
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(rapbase::statsInput("norgastStats")),
                                       shiny::mainPanel(
                                         rapbase::statsUI("norgastStats"),
                                         rapbase::statsGuideUI("norgastStatsGuide")
                                       )
                                     )
                                   )
                 )
)


server <- function(input, output, session) {

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

  callModule(fordelingsfig, "fordelingsfig_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Sykehusvisning #################################################################################################

  callModule(sykehusvisning, "sykehusvisning_id", reshID = reshID, RegData = RegData, hvd_session = session)

  #################################################################################################################################
  ################ Traktplot ######################################################################################################

  callModule(traktplot, "traktplot_id", reshID = reshID, RegData = RegData, hvd_session = session, BrValg = BrValg)


  #################################################################################################################################
  ################ Tidsvisning ####################################################################################################

  callModule(tidsvisning, "tidsvisning_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Indikatorfigurer ###############################################################################################

  callModule(indikatorfig, "indikator_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Overlevelseskurver #############################################################################################

  callModule(overlevelse, "overlevelse_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Sammenlign utvalg ##############################################################################################

  callModule(saml_andeler, "saml_andeler_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)


  #################################################################################################################################
  ################ Samledokumenter ################################################################################################

  callModule(samledok, "samledok_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Datadump   #####################################################################################################

  callModule(datadump, "datadump_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Adm. tabeller ##################################################################################################

  callModule(admtab, "admtab_id", reshID = reshID, RegData = RegData, userRole = userRole,
             hvd_session = session, skjemaoversikt=skjemaoversikt)

  #################################################################################################################################
  ################ Datakvalitet ###################################################################################################

  callModule(datakval_server, "datakval_id", reshID = reshID, userRole = userRole,
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
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
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

# Run the application
shinyApp(ui = ui, server = server)
