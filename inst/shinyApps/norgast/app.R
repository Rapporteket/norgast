
######## Last data ########################################
library(norgast)
library(tidyverse)
library(kableExtra)
library(DT)
library(shiny)
library(shinyjs)
# library(shinyBS)
library(shinyalert)
library(lubridate)
library(survival)
library(survminer)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "NoRGast"
logo <- includeHTML(system.file('www/logo.svg', package='rapbase'))
logoCode <- paste0("var header = $('.navbar> .container-fluid');\n",
                   "header.append('<div class=\"navbar-brand\" style=\"float:left;font-size:75%\">",
                   logo,
                   "</div>');\n",
                   "console.log(header)")
logoWidget <- tags$script(shiny::HTML(logoCode))


if (rapbase::isRapContext()) {
  RegData <- NorgastHentRegData()
  skjemaoversikt <- NorgastHentSkjemaOversikt()
} else {
  Sys.setenv(R_RAP_CONFIG_PATH='C:/GIT/norgast/doc')
  RegData <- read.table('I:/norgast/AlleVarNum2021-02-03 15-28-13.txt', header=TRUE, sep=";",
                        encoding = 'UTF-8', stringsAsFactors = F)
  ForlopData <- read.table('I:/norgast/ForlopsOversikt2021-02-03 15-28-13.txt', header=TRUE, sep=";",
                           encoding = 'UTF-8', stringsAsFactors = F)

  RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                        'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                        'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                        'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                        'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad",
                        "AvstandAnalVerge")]
  names(ForlopData)[names(ForlopData) %in% c("SykehusNavn", "erMann")] <- c("Sykehusnavn", "ErMann")
  ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
  RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")

  skjemaoversikt <- read.table('I:/norgast/SkjemaOversikt2021-02-03 15-28-13.txt', header=TRUE, sep=';', stringsAsFactors = F, encoding = 'UTF-8')
}

skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)


RegData <- NorgastPreprosess(RegData)
# RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)
enhetsliste <- RegData[match(unique(RegData$AvdRESH), RegData$AvdRESH), c("AvdRESH", "Sykehusnavn")]

BrValg <- BrValgNorgastShiny(RegData)

source(system.file("shinyApps/norgast/R/modul_startside.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_fordelingsfig.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_sykehusvisning.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_tidsvisning.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_overlevelse.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_datadump.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_samledok.R", package = "norgast"), encoding = 'UTF-8')
# source(system.file("shinyApps/norgast/R/modul_admtab.R", package = "norgast"), encoding = 'UTF-8')

######################################################################

# Define UI for application
ui <- navbarPage(id = "norgast_app_id",

  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              regTitle),
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",

  shiny::tabPanel("Startside",
                  shinyjs::useShinyjs(),
                  shinyalert::useShinyalert(),
                  rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                               organization = uiOutput("appOrgName"),
                                               addUserInfo = TRUE),
                  tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
                  startside_UI("startside")
  ),

  tabPanel("Fordelinger",
           fordelingsfig_UI(id = "fordelingsfig_id", BrValg = BrValg)
  ),

  tabPanel("Sykehusvisning",
           sykehusvisning_UI(id = "sykehusvisning_id", BrValg = BrValg)
  ),

  tabPanel("Tidsvisning",
           tidsvisning_UI(id = "tidsvisning_id", BrValg = BrValg)
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
           h4("Velg variablene du ønsker inkludert i datadump og for hvilken tidsperiode.", align='center'),
           # h4("Linn og Kristoffer lager mer tekst hvis ønskelig.", align='center'),
           br(),
           br(),
           datadump_UI(id = "datadump_id", BrValg = BrValg)
  ),

  # tabPanel("Administrative tabeller",
  #          admtab_UI(id = "admtab_id")
  # ),


  tabPanel(p("Abonnement",
             title='Bestill automatisk utsending av rapporter på e-post'),
           value = 'adm_abonnement',
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput("subscriptionRep", "Rapport:",
                                      c("Kvartalsrapport")), #, "Samlerapport", "Influensaresultater")),
                          selectInput("subscriptionFreq", "Frekvens:",
                                      list(Årlig="Årlig-year",
                                            Kvartalsvis="Kvartalsvis-quarter",
                                            Månedlig="Månedlig-month",
                                            Ukentlig="Ukentlig-week",
                                            Daglig="Daglig-DSTday"),
                                      selected = "Kvartalsvis-quarter"),
                          dateInput("dato_forste_rap", label="Velg dato for første utsending", value = Sys.Date()+1,
                                    min = Sys.Date()+1, language = "nb"),
                          dateInput("dato_siste_rap", label="Velg dato for avslutning av abonnement", value = Sys.Date()+years(3),
                                    min = Sys.Date()+1, language = "nb"),
                          fileInput("file1", "Last opp CSV-fil med e-post og RESH for potensielle rapportmottakere. Filen må ha en
                                    kolonne med overskrift 'epost' som inneholder e-postadressene til potensielle mottakere av rapporten,
                                    og en kolonne med overskrift 'resh' som angir hvilken avdeling de tilhørende e-postadressene skal
                                    motta rapport for.",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          # bsTooltip(id = "file1", title= 'Forutsetter en semikolonseparert csv-fil med en kolonne med tittel
                          #           epost og en kolonne med tittel resh som angir henholdsvis e-postadresse og avdelingstilhørighet.'
                          #           , options = list(container = "body")),
                          uiOutput("norgast_brukere"),
                          selectInput(inputId = "valgtShus", label = "Velg sykehus rapport skal lages for",
                                      choices = BrValg$sykehus, multiple = FALSE),
                          actionButton("subscribe", "Bestill!")
             ),
             mainPanel(
               uiOutput("subscriptionContent")
             )
           )
  )

)


server <- function(input, output, session) {

  if (rapbase::isRapContext()) {
    raplog::appLogger(session = session, msg = 'Starter NoRGast')
    reshID <- rapbase::getUserReshId(session)
    userRole <- rapbase::getUserRole(session)
  } else {
    reshID <- 601225
    userRole <- 'SC'
  }

  if (userRole != 'SC') {
    shiny::hideTab("norgast_app_id", target = "Sykehusvisning")
    shinyjs::hide(id = 'valgtShus')
    shinyjs::hide(id = 'file1')
  }

  shiny::callModule(startside, "startside", usrRole=userRole)

  #################################################################################################################################
  ################ Fordelingsfigurer ##############################################################################################

  callModule(fordelingsfig, "fordelingsfig_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Sykehusvisning ########################################################################################################

  callModule(sykehusvisning, "sykehusvisning_id", reshID = reshID, RegData = RegData, hvd_session = session)

  #################################################################################################################################
  ################ Tidsvisning ########################################################################################################

  callModule(tidsvisning, "tidsvisning_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Overlevelseskurver ##################################################################################################

  callModule(overlevelse, "overlevelse_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Samledokumenter ##################################################################################################

  callModule(samledok, "samledok_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Datadump   ##################################################################################################

  callModule(datadump, "datadump_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Adm. tabeller ##################################################################################################

  # callModule(admtab, "admtab_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session, skjemaoversikt=skjemaoversikt)

  #################################################################################################################################
  ################ Abonnement ##################################################################################################
  #####################################################################################
  #####################################################################################

  make_named_vector <- function(x) {setNames(x$lnr, x$epost)}

  les_brukerliste <- reactive(
    if (!is.null(input$file1)) {
      df <- read.csv2(input$file1$datapath, header = T, stringsAsFactors = F)
      names(df) <- names(df) %>% trimws() %>% tolower()
      df$shus <- RegData$Sykehusnavn[match(df$resh, RegData$AvdRESH)]
      df$shus[is.na(df$shus)] <- 'Ukjent'
      df$lnr <- 1:dim(df)[1]
      nestliste <- df[, c("shus", "epost", "lnr")] %>% group_by(shus) %>% nest()
      gulp <- map(nestliste$data, make_named_vector)
      nestliste$data <- gulp
      testliste <- tapply(nestliste$data, nestliste$shus, function(x) {x[1]})
      list(df=df, testliste=testliste)
    }
  )

  output$norgast_brukere <- renderUI({
    if (!is.null(input$file1)) {
      selectInput(inputId = "brukere_epost", label = "Velg brukere som skal motta rapport",
                  choices = les_brukerliste()$testliste, multiple = TRUE)
    }
  })


  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab_v2(session, map_resh_name=enhetsliste)
    # subscriptionTab = rapbase::makeUserSubscriptionTab(session)
    )

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    rownames = FALSE, options = list(dom = "tp", pageLength = 15, language = list(
  paginate = list(previous = "Forrige",
                  `next` = "Neste")))
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    fullName <- rapbase::getUserFullName(session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", fullName))
    } else {
      tagList(
        p("Aktive abonnement som sendes per epost:"),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })
  ## nye abonnement
  observeEvent (input$subscribe, { #MÅ HA
    owner <- rapbase::getUserName(session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    organization <- rapbase::getUserReshId(session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(startDay = as.Date(input$dato_forste_rap), interval = interval)
    if (!is.null(input$file1)) {
      email <- les_brukerliste()$df$epost[match(input$brukere_epost, les_brukerliste()$df$lnr)]
    } else {
      email <- rapbase::getUserEmail(session)
    }

    # email <- c('kevin.thon@gmail.com', 'kevin.thon@skde.no')
    if (input$subscriptionRep == "Kvartalsrapport") {
      synopsis <- "NoRGast: Kvartalsrapport"
      baseName <- "NorgastKvartalsrapport_abonnement" #Navn på fila
      #print(rnwFil)
    }

    fun <- "abonnement_kvartal_norgast"  #"henteSamlerapporter"

    paramNames <- c('baseName', "reshID")
    paramValues <- c(baseName, if (userRole == 'SC') {input$valgtShus} else {reshID})

    rapbase::createAutoReport(synopsis = synopsis, package = 'norgast',
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear, interval = interval,
                              intervalName = intervalName, terminateDate = input$dato_siste_rap)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab_v2(session, map_resh_name=enhetsliste)
    # rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })

  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab_v2(session, map_resh_name=enhetsliste)
    # rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })

  #####################################################################################
  #####################################################################################
  #####################################################################################

  #Navbarwidget
  output$appUserName <- renderText(rapbase::getUserFullName(session))
  output$appOrgName <- renderText(rapbase::getUserReshId(session))

  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = "Den er grei!")
  })




}

# Run the application
shinyApp(ui = ui, server = server)




# tabPanel("testfluider",
#          fluidRow(
#            column(2,
#                   sliderInput("obs", "Number of observations:",
#                               min = 1, max = 1000, value = 500)
#            ),
#            column(8,
#                   plotOutput("distPlot")
#            ),
#            column(2,
#                   sliderInput("obs2", "andre siden:",
#                               min = 1, max = 1000, value = 500))
#          )
# ),
