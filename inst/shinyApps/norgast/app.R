
######## Last data ########################################
library(norgast)
library(tidyverse)
library(kableExtra)
library(DT)
library(shiny)
library(shinyjs)
library(shinyBS)
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
  RegData <- read.table('I:/norgast/AlleVarNum2020-01-13 13-43-56.txt', header=TRUE, sep=";",
                        encoding = 'UTF-8', stringsAsFactors = F)
  ForlopData <- read.table('I:/norgast/ForlopsOversikt2020-01-13 13-44-23.txt', header=TRUE, sep=";",
                           encoding = 'UTF-8', stringsAsFactors = F)

  RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                        'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                        'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                        'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                        'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad")]
  ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
  RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")

  skjemaoversikt <- read.table('I:/norgast/SkjemaOversikt2020-01-13 13-44-27.txt', header=TRUE, sep=';', stringsAsFactors = F, encoding = 'UTF-8')
}
skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)

RegData <- NorgastPreprosess(RegData)
RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)

BrValg <- BrValgNorgastShiny(RegData)

source(system.file("shinyApps/norgast/R/modul_startside.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_fordelingsfig.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_sykehusvisning.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_tidsvisning.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_overlevelse.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_datadump.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_samledok.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_admtab.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_abonnement.R", package = "norgast"), encoding = 'UTF-8')

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

  tabPanel("Administrative tabeller",
           admtab_UI(id = "admtab_id")
  ),

  # tabPanel("Abonnement",
  #          abonnement_UI(id = "abonnement_id")
  # )

  tabPanel(p("Abonnement",
             title='Bestill automatisk utsending av rapporter på e-post'),
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
                          fileInput("file1", "Last opp CSV-fil med e-post og RESH for potensielle rapportmottakere",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
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

  callModule(admtab, "admtab_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session, skjemaoversikt=skjemaoversikt)

  #################################################################################################################################
  ################ Abonnement ##################################################################################################

  # callModule(abonnement, "abonnement_id", reshID = reshID, userRole = userRole, hvd_session = session)

  #####################################################################################
  #####################################################################################
  #####################################################################################

  output$norgast_brukere <- renderUI({
  # req(input$file1)
    if (!is.null(input$file1)) {
      df <- read.csv2(input$file1$datapath, header = T)
      names(df) <- names(df) %>% trimws() %>% tolower()
      df$shus <- RegData$Sykehusnavn[match(df$resh, RegData$AvdRESH)]
      df$shus[is.na(df$shus)] <- 'Ukjent'
      testliste <- tapply(df$epost, df$shus, unique)
      selectInput(inputId = "brukere_epost", label = "Velg brukere som skal motta rapport",
                  choices = testliste, multiple = TRUE)
    }


  })


  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab_v2(session))


  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    rownames = FALSE, options = list(dom = 't')
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    fullName <- rapbase::getUserFullName(session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", fullName))
    } else {
      tagList(
        p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
                rapbase::getUserEmail(session), ":")),
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
      email <- input$brukere_epost
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
    paramValues <- c(baseName, input$valgtShus)

    rapbase::createAutoReport(synopsis = synopsis, package = 'norgast',
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear, interval = interval,
                              intervalName = intervalName)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab_v2(session)
  })

  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab_v2(session)
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
