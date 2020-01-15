
######## Last data ########################################
library(norgast)
library(tidyverse)
library(kableExtra)
library(DT)
library(shiny)
library(shinyjs)
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
                  startsideUI("startside")
  ),

  # tabPanel("Startside",
  #          mainPanel(
  #                     shinyjs::useShinyjs(),
  #                     shinyalert::useShinyalert(),
  #                     rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
  #                                                  organization = uiOutput("appOrgName"),
  #                                                  addUserInfo = TRUE),
  #
  #            h2('Velkommen til Rapporteket - NoRGast', align='center'),
  #            br(),
  #            # h4(tags$b('Her skal Linn og Kristoffer formulere kloke og reflekterte meldinger til Rapportekets brukere. En foreløpig variant er gitt under:')),
  #            # br(),
  #            h4('Du er nå inne på Rapporteket for NoRGast, registerets resultattjeneste.
  #               Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret.
  #               På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert
  #               på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database.
  #               Dette medfører at nyere data ikke er kvalitetssikret ennå.'),
  #            h4('Du kan se på resultater for eget sykehus, nasjonale data og eget sykehus sett opp mot landet for øvrig.
  #               Hvis ikke annet oppgis så gjøres alle datovalg basert på operasjonsdato. Alle figurer og
  #               tabeller kan lastes ned.'),
  #            br(),
  #            h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
  #            h4(tags$b('Fordelinger '), 'viser fordelinger (figur/tabell) av ulike variabler.
  #               Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer.'),
  #            h4(tags$b('Sykehusvisning '), 'viser resultater per sykehus.
  #               Man kan velge hvilken variabel man vil se på og om man vil se gjennomsnitt, andeler eller stablede andeler.'),
  #            h4(tags$b('Tidsvisning '), 'viser tidsutviklingen for valgt variabel for ditt sykehus'),
  #            h4(tags$b('Samledokumenter '), 'genererer ulike dokumenter som består av utvalgte figurer og tabeller.'),
  #            h4(tags$b('Datadump '), 'gir mulighet til å laste ned din egen avdelings registreringer. Man kan velge hvilke
  #               variabler man vil inkludere og for hvilket tidsrom og hvilke reseksjonsgrupper.'),
  #            h4(tags$b('Administrative tabeller '), 'er en samling oversikter over antall registreringer.'),
  #            br(),
  #            # br(),
  #            # h3('HER KAN MAN F.EKS. VISE ANTALL REGISTRERINGER SISTE X MND.'),
  #            # br(),
  #            br(),
  #            h4('Oversikt over registerets kvalitetsindikatorer og resultater finner du på www.kvalitetsregistre.no:', #helpText
  #               a("NoRGast", href="https://www.kvalitetsregistre.no/registers/545/resultater"),
  #               target="_blank", align='center'),
  #            br(),
  #            h4('Mer informasjon om registeret finnes på NoRGast sin hjemmeside: ', align='center',
  #               a("www.norgast.no", href="http://www.norgast.no", target="_blank"))
  #            )
  #
  # ),

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
