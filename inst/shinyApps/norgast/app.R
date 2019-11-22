#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

######## Last data ########################################
library(norgast)
library(tidyverse)
library(kableExtra)
library(DT)
library(shiny)
library(shinyjs)
library(shinyalert)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "NoRGast"
logo <- includeHTML(system.file('www/logo.svg', package='rapbase'))
logoCode <- paste0("var header = $('.navbar> .container-fluid');\n",
                   "header.append('<div class=\"navbar-brand\" style=\"float:left;font-size:75%\">",
                   logo,
                   "</div>');\n",
                   "console.log(header)")
logoWidget <- tags$script(shiny::HTML(logoCode))


context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
onServer <- context == "TEST" | context == "QA" | context == "PRODUCTION" | context == "DEV"
if (onServer) {
  RegData <- NorgastHentRegData()
  skjemaoversikt <- NorgastHentSkjemaOversikt()
} else {
  RegData <- read.table('I:/norgast/AlleVarNum2019-11-19 10-04-46.txt', header=TRUE, sep=";",
                        encoding = 'UTF-8', stringsAsFactors = F)
  ForlopData <- read.table('I:/norgast/ForlopsOversikt2019-11-19 10-05-04.txt', header=TRUE, sep=";",
                           encoding = 'UTF-8', stringsAsFactors = F)

  RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                        'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                        'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                        'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                        'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose', "Hastegrad")]
  ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
  RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")

  skjemaoversikt <- read.table('I:/norgast/SkjemaOversikt2019-11-19 10-05-09.txt', header=TRUE, sep=';', stringsAsFactors = F, encoding = 'UTF-8')
}

skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)

RegData <- NorgastPreprosess(RegData)
RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)

BrValg <- BrValgNorgastShiny(RegData)

source(system.file("shinyApps/norgast/R/modul_fordelingsfig_UI.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_fordelingsfig.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_sykehusvisning_UI.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_sykehusvisning.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_tidsvisning_UI.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_tidsvisning.R", package = "norgast"), encoding = 'UTF-8')

######################################################################

# Define UI for application
ui <- navbarPage(

  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              regTitle),
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",

  tabPanel("Startside",
           mainPanel(
                      shinyjs::useShinyjs(),
                      shinyalert::useShinyalert(),
                      rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                                   organization = uiOutput("appOrgName"),
                                                   addUserInfo = TRUE),

             h2('Velkommen til Rapporteket - NoRGast', align='center'),
             br(),
             h4(tags$b('Her skal Linn og Kristoffer formulere kloke og reflekterte meldinger til Rapportekets brukere. En foreløpig variant er gitt under:')),
             br(),
             h4('Du er nå inne på Rapporteket for NoRGast, registerets resultattjeneste.
                Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret.
                På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert
                på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database.
                Dette medfører at nyere data ikke er kvalitetssikret ennå.'),
             h4('Du kan se på resultater for eget sykehus, nasjonale data og eget sykehus sett opp mot landet for øvrig.
                Hvis ikke annet oppgis så gjøres alle datovalg basert på operasjonsdato. Alle figurer og
                tabeller kan lastes ned.'),
             br(),
             h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
             h4(tags$b('Fordelinger '), 'viser fordelinger (figur/tabell) av ulike variabler.
                Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer.'),
             h4(tags$b('Sykehusvisning '), 'viser resultater per sykehus.
                Man kan velge hvilken variabel man vil se på og om man vil se gjennomsnitt, andeler eller stablede andeler.'),
             h4(tags$b('Tidsvisning '), 'viser tidsutviklingen for en andel for ditt sykehus'),
             h4(tags$b('Samledokumenter '), 'genererer ulike dokumenter som består av utvalgte figurer og tabeller.'),
             h4(tags$b('Datadump '), 'gir mulighet til å laste ned din egen avdelings registreringer. Man kan velge hvilke
                variabler man vil inkludere og for hvilket tidsrom og hvilke reseksjonsgrupper.'),
             h4(tags$b('Administrative tabeller '), 'er en samling oversikter over antall registreringer og annet snacks for den registerinteresserte.'),
             br(),
             br(),
             h3('HER KAN MAN F.EKS. VISE ANTALL REGISTRERINGER SISTE X MND.'),
             br(),
             br(),
             h4('Oversikt over registerets kvalitetsindikatorer og resultater finner du på www.kvalitetsregistre.no:', #helpText
                a("NoRGast", href="https://www.kvalitetsregistre.no/registers/545/resultater"),
                target="_blank", align='center'),
             br(),
             h4('Mer informasjon om registeret finnes på NoRGast sin hjemmeside: ', align='center',
                a("www.norgast.no", href="http://www.norgast.no", target="_blank"))
             )

  ),


  # tabPanel("Testpanel",
  #          shinyjs::useShinyjs(),
  #          shinyalert::useShinyalert(),
  #          rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
  #                                       organization = uiOutput("appOrgName"),
  #                                       addUserInfo = TRUE),
  #          mainPanel(
  #            # return from rapbase functions
  #            h4("Test 'rapbase' functions using the session object:"),
  #            textOutput("callUser"),
  #            textOutput("callGroups"),
  #            textOutput("callReshId"),
  #            textOutput("callRole"),
  #            textOutput("callEmail"),
  #            textOutput("callFullName"),
  #            textOutput("callPhone"),
  #            h4("Environment var R_RAP_INSTANCE:"),
  #            textOutput("envInstance"),
  #            h4("Environmental var R_RAP_CONFIG_PATH:"),
  #            textOutput("envConfigPath"),
  #            h4("Locale settings:"),
  #            textOutput("locale")
  #          )
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

  tabPanel("Samledokumenter",
           h2("Samledokumenter", align='center'),
           h4("Når du velger ", strong("Last ned samledokument"), " genereres en samlerapport bestående av figurer og tabeller.", align='center'),
              h4("Nærmere beskrivelse av de ulike samledokumentene finner du under de tilhørende fanene.", align='center'),
           br(),
           br(),
           sidebarPanel(
             dateRangeInput(inputId="datovalg_sml", label = "Dato fra og til", min = '2014-01-01',
                            max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), separator = " til "),
             selectInput(inputId = "valgtShus3", label = "Velg sykehus",
                         choices = BrValg$sykehus, multiple = TRUE)
           ),
           mainPanel(tabsetPanel(
             tabPanel("Samledokument med egen avd. mot landet forøvrig",
                      h4("Kristoffer og Linn lager en tekst som skal inn her."),
                      downloadButton("lastNed_saml", "Last ned samledokument")),
             tabPanel("Samledokument med nasjonale tall",
                      h4("Kristoffer og Linn lager en tekst som skal inn her."),
                      downloadButton("lastNed_saml_land", "Last ned samledokument")),
             tabPanel("Kvartalsrapport for din avdeling",
                      h4("Kristoffer og Linn lager en tekst som skal inn her."),
                      downloadButton("lastNed_kvartal", "Last ned kvartalsrapport")))
           )
  ),
  tabPanel("Datadump",
           h2("Datadump", align='center'),
           h4("Velg variablene du ønsker inkludert i datadump og for hvilken tidsperiode.", align='center'),
           h4("Linn og Kristoffer lager mer tekst hvis ønskelig.", align='center'),
           br(),
           br(),
           sidebarPanel(
             selectInput(inputId = "valgtevar_dump", label = "Velg variabler å inkludere (ingen valgt er lik alle)",
                         choices = names(RegData), multiple = TRUE),
             dateRangeInput(inputId="datovalg_dump", label = "Dato fra og til", min = '2014-01-01',
                            max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), separator = " til "),
             selectInput(inputId = "op_gruppe_dump", label = "Velg reseksjonsgruppe(r)",
                         choices = BrValg$reseksjonsgrupper, multiple = TRUE),
             selectInput(inputId = "valgtShus4", label = "Velg sykehus",
                         choices = BrValg$sykehus, multiple = TRUE)
           ),
           downloadButton("lastNed_dump", "Last ned datadump")
  ),
  tabPanel("Administrative tabeller",
           sidebarPanel(
             dateRangeInput(inputId="datovalg_adm", label = "Dato fra og til", min = '2014-01-01',
                            max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), separator = " til "),
             selectInput(inputId = "regstatus", label = "Skjemastatus",
                         choices = c('Ferdigstilt'=1, 'Kladd'=0))
           ),
           mainPanel(tabsetPanel(
             tabPanel("Antall skjema",
                      DTOutput("Tabell_adm1"), downloadButton("lastNed1", "Last ned tabell")),
             tabPanel("Annen admin rapport",
                      h4("Her kommer det flere tabeller...", align='center')
                      # tableOutput("Tabell_adm2"), downloadButton("lastNed2", "Last ned tabell")
                      )
           )
           )
  )
)


#
server <- function(input, output, session) {

  reshID <- reactive({
    ifelse(onServer, as.numeric(rapbase::getUserReshId(session)), 601225)
  })
  userRole <- reactive({
    ifelse(onServer, rapbase::getUserRole(session), 'SC')
  })

  if (rapbase::isRapContext()) {
    reshId_2 <- rapbase::getUserReshId(session)
    } else {
      reshId_2 <- 601225
    }

  observe(
    if (userRole() != 'SC') {
      shinyjs::hide(id = 'valgtShus3')
      shinyjs::hide(id = 'valgtShus4')
    }
  )


  #################################################################################################################################
  ################ Fordelingsfigurer ##############################################################################################

  callModule(fordelingsfig, "fordelingsfig_id", reshID = reshId_2, RegData = RegData, userRole = userRole())


  #################################################################################################################################
  ################ Sykehusvisning ########################################################################################################

  callModule(sykehusvisning, "sykehusvisning_id", reshID = reshID(), RegData = RegData)

  #################################################################################################################################
  ################ Tidsvisning ########################################################################################################

  callModule(tidsvisning, "tidsvisning_id", reshID = reshID(), RegData = RegData, userRole = userRole())

  #################################################################################################################################
  ################ Samledokumenter ##################################################################################################
  # render file function for re-use
  contentFile <- function(file, srcFile, tmpFile, datoFra, datoTil, reshID=0, valgtShus='') {
    src <- normalizePath(system.file(srcFile, package="norgast"))

    # temporarily switch to the temp dir, in case we do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFile, overwrite = TRUE)

    texfil <- knitr::knit(tmpFile, encoding = 'UTF-8')
    tools::texi2pdf(texfil, clean = TRUE)

    gc()
    file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
    # file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
  }

  output$lastNed_saml <- downloadHandler(
    filename = function(){
      paste0('samleDok', Sys.time(), '.pdf')
    },

    content = function(file){
      contentFile(file, "NorgastSamleDokShiny.Rnw", "tmpNorgastSamle.Rnw", input$datovalg_sml[1],
                  input$datovalg_sml[2], reshID=reshID(),
                  valgtShus=if (!is.null(input$valgtShus3)) {input$valgtShus3} else {''})
    }
  )

  output$lastNed_saml_land <- downloadHandler(
    filename = function(){
      paste0('samleDokLandet', Sys.time(), '.pdf')
    },

    content = function(file){
      contentFile(file, "NorgastSamleDokLandetShiny.Rnw", "tmpNorgastSamleLandet.Rnw", input$datovalg_sml[1],
                  input$datovalg_sml[2], reshID=reshID())
    }
  )

  output$lastNed_kvartal <- downloadHandler(
    filename = function(){
      paste0('Kvartalsrapp', RegData$Sykehusnavn[match(reshID(), RegData$AvdRESH)], Sys.time(), '.pdf')
    },

    content = function(file){
      contentFile(file, "NorgastKvartalsrapportShiny.Rnw", "tmpNorgastKvartalsrapportShiny.Rnw", datoFra=input$datovalg_sml[1],
                  datoTil=input$datovalg_sml[2], reshID=reshID(),
                  valgtShus=if (!is.null(input$valgtShus3)) {input$valgtShus3} else {''})
    }
  )




  #################################################################################################################################
  ################ Datadump   ##################################################################################################

  output$lastNed_dump <- downloadHandler(
    filename = function(){
      paste0('Datadump_NoRGast', Sys.time(), '.csv')
    },
    content = function(file){
      dumpdata <- RegData[RegData$HovedDato >= input$datovalg_dump[1] &
                            RegData$HovedDato <= input$datovalg_dump[2], ]
      if (userRole() != 'SC') {
        dumpdata <- dumpdata[dumpdata$AvdRESH == reshID(), ]
      } else {
        if (!is.null(input$valgtShus4)) {dumpdata <- dumpdata[dumpdata$AvdRESH %in% as.numeric(input$valgtShus4), ]}
      }

      if (!is.null(input$op_gruppe_dump)) {dumpdata <- dumpdata[which(dumpdata$Op_gr %in% as.numeric(input$op_gruppe_dump)), ]}
      if (!is.null(input$valgtevar_dump)) {dumpdata <- dumpdata[, input$valgtevar_dump]}

      write.csv2(dumpdata, file, row.names = F, na = '')
    }
  )


  #################################################################################################################################
  ################ Adm. tabeller ##################################################################################################

  antskjema <- function() {
    aux <- as.data.frame.matrix(addmargins(table(skjemaoversikt[skjemaoversikt$SkjemaStatus == as.numeric(input$regstatus) &
                                                                  skjemaoversikt$HovedDato >= input$datovalg_adm[1] &
                                                                  skjemaoversikt$HovedDato <= input$datovalg_adm[2],
                                                                c("Sykehusnavn", "Skjemanavn")], useNA = 'ifany')))
    aux$Avdeling <- row.names(aux)
    ant_skjema <- aux[, c(dim(aux)[2], 1:(dim(aux)[2]-1))]
    sketch <- htmltools::withTags(table(
      tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)
  }

  output$Tabell_adm1 = renderDT(
    datatable(antskjema()$ant_skjema[-dim(antskjema()$ant_skjema)[1], ],
              container = antskjema()$sketch,
              rownames = F,
              options = list(pageLength = 25)
    )
  )


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

