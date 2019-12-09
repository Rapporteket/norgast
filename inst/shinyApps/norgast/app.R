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
library(lubridate)

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

#alternative til dateInput med mulighet til  bare år, måned og år ..
dateInput2 <- function(inputId, label, minview = "months", maxview = "years", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}


BrValg <- BrValgNorgastShiny(RegData)

source(system.file("shinyApps/norgast/R/modul_fordelingsfig_UI.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_fordelingsfig.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_sykehusvisning_UI.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_sykehusvisning.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_tidsvisning_UI.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_tidsvisning.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_datadump.R", package = "norgast"), encoding = 'UTF-8')
source(system.file("shinyApps/norgast/R/modul_samledok.R", package = "norgast"), encoding = 'UTF-8')

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
             h4(tags$b('Tidsvisning '), 'viser tidsutviklingen for valgt variabel for ditt sykehus'),
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
           samledok_UI(id = "samledok_id", BrValg = BrValg)
  ),

  tabPanel("Datadump",
           h2("Datadump", align='center'),
           h4("Velg variablene du ønsker inkludert i datadump og for hvilken tidsperiode.", align='center'),
           h4("Linn og Kristoffer lager mer tekst hvis ønskelig.", align='center'),
           br(),
           br(),
           datadump_UI(id = "datadump_id", BrValg = BrValg)
  ),

  tabPanel("Administrative tabeller",
           sidebarPanel(
             conditionalPanel(condition = "input.admtabeller == 'Antall skjema'",
             dateRangeInput(inputId="datovalg_adm", label = "Dato fra og til", min = '2014-01-01',
                            max = Sys.Date(), start  = Sys.Date() %m-% months(12), end = Sys.Date(), separator = " til ")
           ),

           conditionalPanel(condition = "input.admtabeller == 'Registreringer over tid'",
                            selectInput(inputId = "adm_tidsenhet", label = "Velg tidsenhet",
                                        choices = c('Måneder'=1, 'År'=2)),
                            conditionalPanel(condition = "input.adm_tidsenhet == '1'",
                                             dateInput2(inputId="datovalg_adm_tid_mnd", label = "Vis til og med måned: ", min = '2014-01-01',
                                                        max = Sys.Date(), value = Sys.Date(), minview = 'months', format = "MM yyyy", language="no"),
                                             sliderInput(inputId= "ant_mnd", label = "Antall måneder", min = 1, max = 24, value = 12, step = 1)),
                            conditionalPanel(condition = "input.adm_tidsenhet == '2'",
                                             dateInput2(inputId="datovalg_adm_tid_aar", label = "Vis til og med år: ", min = '2014-01-01',
                                                        max = Sys.Date(), value = Sys.Date(), minview = 'years', format = "yyyy", language="no"),
                                             sliderInput(inputId= "ant_aar", label = "Antall år", min = 1, max = 10, value = 5, step = 1)),
                            selectInput(inputId = "regstatus_tid", label = "Skjemastatus",
                                        choices = c('Ferdige forløp'=1, 'Oppfølging i kladd'=2, 'Ferdig basisreg. oppfølging mangler'=3,
                                                    'Basisreg. i kladd'=4))
           )
           ),
           mainPanel(tabsetPanel(id="admtabeller",
             tabPanel("Antall skjema",
                      h4(tags$b(tags$u('Denne tabellen gir en avdelingsvis oversikt over innregistreringer i NoRGast:'))),
                      h4(tags$b('Ferdige forløp '), 'viser antall forløp med ferdigstilt basisregistrering og oppfølging.'),
                      h4(tags$b('Oppfølging i kladd '), 'viser antall forløp med ferdigstilt basisregistrering og oppfølging i kladd.'),
                      h4(tags$b('Ferdig basisreg. oppfølging mangler '), 'viser antall forløp med ferdigstilt basisregistrering og ikke påbegynt eller slettet oppfølging'),
                      h4(tags$b('Basisreg. i kladd '), 'viser antallet basisregistreringer i kladd.'),
                      br(),
                      br(),
                      DTOutput("Tabell_adm1"), downloadButton("lastNed_adm1", "Last ned tabell")),
             tabPanel("Registreringer over tid",
                      # h4("Her kommer det flere tabeller...", align='center'),
                      # textOutput("debug_tabell")
                      DTOutput("Tabell_adm2"), downloadButton("lastNed_adm2", "Last ned tabell")
                      # tableOutput("Tabell_adm2"), downloadButton("lastNed_adm2", "Last ned tabell")
                      )
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
  ################ Samledokumenter ##################################################################################################

  callModule(samledok, "samledok_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Datadump   ##################################################################################################

  callModule(datadump, "datadump_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session)

  #################################################################################################################################
  ################ Adm. tabeller ##################################################################################################

  antskjema <- function() {

    tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering', c("ForlopsID", "SkjemaStatus", "HovedDato", "OpprettetDato", "Sykehusnavn", "AvdRESH")],
                 skjemaoversikt[skjemaoversikt$Skjemanavn=='Oppfølging', c("ForlopsID", "SkjemaStatus")],
                 by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))

    tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
    tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
    tmp$HovedDato[is.na(tmp$HovedDato)] <- tmp$OpprettetDato[is.na(tmp$HovedDato)]

    aux <- tmp %>% filter(HovedDato >= input$datovalg_adm[1] & HovedDato <= input$datovalg_adm[2]) %>%
      group_by(Sykehusnavn) %>% summarise('Ferdige forløp' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==1, na.rm = T),
                                          'Oppfølging i kladd' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==0, na.rm = T),
                                          'Ferdig basisreg. oppfølging mangler' = sum(SkjemaStatus==1 & is.na(SkjemaStatus_oppf), na.rm = T),
                                          'Basisreg i kladd' = sum(SkjemaStatus==0, na.rm = T),
                                          'N' = n())
    aux2 <- tmp %>% filter(HovedDato >= input$datovalg_adm[1] & HovedDato <= input$datovalg_adm[2]) %>%
      summarise('Ferdige forløp' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==1, na.rm = T),
                'Oppfølging i kladd' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==0, na.rm = T),
                'Ferdig basisreg. oppfølging mangler' = sum(SkjemaStatus==1 & is.na(SkjemaStatus_oppf), na.rm = T),
                'Basisreg i kladd' = sum(SkjemaStatus==0, na.rm = T),
                'N' = n())

    ant_skjema <- bind_rows(aux, bind_cols(tibble(Sykehusnavn='Totalt'), aux2))

    sketch <- htmltools::withTags(table(
      tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)


  }

  output$Tabell_adm1 = renderDT(
    datatable(antskjema()$ant_skjema[-dim(antskjema()$ant_skjema)[1], ],
              container = antskjema()$sketch,
              rownames = F,
              options = list(pageLength = 40)
    )
  )


  output$lastNed_adm1 <- downloadHandler(
    filename = function(){
      paste0('Regoversikt', Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- antskjema()$ant_skjema
      write.csv2(TabellData, file, row.names = F)
    }
  )


  andre_adm_tab <- function() {

    if (input$adm_tidsenhet == 1) {

      fraDato <- as.Date(input$datovalg_adm_tid_mnd) %m-% months(input$ant_mnd) %>% floor_date(unit="months")
      tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering', c("ForlopsID", "SkjemaStatus", "HovedDato", "OpprettetDato", "Sykehusnavn", "AvdRESH")],
                   skjemaoversikt[skjemaoversikt$Skjemanavn=='Oppfølging', c("ForlopsID", "SkjemaStatus")],
                   by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))

      tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
      tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
      tmp$HovedDato[is.na(tmp$HovedDato)] <- as.Date(tmp$OpprettetDato[is.na(tmp$HovedDato)])

      aux <- tmp[tmp$HovedDato >= fraDato & tmp$HovedDato <= input$datovalg_adm_tid_mnd, ]

      aux$mnd <- factor(format(aux$HovedDato, format='%b-%y'), levels = format(seq(as.Date(fraDato),as.Date(input$datovalg_adm_tid_mnd), by="month"), "%b-%y"))

      ant_skjema <- switch (input$regstatus_tid,
                            '1' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==1) , c('Sykehusnavn', 'mnd')]))),
                            '2' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==0) , c('Sykehusnavn', 'mnd')]))),
                            '3' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & is.na(aux$SkjemaStatus_oppf)) , c('Sykehusnavn', 'mnd')]))),
                            '4' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==0) , c('Sykehusnavn', 'mnd')])))
      ) %>% as_tibble(rownames = 'Sykehusnavn')
    }

    if (input$adm_tidsenhet == 2) {

      fraDato <- as.Date(input$datovalg_adm_tid_aar) %m-% years(input$ant_aar) %>% floor_date(unit="years")
      tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering', c("ForlopsID", "SkjemaStatus", "HovedDato", "OpprettetDato", "Sykehusnavn", "AvdRESH")],
                   skjemaoversikt[skjemaoversikt$Skjemanavn=='Oppfølging', c("ForlopsID", "SkjemaStatus")],
                   by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))

      tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
      tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
      tmp$HovedDato[is.na(tmp$HovedDato)] <- as.Date(tmp$OpprettetDato[is.na(tmp$HovedDato)])

      aux <- tmp[tmp$HovedDato >= fraDato & tmp$HovedDato <= input$datovalg_adm_tid_aar, ]

      aux$mnd <- factor(format(aux$HovedDato, format='%Y'), levels = format(seq(as.Date(fraDato),as.Date(input$datovalg_adm_tid_aar), by="year"), "%Y"))

      ant_skjema <- switch (input$regstatus_tid,
                            '1' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==1) , c('Sykehusnavn', 'mnd')]))),
                            '2' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==0) , c('Sykehusnavn', 'mnd')]))),
                            '3' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & is.na(aux$SkjemaStatus_oppf)) , c('Sykehusnavn', 'mnd')]))),
                            '4' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==0) , c('Sykehusnavn', 'mnd')])))
      ) %>% as_tibble(rownames = 'Sykehusnavn')
    }

    sketch <- htmltools::withTags(table(
      tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)

  }

  output$Tabell_adm2 = renderDT(
    datatable(andre_adm_tab()$ant_skjema[-dim(andre_adm_tab()$ant_skjema)[1], ],
              container = andre_adm_tab()$sketch,
              rownames = F,
              options = list(pageLength = 40)
    )
  )

  output$lastNed_adm2 <- downloadHandler(
    filename = function(){
      paste0('Regoversikt_tid', Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- andre_adm_tab()$ant_skjema
      write.csv2(TabellData, file, row.names = F)
    }
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

