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

  tabPanel("Forside"#,
           # mainPanel(
           # )
  ),


  tabPanel("Testpanel",
           shinyjs::useShinyjs(),
           shinyalert::useShinyalert(),
           rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                        organization = uiOutput("appOrgName"),
                                        addUserInfo = TRUE),
           mainPanel(
             # return from rapbase functions
             h4("Test 'rapbase' functions using the session object:"),
             textOutput("callUser"),
             textOutput("callGroups"),
             textOutput("callReshId"),
             textOutput("callRole"),
             textOutput("callEmail"),
             textOutput("callFullName"),
             textOutput("callPhone"),
             h4("Environment var R_RAP_INSTANCE:"),
             textOutput("envInstance"),
             h4("Environmental var R_RAP_CONFIG_PATH:"),
             textOutput("envConfigPath"),
             h4("Locale settings:"),
             textOutput("locale")
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
           p("Når du velger ", strong("Last ned samledokument"), " genereres en samlerapport bestående av figurer og tabeller",
             align='center'),
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
                      downloadButton("lastNed_saml", "Last ned samledokument")),
             tabPanel("Samledokument med nasjonale tall",
                      downloadButton("lastNed_saml_land", "Last ned samledokument")),
             tabPanel("Kvartalsrapport for din avdeling",
                      downloadButton("lastNed_kvartal", "Last ned kvartalsrapport")))
           )
  ),
  tabPanel("Datadump",
           h2("Datadump", align='center'),
           p("Velg variablene du ønsker inkludert i datadump og for hvilken tidsperiode.", align='center'),
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
                      tableOutput("Tabell_adm2"), downloadButton("lastNed2", "Last ned tabell"))
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

  observe(
    if (userRole() != 'SC') {
      shinyjs::hide(id = 'valgtShus')
      shinyjs::hide(id = 'valgtShus2')
      shinyjs::hide(id = 'valgtShus3')
      shinyjs::hide(id = 'valgtShus4')
      hideTab(inputId = "tabs_andeler", target = "Figur, sykehusvisning")
      hideTab(inputId = "tabs_andeler", target = "Tabell, sykehusvisning")
      hideTab(inputId = "tabs_andeler", target = "Figur, andeler i stabel")
      hideTab(inputId = "tabs_andeler", target = "Tabell, andeler i stabel")
      hideTab(inputId = "tabs_andeler", target = "Figur, gjennomsnitt per sykehus")
      hideTab(inputId = "tabs_andeler", target = "Tabell, gjennomsnitt per sykehus")
    }
  )


  #################################################################################################################################
  ################ Fordelingsfigurer ##############################################################################################

  callModule(fordelingsfig, "fordelingsfig_id", reshID = reshID(), RegData = RegData, userRole = userRole())


  #################################################################################################################################
  ################ Andeler ########################################################################################################

  callModule(sykehusvisning, "sykehusvisning_id", reshID = reshID(), RegData = RegData)

  callModule(tidsvisning, "tidsvisning_id", reshID = reshID(), RegData = RegData, userRole = userRole())


  output$ncsp2 <- renderUI({selectInput(inputId = "ncsp_verdi2", label = "Velg NCSP kode(r)",
                                        choices = if (is.null(input$op_gruppe2)){
                                          ""
                                        } else {
                                          setNames(substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in%
                                                                                               as.numeric(input$op_gruppe2)])), 1, 5),
                                                   sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in%
                                                                                        as.numeric(input$op_gruppe2)])))
                                        },
                                        multiple = TRUE)})

  output$fig_andel_tid <- renderPlot({
    norgast::NorgastFigAndelTid(RegData, valgtVar=input$valgtVar_andel, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                reshID = reshID(), enhetsUtvalg=as.numeric(input$enhetsUtvalg2), minald=as.numeric(input$alder2[1]),
                                maxald=as.numeric(input$alder2[2]), valgtShus = if (!is.null(input$valgtShus2)) {input$valgtShus2} else {''},
                                op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''},
                                BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                tilgang = if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                ASA = if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                malign = as.numeric(input$malign2), erMann = as.numeric(input$erMann2),
                                elektiv = as.numeric(input$elektiv2),
                                tidsenhet = if (!is.null(input$tidsenhet)) {input$tidsenhet} else {'Aar'},
                                inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99})
  }, width = 700, height = 700)

  tabellReagerTid <- reactive({
    TabellData_Tid <- norgast::NorgastFigAndelTid(RegData, valgtVar=input$valgtVar_andel, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                                  reshID = reshID(), enhetsUtvalg=as.numeric(input$enhetsUtvalg2), minald=as.numeric(input$alder2[1]),
                                                  maxald=as.numeric(input$alder2[2]), valgtShus = if (!is.null(input$valgtShus2)) {input$valgtShus2} else {''},
                                                  op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                                  ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''},
                                                  BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                                  tilgang = if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                                  minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                                  ASA = if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                                  whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                                  forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                                  malign = as.numeric(input$malign2), erMann = as.numeric(input$erMann2),
                                                  elektiv = as.numeric(input$elektiv2),
                                                  tidsenhet = if (!is.null(input$tidsenhet)) {input$tidsenhet} else {'Aar'},
                                                  inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99})
  })

  output$utvalg_tid <- renderUI({
    TabellData <- tabellReagerTid()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})



  output$Tabell_tid <- function() {

    utdata <- tabellReagerTid()
    if (input$enhetsUtvalg2 == 1) {
      Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt, Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                           N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                           Konf.int.ovre = utdata$KonfInt$Konf[2,], Antall2 = round(utdata$Andeler$AndelRest*utdata$NTid$NTidRest/100),
                           N2 = utdata$NTid$NTidRest, Andel2 = utdata$Andeler$AndelRest, Konf.int.nedre2 = utdata$KonfInt$KonfRest[1,],
                           Konf.int.ovre2 = utdata$KonfInt$KonfRest[2,])
      names(Tabell_tid) <- c('Tidsperiode', 'Antall', 'N', 'Andel', 'Konf.int.nedre', 'Konf.int.ovre', 'Antall', 'N', 'Andel',
                             'Konf.int.nedre', 'Konf.int.ovre')
      Tabell_tid %>% knitr::kable("html", digits = c(0,0,0,1,1,1,0,0,1,1,1)) %>%
        kable_styling("hover", full_width = F) %>%
        add_header_above(c(" ", "Din avdeling" = 5, "Landet forøvrig" = 5))
    } else {
      Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt,
                           Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                           N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                           Konf.int.ovre = utdata$KonfInt$Konf[2,])
      Tabell_tid %>%
        knitr::kable("html", digits = c(0,0,0,1,1,1)) %>%
        kable_styling("hover", full_width = F)
    }
  }

  output$lastNed_tid <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_andel, '_tid', Sys.time(), '.csv')
    },
    content = function(file){
      utdata <- tabellReagerTid()
      if (input$enhetsUtvalg2 == 1) {
        Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt, Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                             N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                             Konf.int.ovre = utdata$KonfInt$Konf[2,], Antall2 = round(utdata$Andeler$AndelRest*utdata$NTid$NTidRest/100),
                             N2 = utdata$NTid$NTidRest, Andel2 = utdata$Andeler$AndelRest, Konf.int.nedre2 = utdata$KonfInt$KonfRest[1,],
                             Konf.int.ovre2 = utdata$KonfInt$KonfRest[2,])
      } else {
        Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt,
                             Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                             N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                             Konf.int.ovre = utdata$KonfInt$Konf[2,])
      }
      write.csv2(Tabell_tid, file, row.names = F)
    }
  )

  output$lastNedBilde_tid <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_andel, '_tid', Sys.time(), '.', input$bildeformat2)
    },
    content = function(file){
      norgast::NorgastFigAndelTid(RegData, valgtVar=input$valgtVar_andel, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                  reshID = reshID(), enhetsUtvalg=as.numeric(input$enhetsUtvalg2), minald=as.numeric(input$alder2[1]),
                                  maxald=as.numeric(input$alder2[2]), valgtShus = if (!is.null(input$valgtShus2)) {input$valgtShus2} else {''},
                                  op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                  ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''},
                                  BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                  tilgang = if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                  minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                  ASA = if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                  whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                  forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                  malign = as.numeric(input$malign2), erMann = as.numeric(input$erMann2),
                                  elektiv = as.numeric(input$elektiv2),
                                  tidsenhet = if (!is.null(input$tidsenhet)) {input$tidsenhet} else {'Aar'},
                                  inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99}, outfile = file)
    }
  )

  output$fig_andel_grvar <- renderPlot({
    norgast::NorgastFigAndelerGrVar(RegData, valgtVar=input$valgtVar_andel, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                    minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]), erMann=as.numeric(input$erMann2),
                                    inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99},
                                    malign = as.numeric(input$malign2), Ngrense=10,
                                    elektiv=as.numeric(input$elektiv2),BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                    tilgang=if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                    minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                    ASA=if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                    whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                    forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                    op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                    ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''})
    # valgtShus = if (!is.null(input$valgtShus2)) {input$valgtShus2} else {''})
  }, width = 700, height = 700)

  tabellReagerSykehusAndel <- reactive({
    TabellData <- norgast::NorgastFigAndelerGrVar(RegData, valgtVar=input$valgtVar_andel, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                                  minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]), erMann=as.numeric(input$erMann2),
                                                  inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99},
                                                  malign = as.numeric(input$malign2), Ngrense=10,
                                                  elektiv=as.numeric(input$elektiv2),BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                                  tilgang=if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                                  minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                                  ASA=if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                                  whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                                  forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                                  op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                                  ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''})
  })

  output$utvalg_sykehus_andel <- renderUI({
    TabellData <- tabellReagerSykehusAndel()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  output$Tabell_sykehus_andel <- function() {
    utdata <- tabellReagerSykehusAndel()
    Tabell <- tibble(Avdeling = names(utdata$Nvar), Antall=utdata$Nvar, N=utdata$Ngr, Andel = as.numeric(utdata$Nvar/utdata$Ngr*100),
                     KI_nedre=utdata$KI[1,], KI_ovre=utdata$KI[2,])
    Tabell[utdata$Andeler==-0.001, 2:6] <- NA
    Tabell <- Tabell[dim(Tabell)[1]:1, ]
    Tabell %>% knitr::kable("html", digits = c(0,0,0,1,1,1)) %>%
      kable_styling("hover", full_width = F)
  }

  output$lastNed_sykehus_andel <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_andel, '_sykehus_andel_', Sys.time(), '.csv')
    },
    content = function(file){
      utdata <- tabellReagerSykehusAndel()
      Tabell <- tibble(Avdeling = names(utdata$Nvar), Antall=utdata$Nvar, N=utdata$Ngr, Andel = as.numeric(utdata$Nvar/utdata$Ngr*100),
                       KI_nedre=utdata$KI[1,], KI_ovre=utdata$KI[2,])
      Tabell[utdata$Andeler==-0.001, 2:6] <- NA
      Tabell <- Tabell[dim(Tabell)[1]:1, ]
      write.csv2(Tabell, file, row.names = F, na = '')
    }
  )

  output$lastNedBilde_sykehus_andel <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_andel, '_sykehus_andel_', Sys.time(), '.', input$bildeformat2)
    },
    content = function(file){
      norgast::NorgastFigAndelerGrVar(RegData, valgtVar=input$valgtVar_andel, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                      minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]), erMann=as.numeric(input$erMann2),
                                      inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99},
                                      malign = as.numeric(input$malign2), Ngrense=10,
                                      elektiv=as.numeric(input$elektiv2),BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                      tilgang=if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                      minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                      ASA=if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                      whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                      forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                      op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                      ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''}, outfile = file)
    }
  )



  output$fig_gjsn_grvar <- renderPlot({
    norgast::NorgastFigGjsnGrVar(RegData, valgtVar=input$valgtVar_gjsn, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                 minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]), erMann=as.numeric(input$erMann2),
                                 # inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99},
                                 malign = as.numeric(input$malign2), Ngrense=10,
                                 elektiv=as.numeric(input$elektiv2),BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                 tilgang=if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                 minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                 ASA=if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                 whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                 forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                 op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                 ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''})
    # valgtShus = if (!is.null(input$valgtShus2)) {input$valgtShus2} else {''})
  }, width = 700, height = 700)

  tabellReagerSykehusGjsn <- reactive({
    TabellData <- norgast::NorgastFigGjsnGrVar(RegData, valgtVar=input$valgtVar_gjsn, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                               minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]), erMann=as.numeric(input$erMann2),
                                               # inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99},
                                               malign = as.numeric(input$malign2), Ngrense=10,
                                               elektiv=as.numeric(input$elektiv2),BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                               tilgang=if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                               minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                               ASA=if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                               whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                               forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                               op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                               ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''})
  })

  output$utvalg_sykehus_gjsn <- renderUI({
    TabellData <- tabellReagerSykehusGjsn()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  output$Tabell_sykehus_gjsn <- function() {
    utdata <- tabellReagerSykehusGjsn()
    Tabell <- as_tibble(utdata$res, rownames='Sykehusnavn')
    Tabell[Tabell$N < 10, 2:4] <- NA
    Tabell <- Tabell[order(Tabell$Gjsn, decreasing = T, na.last = T), ]
    Tabell %>% knitr::kable("html", digits = c(0,1,1,1,0)) %>%
      kable_styling("hover", full_width = F)
  }

  output$lastNed_sykehus_gjsn <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_gjsn, '_sykehus_gjsn_', Sys.time(), '.csv')
    },
    content = function(file){
      utdata <- tabellReagerSykehusGjsn()
      Tabell <- as_tibble(utdata$res, rownames='Sykehusnavn')
      Tabell[Tabell$N < 10, 2:4] <- NA
      Tabell <- Tabell[order(Tabell$Gjsn, decreasing = T, na.last = T), ]
      write.csv2(Tabell, file, row.names = F, na = '')
    }
  )

  output$lastNedBilde_sykehus_gjsn <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_gjsn, '_sykehus_gjsn_', Sys.time(), '.', input$bildeformat2)
    },
    content = function(file){
      norgast::NorgastFigGjsnGrVar(RegData, valgtVar=input$valgtVar_gjsn, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                   minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]), erMann=as.numeric(input$erMann2),
                                   # inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99},
                                   malign = as.numeric(input$malign2), Ngrense=10,
                                   elektiv=as.numeric(input$elektiv2),BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                   tilgang=if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                   minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                   ASA=if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                   whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                   forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                   op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                   ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''}, outfile = file)
    }
  )

  output$fig_andel_grvar_stabel <- renderPlot({
    norgast::NorgastFigAndelStabelGrVar(RegData, valgtVar=input$valgtVar_andel_stabel, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                        minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]), erMann=as.numeric(input$erMann2),
                                        malign = as.numeric(input$malign2), Ngrense=10,
                                        elektiv=as.numeric(input$elektiv2),BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                        tilgang=if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                        minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                        ASA=if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                        whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                        forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                        op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                        ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''})
  }, width = 700, height = 700)

  tabellReagerSykehusAndelStabel <- reactive({
    TabellData <- norgast::NorgastFigAndelStabelGrVar(RegData, valgtVar=input$valgtVar_andel_stabel, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                                      minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]), erMann=as.numeric(input$erMann2),
                                                      malign = as.numeric(input$malign2), Ngrense=10,
                                                      elektiv=as.numeric(input$elektiv2),BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                                      tilgang=if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                                      minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                                      ASA=if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                                      whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                                      forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                                      op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                                      ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''})
  })

  output$utvalg_sykehus_andel_stabel <- renderUI({
    TabellData <- tabellReagerSykehusAndelStabel()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  output$Tabell_sykehus_andel_stabel <- function() {
    TabellData <- tabellReagerSykehusAndelStabel()
    Tabell <- bind_cols(TabellData$antall, TabellData$andeler[, 2:(dim(TabellData$andeler)[2]-1)])
    names(Tabell)[(dim(Tabell)[2]/2 + 2):dim(Tabell)[2]] <- names(Tabell)[2:(dim(Tabell)[2]/2)]
    names(Tabell)[dim(Tabell)[2]/2 + 1] <- 'N'

    Tabell %>% knitr::kable("html", digits = c(rep(0, dim(Tabell)[2]/2 + 1), rep(1, dim(Tabell)[2]/2 - 1))) %>%
      kable_styling("hover", full_width = F) %>%
      add_header_above(c(" ", "Antall" = dim(Tabell)[2]/2 - 1, " ", "Andel (%)" = dim(Tabell)[2]/2 - 1))

  }



  output$lastNedBilde_sykehus_andel_stabel <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_andel_stabel, '_stabel', Sys.time(), '.', input$bildeformat2)
    },
    content = function(file){
      norgast::NorgastFigAndelStabelGrVar(RegData, valgtVar=input$valgtVar_andel_stabel, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                                          minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]), erMann=as.numeric(input$erMann2),
                                          malign = as.numeric(input$malign2), Ngrense=10,
                                          elektiv=as.numeric(input$elektiv2),BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                                          tilgang=if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                                          minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                                          ASA=if (!is.null(input$ASA2)) {input$ASA2} else {''},
                                          whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                                          forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                                          op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                          ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''}, outfile = file)
    }
  )






  # (RegData=0, valgtVar='', datoFra='2014-01-01', datoTil='2050-12-31',
  # minald=0, maxald=130, erMann=99, outfile='',
  # reshID, preprosess=F, inkl_konf=F, malign=99, Ngrense=10,
  # elektiv=99, BMI='', tilgang='', valgtShus=c(''), minPRS=0,
  # maxPRS=2.2, ASA='', whoEcog= '', forbehandling='', hentData=0, op_gruppe='', ncsp='')

  # output$fig_andel_grvar_tid <- renderPlot({
  #   norgast::norgastFigAndelGrVarTid(RegData, valgtVar=input$valgtVar_andel, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2])
  # }, width = 700, height = 700)

  # norgastFigAndelGrVarTid(RegData, valgtVar, tittel='', width=800, height=700, sideTxt='Boområde/opptaksområde',
  #                         decreasing=F, terskel=30, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4, legPlass='top',
  #                         minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, inkl_konf=F, datoFra='2014-01-01', datoTil='2050-12-31',
  #                         minald=0, maxald=130, erMann=99, outfile='', preprosess=F, malign=99, elektiv=99, BMI='',
  #                         tilgang='', minPRS=0, maxPRS=2.2, ASA='', whoEcog= '', forbehandling='',
  #                         hentData=0, op_gruppe='', ncsp='')

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

