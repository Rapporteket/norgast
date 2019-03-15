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

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
onServer <- context == "TEST" | context == "QA" | context == "PRODUCTION" | context == "DEV"
if (onServer) {
  RegData <- NorgastHentRegData()
  skjemaoversikt <- NorgastHentSkjemaOversikt()
} else {
  # rm(list = ls())
  # RegData <- read.table('I:/norgast/AlleVariablerNum2018-11-14 14-30-58.txt', header=TRUE, sep=";",
  #                       encoding = 'UFT-8', stringsAsFactors = F)
  RegData <- read.table('I:/norgast/AlleVarNum2019-02-12 10-51-03.txt', header=TRUE, sep=";",
                        encoding = 'UFT-8', stringsAsFactors = F)
  ForlopData <- read.table('I:/norgast/ForlopsOversikt2019-02-12 10-51-16.txt', header=TRUE, sep=";",
                           encoding = 'UFT-8', stringsAsFactors = F)

  RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                        'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                        'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                        'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                        'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]
  # , "Hastegrad")]
  ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
  RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")

  skjemaoversikt <- read.table('I:/norgast/SkjemaOversikt2019-02-12 10-51-20.txt', header=TRUE, sep=';', stringsAsFactors = F)
}
skjemaoversikt$Sykehusnavn <- iconv(skjemaoversikt$Sykehusnavn, from = 'UTF-8', to = '')
skjemaoversikt$Skjemanavn <- iconv(skjemaoversikt$Skjemanavn, from = 'UTF-8', to = '')
skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)

RegData <- NorgastPreprosess(RegData)
RegData$Sykehusnavn <- iconv(RegData$Sykehusnavn, from = 'UTF-8', to = '')  # Fiks lokale encoding issues
RegData$Sykehusnavn[RegData$AvdRESH==700413] <- 'OUS' # Navn på OUS fikses
RegData$Sykehusnavn <- trimws(RegData$Sykehusnavn)

varvalg <- c('Alder', 'BMI_kodet', 'Vektendring', 'Op_gr', 'AccordionGrad', 'Forbehandling',
             'WHOECOG', 'ASA', 'Hastegrad', 'erMann', 'MedDiabetes', 'PRSScore', 'Robotassistanse',
             'Tilgang', 'NyAnastomose', 'ModGlasgowScore', 'ReLapNarkose', 'Anastomoselekkasje',
             'Avdod', 'Saarruptur')
names(varvalg) <- c('Alder', 'BMI', 'Vektendring', 'Operasjonsgrupper', 'Komplikasjoner', 'Forbehandling',
                    'WHO-ECOG', 'ASA-grad', 'Elektiv kirurgi', 'Kjønn', 'Diabetes', 'mE-PASS', 'Robotassistanse',
                    'Tilgang i abdomen', 'Ny anastomose', 'Glasgow score', 'Relaparotomi', 'Anastomoselekkasje',
                    'Andel avdøde', 'Sårruptur')

aux<-c('Robotassistanse', 'Robotassistanse', 'Tilgang i abdomen', 'LapTilgang', 'Operert i normalarbeidstid', 'Hastegrad_tid',
       'Ny anastomose', 'NyAnastomose', 'Relaparotomi', 'ReLapNarkose', 'Anastomoselekkasje', 'Anastomoselekkasje',
       'Andel avdøde', 'Avdod', 'Kummulativ accordion score', 'KumAcc', 'Andel maligne', 'Malign', 'Sårruptur',
       'Saarruptur')
varvalg_andel <- aux[seq(2,length(aux), by = 2)]
names(varvalg_andel) <- aux[seq(1,length(aux), by = 2)]

varvalg_andel_stabel <- setNames(c('ModGlasgowScore', 'AccordionGrad', 'Tilgang'),
                                 c('Modified Glasgow Score', 'Komplikasjoner', 'Tilgang i abdomen'))
varvalg_gjsn <- setNames(c('BMI', 'VekttapProsent', 'ModGlasgowScore', 'Alder', 'PRSScore'),
                         c('BMI', 'Vekttap i prosent', 'Modified Glasgow Score', 'Alder', 'mE-PASS'))

reseksjonsgrupper <- sort(unique(RegData$Op_gr))
names(reseksjonsgrupper) <- RegData$Operasjonsgrupper[match(reseksjonsgrupper, RegData$Op_gr)]

sykehus <- RegData$AvdRESH[match(sort(unique(RegData$Sykehusnavn)), RegData$Sykehusnavn)]
names(sykehus) <- sort(unique(RegData$Sykehusnavn))

bmi_valg <- 1:8
names(bmi_valg) <- levels(RegData$BMI_kategori)
tilgang_valg <- c(1,2,3,5)
names(tilgang_valg) <- c('Åpen', 'Laparoskopisk', 'Konvertert', 'Endoskopisk')
ASA_valg <- 1:5
names(ASA_valg) <- c('Grad I', 'Grad II', 'Grad III', 'Grad IV', 'Grad V')
whoEcog_valg <- c(0:4, 9)
names(whoEcog_valg) <- c('0: Fullt aktiv', '1: Lett husarbeid og sittende arbeid', '2: Oppe > 50% av dagen, selvstelt',
                         '3: Oppe < 50% av dagen, delvis selvstelt', '4: Kun i stol/seng, hjelp til alt stell', 'Ukjent')



######################################################################

# Define UI for application
ui <- navbarPage(title = "RAPPORTEKET NORGAST", theme = "bootstrap.css",
                 tabPanel("Fordelingsfigurer",
                          # sidebarLayout(
                          sidebarPanel(
                            shinyjs::useShinyjs(),
                            selectInput(inputId = "valgtVar", label = "Velg variabel",
                                        choices = varvalg),
                            dateRangeInput(inputId="datovalg", label = "Dato fra og til", min = '2014-01-01',
                                           max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), language = "nb", separator = " til "),
                            selectInput(inputId = "enhetsUtvalg", label = "Kjør rapport for",
                                        choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1, 'Egen avd.'=2)),
                            selectInput(inputId = "valgtShus", label = "Velg sykehus",
                                        choices = sykehus, multiple = TRUE),
                            sliderInput(inputId="alder", label = "Alder", min = 0,
                                        max = 120, value = c(0, 120)),
                            selectInput(inputId = "erMann", label = "Kjønn",
                                        choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
                            shinyjs::hidden(
                              div(id = "avansert",
                                selectInput(inputId = "op_gruppe", label = "Velg reseksjonsgruppe(r)",
                                            choices = reseksjonsgrupper, multiple = TRUE),
                                uiOutput(outputId = 'ncsp'),
                                selectInput(inputId = "elektiv", label = "Operasjonstid",
                                            choices = c('Ikke valgt'=99, 'Innenfor normalarbeidstid'=1, 'Utenfor normalarbeidstid'=0)),
                                selectInput(inputId = "hastegrad", label = "Hastegrad",
                                            choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=2)),
                                selectInput(inputId = "BMI", label = "BMI", choices = bmi_valg, multiple = TRUE),
                                selectInput(inputId = "tilgang", label = "Tilgang i abdomen", choices = tilgang_valg, multiple = TRUE),
                                sliderInput(inputId="PRS", label = "mE-PASS", min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
                                selectInput(inputId = "ASA", label = "ASA-grad", choices = ASA_valg, multiple = TRUE),
                                selectInput(inputId = "whoEcog", label = "WHO ECOG score", choices = whoEcog_valg, multiple = TRUE),
                                selectInput(inputId = "forbehandling", label = "Onkologisk forbehandling", multiple = TRUE,
                                            choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
                                selectInput(inputId = "malign", label = "Diagnose", choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)))),
                            selectInput(inputId = "bildeformat", label = "Velg bildeformat",
                                        choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                            a(id = "toggleAdvanced", "Skjul/vis flere valg", href = "#")
                          ),
                          mainPanel(tabsetPanel(
                            tabPanel("Figur",
                                     plotOutput("Figur1", height="auto"), downloadButton("lastNedBilde", "Last ned figur")),
                            tabPanel("Tabell",
                                     uiOutput("utvalg"),
                                     tableOutput("Tabell1"), downloadButton("lastNed", "Last ned tabell"))
                            # tabPanel("Tabell 2",
                            #          tableOutput("Tabell2"))
                          )
                          )
                 ),
                 tabPanel("Andeler",
                          sidebarPanel(
                            selectInput(inputId = "valgtVar_andel", label = "Velg variabel",
                                        choices = varvalg_andel),
                            selectInput(inputId = "valgtVar_andel_stabel", label = "Velg variabel",
                                        choices = varvalg_andel_stabel),
                            dateRangeInput(inputId="datovalg2", label = "Dato fra og til", min = '2014-01-01',
                                           max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), separator = " til "),
                            selectInput(inputId = "enhetsUtvalg2", label = "Kjør rapport for",
                                        choices = c('Egen avd. mot landet forøvrig'=1, 'Hele landet'=0, 'Egen avd.'=2)),
                            selectInput(inputId = "valgtShus2", label = "Velg sykehus",
                                        choices = sykehus, multiple = TRUE),
                            sliderInput(inputId="alder2", label = "Alder", min = 0,
                                        max = 120, value = c(0, 120)),
                            selectInput(inputId = "erMann2", label = "Kjønn",
                                        choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
                            selectInput(inputId = "op_gruppe2", label = "Velg reseksjonsgruppe(r)",
                                        choices = reseksjonsgrupper, multiple = TRUE),
                            uiOutput(outputId = 'ncsp2'),
                            selectInput(inputId = "inkl_konf", label = "Inkluder konfidensintervall",
                                        choices = c(' '=99, 'Ja'=1, 'Nei'=0)),
                            selectInput(inputId = "elektiv2", label = "Operasjonstid",
                                        choices = c('Ikke valgt'=99, 'Innenfor normalarbeidstid'=1, 'Utenfor normalarbeidstid'=0)),
                            selectInput(inputId = "hastegrad2", label = "Hastegrad",
                                        choices = c('Ikke valgt'=99, 'Elektiv'=0, 'Akutt'=1)),
                            selectInput(inputId = "BMI2", label = "BMI", choices = bmi_valg, multiple = TRUE),
                            selectInput(inputId = "tilgang2", label = "Tilgang i abdomen", choices = tilgang_valg, multiple = TRUE),
                            sliderInput(inputId="PRS2", label = "mE-PASS", min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
                            selectInput(inputId = "ASA2", label = "ASA-grad", choices = ASA_valg, multiple = TRUE),
                            selectInput(inputId = "whoEcog2", label = "WHO ECOG score", choices = whoEcog_valg, multiple = TRUE),
                            selectInput(inputId = "forbehandling2", label = "Onkologisk forbehandling", multiple = TRUE,
                                        choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
                            selectInput(inputId = "malign2", label = "Diagnose", choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
                            selectInput(inputId = "tidsenhet", label = "Velg tidsenhet", choices = c('Aar', 'Mnd', 'Kvartal', 'Halvaar')),
                            selectInput(inputId = "bildeformat2", label = "Velg bildeformat",
                                        choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
                          ),
                          mainPanel(tabsetPanel(id = "tabs_andeler",
                            tabPanel("Figur, tidssvisning",
                                     plotOutput("fig_andel_tid", height="auto"),
                                     downloadButton("lastNedBilde_tid", "Last ned figur")),
                            tabPanel("Tabell, tidssvisning",
                                     uiOutput("utvalg_tid"),
                                     tableOutput("Tabell_tid"), downloadButton("lastNed_tid", "Last ned tabell")),
                            tabPanel("Figur, sykehusvisning",
                                     plotOutput("fig_andel_grvar", height="auto"),
                                     downloadButton("lastNedBilde_sykehus_andel", "Last ned figur")),
                            tabPanel("Tabell, sykehusvisning",
                                     uiOutput("utvalg_sykehus_andel"),
                                     tableOutput("Tabell_sykehus_andel"), downloadButton("lastNed_sykehus_andel", "Last ned tabell")),
                            tabPanel("Figur, andeler i stabel",
                                     plotOutput("fig_andel_grvar_stabel", height="auto"),
                                     downloadButton("lastNedBilde_sykehus_andel_stabel", "Last ned figur")),
                            tabPanel("Tabell, andeler i stabel",
                                     DTOutput("Tabell_sykehus_andel_stabel"), downloadButton("lastNedStabelTabell", "Last ned tabell"))
                          )
                          )
                 ),
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
                                        choices = sykehus, multiple = TRUE)
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
    # as.numeric(rapbase::getUserReshId(session))
  })
  userRole <- reactive({
    ifelse(onServer, rapbase::getUserRole(session), 'SC')
    # rapbase::getUserRole(session)
  })

  observe(
    if (userRole() != 'SC') {
      shinyjs::hide(id = 'valgtShus')
      shinyjs::hide(id = 'valgtShus2')
      hideTab(inputId = "tabs_andeler", target = "Figur, sykehusvisning")
      hideTab(inputId = "tabs_andeler", target = "Tabell, sykehusvisning")
    }
  )

  observe(
    if (!is.null(input$tabs_andeler)) {
      if (input$tabs_andeler %in%  c("Figur, sykehusvisning", "Tabell, sykehusvisning",
                                     "Figur, andeler i stabel", "Tabell, andeler i stabel")) {
        shinyjs::hide(id = 'enhetsUtvalg2')
        shinyjs::hide(id = 'tidsenhet')
        shinyjs::hide(id = 'valgtShus2')
      } else {
        shinyjs::show(id = 'enhetsUtvalg2')
        shinyjs::show(id = 'tidsenhet')
        if (userRole() == 'SC') { shinyjs::show(id = 'valgtShus2')}
      }
      if (input$tabs_andeler %in%  c("Figur, andeler i stabel", "Tabell, andeler i stabel")){
        shinyjs::hide(id = 'valgtVar_andel')
        shinyjs::show(id = 'valgtVar_andel_stabel')
        shinyjs::hide(id = 'inkl_konf')
      } else {
        shinyjs::hide(id = 'valgtVar_andel_stabel')
        shinyjs::show(id = 'valgtVar_andel')
        shinyjs::show(id = 'inkl_konf')
      }
      }
  )

  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "avansert", anim = TRUE))

  #################################################################################################################################
  ################ Fordelingsfigurer ##############################################################################################

  output$ncsp <- renderUI({selectInput(inputId = "ncsp_verdi", label = "Velg NCSP kode(r)",
                                       choices = if (is.null(input$op_gruppe)){
                                         ""
                                       } else {
                                         setNames(substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in%
                                                                                              as.numeric(input$op_gruppe)])), 1, 5),
                                                  sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in%
                                                                                       as.numeric(input$op_gruppe)])))
                                       },
                                       multiple = TRUE)})

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

  observe(
    if (is.null(input$op_gruppe)) {
      shinyjs::hide(id = 'ncsp')
    } else {
      shinyjs::show(id = 'ncsp')
    }
  )

  output$Figur1 <- renderPlot({
    norgast::FigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                        maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                        valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
                        op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
                        ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
                        BMI = if (!is.null(input$BMI)) {input$BMI} else {''},
                        tilgang = if (!is.null(input$tilgang)) {input$tilgang} else {''},
                        minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]),
                        ASA = if (!is.null(input$ASA)) {input$ASA} else {''},
                        whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
                        forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
                        malign = as.numeric(input$malign),
                        reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg, erMann = as.numeric(input$erMann),
                        elektiv = as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad))
  }, width = 700, height = 700)

  tabellReager <- reactive({
    TabellData <- norgast::FigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                                      maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                      valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
                                      op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
                                      ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
                                      BMI = if (!is.null(input$BMI)) {input$BMI} else {''},
                                      tilgang = if (!is.null(input$tilgang)) {input$tilgang} else {''},
                                      minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]),
                                      ASA = if (!is.null(input$ASA)) {input$ASA} else {''},
                                      whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
                                      forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
                                      malign = as.numeric(input$malign),
                                      reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg, erMann = as.numeric(input$erMann),
                                      elektiv = as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad))
  })

  output$utvalg <- renderUI({
    TabellData <- tabellReager()
    tagList(
      h3(TabellData$tittel),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})



  output$Tabell1 <- function() {

    TabellData <- tabellReager()
    if (input$enhetsUtvalg == 1) {
      Tabell1 <- TabellData$Antall %>%
        mutate(Kategori = rownames(.)) %>%
        select(Kategori, everything()) %>%
        mutate(AndelHoved = 100*AntHoved/NHoved) %>%
        mutate(AndelRest= 100*AntRest/Nrest)
      Tabell1 <- Tabell1[, c(1,2,4,6,3,5,7)]
      names(Tabell1) <- c('Kategori', 'Antall', 'N', 'Andel', 'Antall', 'N', 'Andel')
      Tabell1 %>% knitr::kable("html", digits = c(0,0,0,1,0,0,1)) %>%
        kable_styling("hover", full_width = F) %>%
        add_header_above(c(" ", "Din avdeling" = 3, "Landet forøvrig" = 3))
    } else {
      Tabell1 <- TabellData$Antall %>%
        mutate(Kategori = rownames(.)) %>%
        select(Kategori, everything()) %>%
        mutate(AndelHoved = 100*AntHoved/NHoved)
      names(Tabell1) <- c('Kategori', 'Antall', 'N', 'Andel')
      Tabell1 %>%
        knitr::kable("html", digits = c(0,0,0,1)) %>%
        kable_styling("hover", full_width = F)
    }

  }

  output$lastNed <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- tabellReager()
      if (input$enhetsUtvalg == 1) {
        Tabell1 <- TabellData$Antall %>%
          mutate(Kategori = rownames(.)) %>%
          select(Kategori, everything()) %>%
          mutate(AndelHoved = 100*AntHoved/NHoved) %>%
          mutate(AndelRest= 100*AntRest/Nrest)
        Tabell1 <- Tabell1[, c(1,2,4,6,3,5,7)]
      } else {
        Tabell1 <- TabellData$Antall %>%
          mutate(Kategori = rownames(.)) %>%
          select(Kategori, everything()) %>%
          mutate(AndelHoved = 100*AntHoved/NHoved)
      }
      write.csv2(Tabell1, file, row.names = F)
    }
  )

  output$lastNedBilde <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
    },

    content = function(file){
      norgast::FigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                          maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                          valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
                          op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
                          ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
                          BMI = if (!is.null(input$BMI)) {input$BMI} else {''},
                          tilgang = if (!is.null(input$tilgang)) {input$tilgang} else {''},
                          minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]),
                          ASA = if (!is.null(input$ASA)) {input$ASA} else {''},
                          whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
                          forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
                          malign = as.numeric(input$malign),
                          reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg, erMann = as.numeric(input$erMann),
                          elektiv = as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad), outfile = file)
    }
  )

  #################################################################################################################################
  ################ Andeler ########################################################################################################

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
      h3(TabellData$tittel),
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
      h3(TabellData$tittel),
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
      h3(TabellData$tittel),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  stabeltabell <- function() {
    TabellData <- tabellReagerSykehusAndelStabel()
    aux <- as.data.frame.matrix(TabellData$Antall)
    aux$N <- TabellData$Ngr
    aux$Avdeling <- row.names(aux)
    row.names(aux) <- 1:dim(aux)[1]
    aux <- aux[, c(dim(aux)[2], dim(aux)[2]-1, 1:(dim(aux)[2]-2))]
    aux <- rbind(aux, c(Avdeling='Norge', colSums(aux[,-1])))
    aux[,-1] <- apply(aux[,-1], 2, as.numeric)
    sketch <- htmltools::withTags(table(
      tableHeader(aux[-dim(aux)[1], ]),
      tableFooter(c('Sum' , as.numeric(aux[dim(aux)[1], 2:dim(aux)[2]])))))
    list(ant_skjema=aux, sketch=sketch)
  }


  ##### Under Arbeid: Tall tolkes som tekst
  output$Tabell_sykehus_andel_stabel = renderDT(
    datatable(stabeltabell()$ant_skjema[-dim(stabeltabell()$ant_skjema)[1], ],
              container = stabeltabell()$sketch,
              rownames = F,
              options = list(pageLength = 25)
    )
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
}

# Run the application
shinyApp(ui = ui, server = server)

