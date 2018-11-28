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

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
onServer <- context == "TEST" | context == "QA" | context == "PRODUCTION"
if (onServer) {
  RegData <- NorgastHentRegData()
} else {
  # rm(list = ls())
  RegData <- read.table('I:/norgast/AlleVariablerNum2018-11-14 14-30-58.txt', header=TRUE, sep=";",
                        encoding = 'UFT-8', stringsAsFactors = F)
  # RegData <- read.table('I:/norgast/AlleVarNum2018-11-26 12-42-46.txt', header=TRUE, sep=";",
  #                       encoding = 'UFT-8', stringsAsFactors = F)
  ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-11-14 14-31-11.txt', header=TRUE, sep=";",
                           encoding = 'UFT-8', stringsAsFactors = F)

  RegData <- RegData[,c('ForlopsID','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                        'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                        'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                        'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                        'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]
  # , "Hastegrad")]
  ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
  RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")

  skjemaoversikt <- read.table('I:/norgast/SkjemaOversikt2018-11-14 14-31-15.txt', header=TRUE, sep=';', stringsAsFactors = F)
  skjemaoversikt$Sykehusnavn <- iconv(skjemaoversikt$Sykehusnavn, from = 'UTF-8', to = '')
  skjemaoversikt$Skjemanavn <- iconv(skjemaoversikt$Skjemanavn, from = 'UTF-8', to = '')
  skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
}

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
library(shiny)
library(shinyjs)

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
                                        choices = c(' '=99, 'Kvinne'=0, 'Mann'=1)),
                            shinyjs::hidden(
                              div(id = "avansert",
                                selectInput(inputId = "op_gruppe", label = "Velg reseksjonsgruppe(r)",
                                            choices = reseksjonsgrupper, multiple = TRUE),
                                uiOutput(outputId = 'ncsp'),
                                selectInput(inputId = "elektiv", label = "Operasjonstid",
                                            choices = c(' '=99, 'Innenfor normalarbeidstid'=1, 'Utenfor normalarbeidstid'=0)),
                                selectInput(inputId = "hastegrad", label = "Hastegrad",
                                            choices = c(' '=99, 'Elektiv'=0, 'Akutt'=1)),
                                selectInput(inputId = "BMI", label = "BMI", choices = bmi_valg, multiple = TRUE),
                                selectInput(inputId = "tilgang", label = "Tilgang i abdomen", choices = tilgang_valg, multiple = TRUE),
                                sliderInput(inputId="PRS", label = "mE-PASS", min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
                                selectInput(inputId = "ASA", label = "ASA-grad", choices = ASA_valg, multiple = TRUE),
                                selectInput(inputId = "whoEcog", label = "WHO ECOG score", choices = whoEcog_valg, multiple = TRUE),
                                selectInput(inputId = "forbehandling", label = "Onkologisk forbehandling", multiple = TRUE,
                                            choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
                                selectInput(inputId = "malign", label = "Diagnose", choices = c(' '=99, 'Malign'=1, 'Benign'=0)))),
                            selectInput(inputId = "bildeformat", label = "Velg bildeformat",
                                        choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                            a(id = "toggleAdvanced", "Skjul/vis flere valg", href = "#")
                          ),
                          mainPanel(tabsetPanel(
                            tabPanel("Figur",
                                     plotOutput("Figur1", height="auto"), downloadButton("lastNedBilde", "Last ned bilde")),
                            tabPanel("Tabell",
                                     uiOutput("utvalg"),
                                     tableOutput("Tabell1"), downloadButton("lastNed", "Last ned tabell"))
                            # tabPanel("Tabell 2",
                            #          tableOutput("Tabell2"))
                          )
                          )
                 ),
                 tabPanel("Administrative tabeller",
                          sidebarPanel(
                            dateRangeInput(inputId="datovalg2", label = "Dato fra og til", min = '2014-01-01',
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
    ifelse(onServer, as.numeric(rapbase::getShinyUserReshId(session, testCase = TRUE)), 4204082)
  })
  userRole <- reactive({
    ifelse(onServer, rapbase::getShinyUserRole(session, testCase = TRUE), 'SC')
  })

  observe(
    if (userRole() != 'SC') {
      shinyjs::hide(id = 'valgtShus')
    }
  )

  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "avansert", anim = TRUE))

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

  observe(
    if (is.null(input$op_gruppe)) {
      shinyjs::hide(id = 'ncsp')
    } else {
      shinyjs::show(id = 'ncsp')
    }
  )

  antskjema <- function() {
    aux <- as.data.frame.matrix(addmargins(table(skjemaoversikt[skjemaoversikt$SkjemaStatus == as.numeric(input$regstatus) &
                                                                  skjemaoversikt$HovedDato >= input$datovalg[1] &
                                                                  skjemaoversikt$HovedDato <= input$datovalg[2],
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
                        elektiv = as.numeric(input$elektiv))
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
                                      elektiv = as.numeric(input$elektiv))
  })

  output$utvalg <- renderText({
    TabellData <- tabellReager()
    utvalgstekst <- TabellData$utvalgTxt
    paste0('Utvalg: ', utvalgstekst)
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
                          elektiv = as.numeric(input$elektiv), outfile = file)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)

