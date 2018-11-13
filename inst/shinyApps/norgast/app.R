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

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
onServer <- context == "TEST" | context == "QA" | context == "PRODUCTION"
if (onServer) {
  RegData <- NorgastHentRegData()
} else {
  # rm(list = ls())
  RegData <- read.table('I:/norgast/AlleVariablerNum2018-09-12 08-52-43.txt', header=TRUE, sep=";",
                        encoding = 'UFT-8', stringsAsFactors = F)
  ForlopData <- read.table('I:/norgast/ForlopsOversikt2018-09-12 08-52-54.txt', header=TRUE, sep=";",
                           encoding = 'UFT-8', stringsAsFactors = F)

  RegData <- RegData[,c('ForlopsID','BMIKategori','VekttapProsent','MedDiabetes','KunCytostatika','KunStraaleterapi',
                        'KjemoRadioKombo','WHOECOG','ModGlasgowScore','ASA','AnestesiStartKl','Hovedoperasjon','OpDato',
                        'NyAnastomose','NyStomi','Tilgang','Robotassistanse','ThoraxTilgang','ReLapNarkose','ViktigsteFunn',
                        'AccordionGrad', 'PRSScore','RegistreringStatus', 'OppfStatus', 'OppfAccordionGrad',
                        'OppfReLapNarkose', 'OppfViktigsteFunn', 'Avdod', 'AvdodDato', 'BMI', 'Hoveddiagnose')]
  ForlopData <- ForlopData[,c('ErMann', 'AvdRESH', 'Sykehusnavn', 'PasientAlder', 'HovedDato', 'BasisRegStatus', 'ForlopsID', 'PasientID')]
  RegData <- merge(RegData, ForlopData, by.x = "ForlopsID", by.y = "ForlopsID")
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

######################################################################

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(title = "RAPPORTEKET NORGAST", theme = "bootstrap.css",
                 tabPanel("Fordelingsfigurer",
                          # sidebarLayout(
                          sidebarPanel(
                            shinyjs::useShinyjs(),
                            selectInput(inputId = "valgtVar", label = "Velg variabel",
                                        choices = varvalg),
                            dateInput(inputId = 'datoFra', value = '2014-01-01', min = '2014-01-01',
                                      label = "F.o.m. dato", language="nb"),
                            dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2014-01-01',
                                      label = "T.o.m. dato", language="nb"),
                            selectInput(inputId = "valgtShus", label = "Velg sykehus",
                                        choices = sykehus, multiple = TRUE),
                            selectInput(inputId = "enhetsUtvalg", label = "Kjør rapport for",
                                        choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1, 'Egen avd.'=2)),
                            sliderInput(inputId="alder", label = "Alder", min = 0,
                                        max = 120, value = c(0, 120)),
                            selectInput(inputId = "op_gruppe", label = "Velg reseksjonsgruppe(r)",
                                        choices = reseksjonsgrupper, multiple = TRUE),
                            uiOutput(outputId = 'ncsp'),
                            selectInput(inputId = "bildeformat", label = "Velg bildeformat",
                                        choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
                          ),
                          mainPanel(tabsetPanel(
                            tabPanel("Figur",
                                     textOutput("testSessionObj"),
                                     plotOutput("Figur1", height="auto"), downloadButton("lastNedBilde", "Last ned bilde"))#,
                            # tabPanel("Tabell",
                            #          tableOutput("Tabell1"), downloadButton("lastNed", "Last ned tabell")),
                            # tabPanel("Tabell 2",
                            #          tableOutput("Tabell2"))
                          )
                          )
                 )
)


#
server <- function(input, output, session) {

  output$testSessionObj <- renderText({
    paste("username:", rapbase::getShinyUserName(session, testCase = TRUE),
          "groups:", rapbase::getShinyUserGroups(session, testCase = TRUE),
          "role:", rapbase::getShinyUserRole(session, testCase = TRUE),
          "reshId:", rapbase::getShinyUserReshId(session, testCase = TRUE))
  })

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

  output$ncsp <- renderUI({selectInput(inputId = "ncsp_verdi", label = "Velg NCSP kode(r)",
                                             choices = if (is.null(input$op_gruppe)){
                                               ""
                                             } else {
                                               substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in%
                                                                                           as.numeric(input$op_gruppe)])), 1, 5)
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
                        maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil,
                        valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
                        op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
                        ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
                        reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg)
  }, width = 700, height = 700)

  # tabellReager <- reactive({
  #   TabellData <- MuskelFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
  #                                  maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil,
  #                                  diagnosegr = if (!is.null(input$diagnosegr)) {as.numeric(input$diagnosegr)} else {-1},
  #                                  diagnose = if (!is.null(input$icd10_kntr_verdi)) {input$icd10_kntr_verdi} else {'-1'},
  #                                  undergr = if (!is.null(input$undergruppe1_verdi)) {as.numeric(input$undergruppe1_verdi)} else {-1},
  #                                  undergr2 = if (!is.null(input$undergruppe2_verdi)) {as.numeric(input$undergruppe2_verdi)} else {-1},
  #                                  reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg)
  # })
  #
  # output$Tabell1 <- function() {
  #
  #   TabellData <- tabellReager()
  #   if (input$enhetsUtvalg == 1) {
  #     Tabell1 <- TabellData$Antall %>%
  #       mutate(Kategori = rownames(.)) %>%
  #       select(Kategori, everything()) %>%
  #       mutate(AndelHoved = 100*AntHoved/NHoved) %>%
  #       mutate(AndelRest= 100*AntRest/Nrest)
  #     Tabell1 <- Tabell1[, c(1,2,4,6,3,5,7)]
  #     names(Tabell1) <- c('Kategori', 'Antall', 'N', 'Andel', 'Antall', 'N', 'Andel')
  #     Tabell1 %>% knitr::kable("html", digits = c(0,0,0,1,0,0,1)) %>%
  #       kable_styling("hover", full_width = F) %>%
  #       add_header_above(c(" ", "Din avdeling" = 3, "Landet forøvrig" = 3))
  #   } else {
  #     Tabell1 <- TabellData$Antall %>%
  #       mutate(Kategori = rownames(.)) %>%
  #       select(Kategori, everything()) %>%
  #       mutate(AndelHoved = 100*AntHoved/NHoved)
  #     names(Tabell1) <- c('Kategori', 'Antall', 'N', 'Andel')
  #     Tabell1 %>%
  #       knitr::kable("html", digits = c(0,0,0,1)) %>%
  #       kable_styling("hover", full_width = F)
  #   }
  #
  # }
  #
  # output$lastNed <- downloadHandler(
  #   filename = function(){
  #     paste0(input$valgtVar, Sys.time(), '.csv')
  #   },
  #
  #   content = function(file){
  #     TabellData <- tabellReager()
  #     if (input$enhetsUtvalg == 1) {
  #       Tabell1 <- TabellData$Antall %>%
  #         mutate(Kategori = rownames(.)) %>%
  #         select(Kategori, everything()) %>%
  #         mutate(AndelHoved = 100*AntHoved/NHoved) %>%
  #         mutate(AndelRest= 100*AntRest/Nrest)
  #       Tabell1 <- Tabell1[, c(1,2,4,6,3,5,7)]
  #     } else {
  #       Tabell1 <- TabellData$Antall %>%
  #         mutate(Kategori = rownames(.)) %>%
  #         select(Kategori, everything()) %>%
  #         mutate(AndelHoved = 100*AntHoved/NHoved)
  #     }
  #     write.csv2(Tabell1, file, row.names = F)
  #   }
  # )

  # output$lastNedBilde <- downloadHandler(
  #   filename = function(){
  #     paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
  #   },
  #
  #   content = function(file){
  #     MuskelFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
  #                      maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil,
  #                      diagnosegr = if (!is.null(input$diagnosegr)) {as.numeric(input$diagnosegr)} else {-1},
  #                      diagnose = if (!is.null(input$icd10_kntr_verdi)) {input$icd10_kntr_verdi} else {'-1'},
  #                      undergr = if (!is.null(input$undergruppe1_verdi)) {as.numeric(input$undergruppe1_verdi)} else {-1},
  #                      undergr2 = if (!is.null(input$undergruppe2_verdi)) {as.numeric(input$undergruppe2_verdi)} else {-1},
  #                      reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg)
  #   }
  # )


  # output$Tabell2 <- function() {
  #   req(input$mpg)
  #   mtcars %>%
  #     mutate(car = rownames(.)) %>%
  #     select(car, everything()) %>%
  #     filter(mpg <= 20) %>%
  #     knitr::kable("html") %>%
  #     kable_styling("striped", full_width = F) %>%
  #     add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6))
  # }

}

# Run the application
shinyApp(ui = ui, server = server)

