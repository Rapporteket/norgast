# Modul for fordelingsfigurer i NoRGast sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# return Modul fordelingsfigur
#
indikatorfig_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(width = 3,
                 id = ns("id_indikator_panel"),
                 selectInput(inputId = ns("valgtVar"), label = "Velg indikator",
                             choices = c("Generell: Premorbid vekttap registrert" = "Vekttap_registrert",
                                         "Generell: Aktiv kontroll" = "AktivKontroll",
                                         "Generell: Sårruptur" = "Saarruptur",
                                         "Kolon: Laparoskopi" = "LapTilgang2",
                                         "Kolon: Laparoskopi, ECOG 1 og 2" = "LapTilgang2_ecog",
                                         "Kolon: Relaparotomi/-laparoskopi" = "ReLapNarkose",
                                         "Kolon: Konverteringsrate" = "konv_rate",
                                         "Kolon: Anastomoselekkasje" = "Anastomoselekkasje",
                                         "Kolon: 90-dagers dødelighet" = "mortalitet90")),
                 uiOutput(outputId = ns('tilAar')),
                 selectInput(inputId = ns("valgtShus"), label = "Fjern sykehus pga. lav dekningsgrad",
                             choices = BrValg$sykehus, multiple = TRUE),
                 # selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
                 #             choices = BrValg$reseksjonsgrupper, multiple = TRUE),
                 # uiOutput(outputId = ns('ncsp')),
                 # shinyjs::hidden(
                 #   div(
                 #     id = ns("avansert"),
                 #     selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                 #                 choices = BrValg$sykehus, multiple = TRUE),
                 #     selectInput(inputId = ns("tilgang"), label = "Tilgang i abdomen (velg en eller flere)", choices = BrValg$tilgang_valg, multiple = TRUE),
                 #     sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                 #                 max = 120, value = c(0, 120)),
                 #     selectInput(inputId = ns("erMann"), label = "Kjønn",
                 #                 choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
                 #     selectInput(inputId = ns("elektiv"), label = "Tidspunkt for operasjonsstart",
                 #                 choices = c('Ikke valgt'=99, 'Innenfor normalarbeidstid'=1, 'Utenfor normalarbeidstid'=0)),
                 #     selectInput(inputId = ns("hastegrad"), label = "Hastegrad",
                 #                 choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=2)),
                 #     selectInput(inputId = ns("BMI"), label = "BMI", choices = BrValg$bmi_valg, multiple = TRUE),
                 #     sliderInput(inputId=ns("PRS"), label = "mE-PASS", min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
                 #     selectInput(inputId = ns("ASA"), label = "ASA-grad", choices = BrValg$ASA_valg, multiple = TRUE),
                 #     selectInput(inputId = ns("modGlasgow"), label = "Modified Glasgow score", choices = 0:2, multiple = TRUE),
                 #     selectInput(inputId = ns("whoEcog"), label = "WHO ECOG score", choices = BrValg$whoEcog_valg, multiple = TRUE),
                 #     selectInput(inputId = ns("forbehandling"), label = "Onkologisk forbehandling", multiple = TRUE,
                 #                 choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
                 #     selectInput(inputId = ns("malign"), label = "Diagnose", choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
                 #   )
                 # ),
                 selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                             choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                 tags$hr(),
                 actionButton(ns("reset_input"), "Nullstill valg"),
                 # br(),
                 # a(id = ns("toggleAdvanced"), "Skjul/vis flere valg", href = "#")
    ),
    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur", value = "fig",
                           plotOutput(ns("Figur1"), height="auto"), downloadButton(ns("lastNedBilde"), "Last ned figur"))#,
                  # tabPanel("Tabell", value = "tab",
                  #          uiOutput(ns("utvalg")),
                  #          # textOutput(ns("utvalg")),
                  #          br(),
                  #          tableOutput(ns("Tabell1")),
                  #          downloadButton(ns("lastNed"), "Last ned tabell"))
      )
    )
  )

}





indikatorfig <- function(input, output, session, reshID, RegData, userRole, hvd_session){

  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "avansert", anim = TRUE))

  observeEvent(input$reset_input, {
    shinyjs::reset("id_indikator_panel")
  })

  observe(
    if (userRole != 'SC') {
      shinyjs::hide(id = 'valgtShus')
    })

  output$ncsp <- renderUI({
    ns <- session$ns
    if (!is.null(input$op_gruppe)) {
      selectInput(inputId = ns("ncsp_verdi"), label = "NCSP koder (velg en eller flere)",
                  choices = if (!is.null(input$op_gruppe)) {
                    setNames(substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe)])), 1, 5),
                             sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe)])))
                  }, multiple = TRUE)
    }
  })

  output$tilAar <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("tilAar_verdi"), label = "T.o.m. år",
                choices = rev((min(RegData$Aar)+2):max(RegData$Aar)))
  })


  ############### Andeler #######################################################

  output$Figur1 <- renderPlot({
    switch(input$valgtVar,
           "Vekttap_registrert" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ], valgtVar = "Vekttap_registrert",
                                                                        outfile="", tittel="",  width=600, height=700,
                                                                        decreasing=F, terskel=10, minstekrav = 80, maal = 90,
                                                                        legPlass='topleft',
                                                                        lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                        inkl_konf=T, op_gruppe='',
                                                                        hastegrad_hybrid=1, malign=99, lavDGtekst = ""),
           "AktivKontroll" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ], valgtVar = "AktivKontroll",
                                                                   outfile="", tittel="",  width=600, height=700,
                                                                   decreasing=F, terskel=10, minstekrav = 70, maal = 90,
                                                                   legPlass='topleft', graaUt="",
                                                                   lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                   inkl_konf=T, op_gruppe='',
                                                                   hastegrad_hybrid=1, malign=99, lavDGtekst = ""),
           "Saarruptur" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ], valgtVar = "Saarruptur",
                                                                   outfile="", tittel="",  width=600, height=700,
                                                                   decreasing=T, terskel=10, minstekrav = 4, maal = 3,
                                                                   legPlass='topright', graaUt="",
                                                                lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                   inkl_konf=T, op_gruppe='',
                                                                   hastegrad_hybrid=1, malign=99, lavDGtekst = "")
           )
  }, width = 600, height = 700)

}








# library(norgast)
# library(tidyverse)
# rm(list = ls())
#
# RegData <- NorgastHentRegData()
# # skjemaoversikt <- NorgastHentSkjemaOversikt()
# RegData <- NorgastPreprosess(RegData)
#
# gr <- c(1:6)
# grtxt <- c('Kol.','Rekt.','Øsof.','Ventr.',
#            'Lever',"Pankreas")
# RegData$Op_grAarsrapp[RegData$Op_grAarsrapp==7]<- 6
# RegData$Op_grAarsrapp[RegData$Op_grAarsrapp %in% c(8,99)]<- NA
#
# rap_aar <- 2020 # Året rapporten skal kjøres for
# ant_aar <- 3 # Hvor mange år som skal inkluderes i flerårsfigurer
#
# RegData$Op_grAarsrapp <- factor(RegData$Op_grAarsrapp, levels=gr, labels = grtxt)
#
# reshID <- 0
# datoFra= paste0(rap_aar, '-01-01')
# datoTil= paste0(rap_aar, '-12-31')
#
# RegDataAll <- RegData[RegData$Aar<=rap_aar, ]
# RegData <- RegData[RegData$Aar==rap_aar, ]
#
# graaUt_alle <- ""
#
# width=600
# height=700
# sideTxt='Sykehus'
# decreasing=F
# terskel=10
# minstekrav = NA
# maal = NA
# skriftStr=1.3
# pktStr=1.4
# legPlass='top'
# minstekravTxt='Akseptabelt'
# maalTxt='Mål'
# graaUt=NA
# minald=0
# maxald=130
# erMann <- 99
# inkl_konf <- T
# elektiv=99
# datoFra <- '2015-01-01'
# tittel <- ''
# hentData <- F
# preprosess <- F
# BMI=''
# tilgang=''
# minPRS=0
# maxPRS=2.2
# ASA=''
# whoEcog= ''
# ncsp=''
# forbehandling=''
# valgtShus=c('')
# # reseksjonsGr <- ''
# op_gruppe <- ''
# malign <- 99
# annet_format_ut <- T
# ut_format <- 'pdf'
#
# valgtVar <- 'Vekttap_registrert'
# outfile <- 'Vekttap_registrert.pdf'
# x11()
# outfile <- ''
# norgastIndikator_rapporteket(RegDataAll[RegDataAll$Op_gr %in% 1:7, ], valgtVar = valgtVar, outfile=outfile, tittel=tittel, width=width, height=height,
#                              decreasing=decreasing, terskel=terskel, minstekrav = 80, maal = 90, skriftStr=skriftStr,
#                              pktStr=pktStr, legPlass='topleft', minstekravTxt=minstekravTxt, maalTxt=maalTxt, graaUt=graaUt,
#                              inkl_konf=inkl_konf, op_gruppe=op_gruppe, datoFra=datoFra, datoTil=datoTil, hastegrad_hybrid=1, malign=malign, lavDG = graaUt_alle, lavDGtekst = 'Dekningsgrad < 55 %')























