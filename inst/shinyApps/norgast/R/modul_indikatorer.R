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
                                         "Kolon: Laparoskopi" = "Tilgang_elektiv_malign_aar",
                                         "Kolon: Laparoskopi, ECOG 1 og 2" = "Tilgang_elektiv_malign_ecog_0_1",
                                         "Kolon: Accordion score ≥ 3, benign diagnose" = "KumAcc_elektiv_benign_aar",
                                         "Kolon: Accordion score ≥ 3, malign diagnose" = "KumAcc_elektiv_malign_aar",
                                         "Kolon: Relaparotomi/-laparoskopi" = "ReLapNarkose_malign_ecog0_1_aar",
                                         "Kolon: Konverteringsrate" = "konv_rate_malign_ecog0_1_aar",
                                         "Kolon: Anastomoselekkasje" = "Anastomoselekkasje_malign_ecog0_1aar",
                                         "Kolon: 90-dagers dødelighet" = "mortalitet90_kolon_aar",
                                         "Kolon: 90-dagers dødelighet referansepasienter" = "mortalitet90_kolon_aar_refpas",
                                         "Kolon: Andel akuttkirurgi" = "hastegrad_ny_kolon_aar",
                                         "Rektum: Laparoskopi" = "Tilgang_rekt_malign_ecog_0_1",
                                         "Rektum: Accordion score ≥ 3" = "KumAcc_rekt_aar",
                                         "Rektum: Relaparotomi/-laparoskopi" = "ReLapNarkose_rekt_malign_ecog_0_1",
                                         "Rektum: Konverteringsrate" = "konv_rate_rektum_malign_ecog0_1",
                                         "Rektum: Anastomoselekkasje" = "Anastomoselekkasje_rekt_malign_ecog_0_1",
                                         "Rektum: 90-dagers dødelighet" = "mortalitet90_rektum_aar",
                                         "Lever: Laparoskopi" = "Tilgang_lever_aar",
                                         "Lever: Relaparotomi/-laparoskopi" = "ReLapNarkose_lever_aar",
                                         "Lever: 90-dagers dødelighet" = "mortalitet90_lever_aar",
                                         "Whipple: Vene- eller arterierekonstruksjon" = "Rekonstruksjon_whipple_aar",
                                         "Whipple: Relaparotomi/-laparoskopi" = "ReLapNarkose_whipple",
                                         "Whipple: Accordion score ≥ 3" = "AccordionGrad_whipple_aar",
                                         "Whipple: 90-dagers dødelighet" = "mortalitet90_whipple_aar",
                                         "Whipple: Postoperativ pankreasfistel" = "CR_POPF_whipple_aar",
                                         "Andre pankreas: Laparoskopi" = "Tilgang_ovrigpankreas_aar",
                                         "Andre pankreas: Vene- eller arterierekonstruksjon" = "Rekonstruksjon_ovrigpankreas_aar",
                                         "Andre pankreas Postoperativ pankreasfistel" = "CR_POPF_ovrigpankreas_aar",
                                         "Whipple: Accordion score ≥ 3" = "AccordionGrad_ovrigpankreas_aar",
                                         "Øsofagus: Accordion score ≥ 3" = "AccordionGrad_osofagus_aar",
                                         "Øsofagus: Relaparotomi/-laparoskopi" = "ReLapNarkose_osofagus_aar",
                                         "Øsofagus: 90-dagers dødelighet" = "mortalitet90_osofagus_aar",
                                         "Øsofagus: Anastomoselekkasje" = "Anastomoselekk_osofagus_aar",
                                         "Ventrikkel: Laparoskopi" = "Tilgang_ventrikkel_aar_v2",
                                         "Ventrikkel: Accordion score ≥ 3" = "AccordionGrad_ventrikkel_aar",
                                         "Ventrikkel: Relaparotomi/-laparoskopi" = "ReLapNarkose_ventrikkel_aar",
                                         "Ventrikkel: 90-dagers dødelighet" = "mortalitet90_ventrikkel_aar"
                             )
                 ),
                 uiOutput(outputId = ns('tilAar')),
                 selectInput(inputId = ns("valgtShus"), label = "Fjern sykehus pga. lav dekningsgrad",
                             choices = BrValg$sykehus, multiple = TRUE),
                 selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                             choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                 tags$hr(),
                 actionButton(ns("reset_input"), "Nullstill valg")
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

  RegData <- RegData[RegData$Op_gr %in% 1:7, ]

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


  ############### Plot #######################################################

  output$Figur1 <- renderPlot({
    switch(input$valgtVar,
           "Vekttap_registrert" = norgast::norgastIndikator_rapporteket(RegData = if(!is.null(input$tilAar_verdi)) {RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ]} else {RegData},
                                                                        valgtVar = "Vekttap_registrert",
                                                                        outfile="", tittel="",  width=600, height=700,
                                                                        decreasing=F, terskel=10, minstekrav = 80, maal = 90,
                                                                        legPlass='topleft',
                                                                        lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                        inkl_konf=T, op_gruppe='',
                                                                        hastegrad_hybrid=1, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "AktivKontroll" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                   valgtVar = "AktivKontroll",
                                                                   outfile="", tittel="",  width=600, height=700,
                                                                   decreasing=F, terskel=10, minstekrav = 70, maal = 90,
                                                                   legPlass='topleft', graaUt="",
                                                                   lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                   inkl_konf=T, op_gruppe='',
                                                                   hastegrad_hybrid=1, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "Saarruptur" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                valgtVar = "Saarruptur",
                                                                outfile="", tittel="",  width=600, height=700,
                                                                decreasing=T, terskel=10, minstekrav = 4, maal = 3,
                                                                legPlass='topright', graaUt="",
                                                                lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                inkl_konf=T, op_gruppe='',
                                                                hastegrad_hybrid=1, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "Tilgang_elektiv_malign_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                valgtVar = "LapTilgang2",
                                                                                outfile="", tittel="",  width=600, height=700,
                                                                                decreasing=F, terskel=10, minstekrav = 60, maal = 70,
                                                                                legPlass='topleft', graaUt="",
                                                                                lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                inkl_konf=T, op_gruppe=1,
                                                                                hastegrad_hybrid=1, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "Tilgang_elektiv_malign_ecog_0_1" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                     valgtVar = "LapTilgang2",
                                                                                     outfile="", tittel="",  width=600, height=700,
                                                                                     decreasing=F, terskel=10, minstekrav = 60, maal = 70,
                                                                                     legPlass='topleft', graaUt="",
                                                                                     lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                     inkl_konf=T, op_gruppe=1, whoEcog = c(0,1),
                                                                                     hastegrad_hybrid=1, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "KumAcc_elektiv_benign_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                               valgtVar = "KumAcc",
                                                                               outfile="", tittel="",  width=600, height=700,
                                                                               decreasing=T, terskel=10, minstekrav = NA, maal = NA,
                                                                               legPlass='topright', graaUt="",
                                                                               lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                               inkl_konf=T, op_gruppe=1,
                                                                               hastegrad_hybrid=99, malign=0, lavDGtekst = "Lav dekningsgrad"),
           "KumAcc_elektiv_malign_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                               valgtVar = "KumAcc",
                                                                               outfile="", tittel="",  width=600, height=700,
                                                                               decreasing=T, terskel=10, minstekrav = NA, maal = NA,
                                                                               legPlass='topright', graaUt="",
                                                                               lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                               inkl_konf=T, op_gruppe=1,
                                                                               hastegrad_hybrid=99, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "ReLapNarkose_malign_ecog0_1_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                     valgtVar = "ReLapNarkose",
                                                                                     outfile="", tittel="",  width=600, height=700,
                                                                                     decreasing=T, terskel=10, minstekrav = NA, maal = 12, maalretn = "lav",
                                                                                     legPlass='topright', graaUt="",
                                                                                     lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                     inkl_konf=T, op_gruppe=1, whoEcog = c(0,1),
                                                                                     hastegrad_hybrid=1, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "konv_rate_malign_ecog0_1_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                  valgtVar = "konv_rate",
                                                                                  outfile="", tittel="",  width=600, height=700,
                                                                                  decreasing=T, terskel=10, minstekrav = 15, maal = 10,
                                                                                  legPlass='topright', graaUt="",
                                                                                  lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                  inkl_konf=T, op_gruppe=1, whoEcog = c(0,1),
                                                                                  hastegrad_hybrid=1, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "Anastomoselekkasje_malign_ecog0_1aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                          valgtVar = "Anastomoselekkasje",
                                                                                          outfile="", tittel="",  width=600, height=700,
                                                                                          decreasing=T, terskel=10, minstekrav = 6, maal = 4,
                                                                                          legPlass='topright', graaUt="",
                                                                                          lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                          inkl_konf=T, op_gruppe=1, whoEcog = c(0,1),
                                                                                          hastegrad_hybrid=1, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "mortalitet90_kolon_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                            valgtVar = "mortalitet90",
                                                                            outfile="", tittel="",  width=600, height=700,
                                                                            decreasing=T, terskel=10, minstekrav = NA, maal = NA,
                                                                            legPlass='topright', graaUt="",
                                                                            lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                            inkl_konf=T, op_gruppe=1,
                                                                            hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "mortalitet90_kolon_aar_refpas" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                   valgtVar = "mortalitet90",
                                                                                   outfile="", tittel="",  width=600, height=700,
                                                                                   decreasing=T, terskel=10, minstekrav = NA, maal = NA,
                                                                                   legPlass='topright', graaUt="",
                                                                                   lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                   inkl_konf=T, op_gruppe=1, whoEcog = c(0,1),
                                                                                   hastegrad_hybrid=1, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "hastegrad_ny_kolon_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                            valgtVar = "Hastegrad",
                                                                            outfile="", tittel="",  width=600, height=700,
                                                                            decreasing=F, terskel=10, minstekrav = NA, maal = NA,
                                                                            legPlass='topright', graaUt="",
                                                                            lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                            inkl_konf=T, op_gruppe=1,
                                                                            hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),

           "ohjelp_kveld_kolon_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                            valgtVar = "ohjelp_kveld",
                                                                            outfile="", tittel="",  width=600, height=700,
                                                                            decreasing=F, terskel=10, minstekrav = NA, maal = NA,
                                                                            legPlass='topright', graaUt="",
                                                                            lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                            inkl_konf=T, op_gruppe=1, hastegrad = 2,
                                                                            hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "Tilgang_rekt_malign_ecog_0_1" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                  valgtVar = "LapTilgang2",
                                                                                  outfile="", tittel="",  width=600, height=700,
                                                                                  decreasing=F, terskel=10, minstekrav = 60, maal = 70,
                                                                                  legPlass='topleft', graaUt="",
                                                                                  lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                  inkl_konf=T, op_gruppe=2, whoEcog = c(0,1),
                                                                                  hastegrad_hybrid=1, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "KumAcc_rekt_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                     valgtVar = "KumAcc",
                                                                     outfile="", tittel="",  width=600, height=700,
                                                                     decreasing=T, terskel=10, minstekrav = NA, maal = NA,
                                                                     legPlass='topright', graaUt="",
                                                                     lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                     inkl_konf=T, op_gruppe=2,
                                                                     hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "ReLapNarkose_rekt_malign_ecog_0_1" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                       valgtVar = "ReLapNarkose",
                                                                                       outfile="", tittel="",  width=600, height=700,
                                                                                       decreasing=T, terskel=10, minstekrav = NA, maal = 12, maalretn = "lav",
                                                                                       legPlass='topright', graaUt="",
                                                                                       lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                       inkl_konf=T, op_gruppe=2, whoEcog = c(0,1),
                                                                                       hastegrad_hybrid=1, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "konv_rate_rektum_malign_ecog0_1" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                     valgtVar = "konv_rate",
                                                                                     outfile="", tittel="",  width=600, height=700,
                                                                                     decreasing=T, terskel=10, minstekrav = 15, maal = 10,
                                                                                     legPlass='topright', graaUt="",
                                                                                     lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                     inkl_konf=T, op_gruppe=2, whoEcog = c(0,1),
                                                                                     hastegrad_hybrid=1, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "Anastomoselekkasje_rekt_malign_ecog_0_1" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                             valgtVar = "Anastomoselekkasje",
                                                                                             outfile="", tittel="",  width=600, height=700,
                                                                                             decreasing=T, terskel=10, minstekrav = 7, maal = 5,
                                                                                             legPlass='topright', graaUt="",
                                                                                             lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                             inkl_konf=T, op_gruppe=2, whoEcog = c(0,1),
                                                                                             hastegrad_hybrid=1, malign=1, lavDGtekst = "Lav dekningsgrad"),
           "mortalitet90_rektum_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                             valgtVar = "mortalitet90",
                                                                             outfile="", tittel="",  width=600, height=700,
                                                                             decreasing=T, terskel=10, minstekrav = NA, maal = NA,
                                                                             legPlass='topright', graaUt="",
                                                                             lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                             inkl_konf=T, op_gruppe=2,
                                                                             hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "Tilgang_lever_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                       valgtVar = "LapTilgang2",
                                                                       outfile="", tittel="",  width=600, height=700,
                                                                       decreasing=F, terskel=10, minstekrav = NA, maal = 30,
                                                                       legPlass='topleft', graaUt="",
                                                                       lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                       inkl_konf=T, op_gruppe=5,
                                                                       hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "ReLapNarkose_lever_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                            valgtVar = "ReLapNarkose",
                                                                            outfile="", tittel="",  width=600, height=700,
                                                                            decreasing=T, terskel=10, minstekrav = 10, maal = 7,
                                                                            legPlass='topleft', graaUt="",
                                                                            lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                            inkl_konf=T, op_gruppe=5,
                                                                            hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "mortalitet90_lever_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                            valgtVar = "mortalitet90",
                                                                            outfile="", tittel="",  width=600, height=700,
                                                                            decreasing=T, terskel=10, minstekrav = 5, maal = 3,
                                                                            legPlass='topleft', graaUt="",
                                                                            lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                            inkl_konf=T, op_gruppe=5,
                                                                            hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "Rekonstruksjon_whipple_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                valgtVar = "Rekonstruksjon",
                                                                                outfile="", tittel="",  width=600, height=700,
                                                                                decreasing=F, terskel=10, minstekrav = NA, maal = NA,
                                                                                legPlass='topleft', graaUt="",
                                                                                lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                inkl_konf=T, op_gruppe=6,
                                                                                hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "ReLapNarkose_whipple" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                          valgtVar = "ReLapNarkose",
                                                                          outfile="", tittel="",  width=600, height=700,
                                                                          decreasing=T, terskel=10, minstekrav = NA, maal = 20, maalretn = "lav",
                                                                          legPlass='topleft', graaUt="",
                                                                          lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                          inkl_konf=T, op_gruppe=6,
                                                                          hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "AccordionGrad_whipple_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                               valgtVar = "KumAcc",
                                                                               outfile="", tittel="",  width=600, height=700,
                                                                               decreasing=F, terskel=10, minstekrav = NA, maal = NA,
                                                                               legPlass='topleft', graaUt="",
                                                                               lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                               inkl_konf=T, op_gruppe=6,
                                                                               hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "mortalitet90_whipple_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                              valgtVar = "mortalitet90",
                                                                              outfile="", tittel="",  width=600, height=700,
                                                                              decreasing=T, terskel=10, minstekrav = 8, maal = 5,
                                                                              legPlass='topleft', graaUt="",
                                                                              lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                              inkl_konf=T, op_gruppe=6,
                                                                              hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "CR_POPF_whipple_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                         valgtVar = "CR_POPF",
                                                                         outfile="", tittel="",  width=600, height=700,
                                                                         decreasing=T, terskel=10, minstekrav = 20, maal = 15,
                                                                         legPlass='topleft', graaUt="",
                                                                         lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                         inkl_konf=T, op_gruppe=6,
                                                                         hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "Tilgang_ovrigpankreas_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                               valgtVar = "LapTilgang2",
                                                                               outfile="", tittel="",  width=600, height=700,
                                                                               decreasing=F, terskel=10, minstekrav = NA, maal = 35, maalretn = "hoy",
                                                                               legPlass='topleft', graaUt="",
                                                                               lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                               inkl_konf=T, op_gruppe=7,
                                                                               hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "Rekonstruksjon_ovrigpankreas_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                      valgtVar = "Rekonstruksjon",
                                                                                      outfile="", tittel="",  width=600, height=700,
                                                                                      decreasing=F, terskel=10, minstekrav = NA, maal = NA,
                                                                                      legPlass='topleft', graaUt="",
                                                                                      lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                      inkl_konf=T, op_gruppe=7,
                                                                                      hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "CR_POPF_ovrigpankreas_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                               valgtVar = "CR_POPF",
                                                                               outfile="", tittel="",  width=600, height=700,
                                                                               decreasing=T, terskel=10, minstekrav = 20, maal = 15,
                                                                               legPlass='topleft', graaUt="",
                                                                               lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                               inkl_konf=T, op_gruppe=7,
                                                                               hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "AccordionGrad_ovrigpankreas_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                     valgtVar = "KumAcc",
                                                                                     outfile="", tittel="",  width=600, height=700,
                                                                                     decreasing=F, terskel=10, minstekrav = NA, maal = NA,
                                                                                     legPlass='topleft', graaUt="",
                                                                                     lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                     inkl_konf=T, op_gruppe=7,
                                                                                     hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "AccordionGrad_osofagus_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                valgtVar = "KumAcc",
                                                                                outfile="", tittel="",  width=600, height=700,
                                                                                decreasing=F, terskel=10, minstekrav = NA, maal = NA,
                                                                                legPlass='topleft', graaUt="",
                                                                                lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                inkl_konf=T, op_gruppe=3,
                                                                                hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "ReLapNarkose_osofagus_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                               valgtVar = "ReLapNarkose",
                                                                               outfile="", tittel="",  width=600, height=700,
                                                                               decreasing=T, terskel=10, minstekrav = NA, maal = NA,
                                                                               legPlass='topleft', graaUt="",
                                                                               lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                               inkl_konf=T, op_gruppe=3,
                                                                               hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "mortalitet90_osofagus_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                               valgtVar = "mortalitet90",
                                                                               outfile="", tittel="",  width=600, height=700,
                                                                               decreasing=T, terskel=10, minstekrav = 8, maal = 5,
                                                                               legPlass='topleft', graaUt="",
                                                                               lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                               inkl_konf=T, op_gruppe=3,
                                                                               hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "Anastomoselekk_osofagus_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                 valgtVar = "Anastomoselekk_osofagus",
                                                                                 outfile="", width=600, height=700,
                                                                                 decreasing=T, terskel=10, minstekrav = NA, maal = 20, maalretn = "lav",
                                                                                 legPlass='topleft', graaUt="",
                                                                                 lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                 inkl_konf=T, op_gruppe=3,
                                                                                 hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad",
                                                                                 tittel=c('Anastomoselekkasje eller dyp infeksjon, eller endoskopisk', ' intervensjon for lekkasje v/ øsofaguskirurgi')),
           "Tilgang_ventrikkel_aar_v2" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                               valgtVar = "LapTilgang2",
                                                                               outfile="", tittel="",  width=600, height=700,
                                                                               decreasing=F, terskel=5, minstekrav = NA, maal = NA, maalretn = "hoy",
                                                                               legPlass='topleft', graaUt="",
                                                                               lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                               inkl_konf=T, op_gruppe=4,
                                                                               hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "AccordionGrad_ventrikkel_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                  valgtVar = "KumAcc",
                                                                                  outfile="", tittel="",  width=600, height=700,
                                                                                  decreasing=F, terskel=5, minstekrav = NA, maal = NA,
                                                                                  legPlass='topleft', graaUt="",
                                                                                  lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                  inkl_konf=T, op_gruppe=4,
                                                                                  hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "ReLapNarkose_ventrikkel_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                 valgtVar = "ReLapNarkose",
                                                                                 outfile="", tittel="",  width=600, height=700,
                                                                                 decreasing=T, terskel=5, minstekrav = NA, maal = 15, maalretn = "lav",
                                                                                 legPlass='topleft', graaUt="",
                                                                                 lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                 inkl_konf=T, op_gruppe=4,
                                                                                 hastegrad_hybrid=1, malign=99, lavDGtekst = "Lav dekningsgrad"),
           "mortalitet90_ventrikkel_aar" = norgast::norgastIndikator_rapporteket(RegData = RegData[which(RegData$Aar <= as.numeric(input$tilAar_verdi)), ],
                                                                                 valgtVar = "mortalitet90",
                                                                                 outfile="", tittel="",  width=600, height=700,
                                                                                 decreasing=T, terskel=5, minstekrav = 8, maal = 5,
                                                                                 legPlass='topleft', graaUt="",
                                                                                 lavDG= if (!is.null(input$valgtShus)) {RegData$Sykehusnavn[match(as.numeric(input$valgtShus), RegData$AvdRESH)]} else {''},
                                                                                 inkl_konf=T, op_gruppe=4,
                                                                                 hastegrad_hybrid=99, malign=99, lavDGtekst = "Lav dekningsgrad")
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























