#' UI-modul for tidsvisning andeler i NoRGast sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul tidsvisning, andeler
#'
#' @export
tidsvisning_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      width = 3,
      id = ns("id_tid_panel"),
      checkboxInput(
        inputId = ns("kun_ferdigstilte"),
        label = "Inkludér kun komplette forløp (også oppfølging ferdigstilt)",
        value = TRUE),
      uiOutput(outputId = ns('valgtVar_ui')),
      dateRangeInput(
        inputId=ns("datovalg"), label = "Dato fra og til",
        min = '2014-01-01', language = "nb",
        max = Sys.Date(),
        start  = lubridate::floor_date(lubridate::today() - lubridate::years(1),
                                       unit = "year"),
        end = Sys.Date(), separator = " til "),
      selectInput(inputId = ns("tidsenhet"),
                  label = "Velg tidsenhet",
                  choices = c('Aar', 'Mnd', 'Kvartal', 'Halvaar'),
                  selected = 'Kvartal'),
      selectInput(inputId = ns("enhetsUtvalg"), label = "Kjør rapport for",
                  choices = c('Egen avd. mot landet forøvrig'=1,
                              'Hele landet'=0, 'Egen avd.'=2)),
      uiOutput(outputId = ns('valgtShus_ui')),
      sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                  max = 120, value = c(0, 120)),
      selectInput(inputId = ns("erMann"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      uiOutput(outputId = ns('op_gruppe_ui')),
      uiOutput(outputId = ns('ncsp')),
      selectInput(inputId = ns("inkl_konf"),
                  label = "Inkluder konfidensintervall",
                  choices = c(' '=99, 'Ja'=1, 'Nei'=0)),
      selectInput(inputId = ns("elektiv"), label = "Tidspunkt for operasjonsstart",
                  choices = c('Ikke valgt'=99, 'Innenfor normalarbeidstid'=1,
                              'Utenfor normalarbeidstid'=0)),
      selectInput(inputId = ns("hastegrad"), label = "Hastegrad",
                  choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=2)),
      selectInput(inputId = ns("hastegrad_hybrid"),
                  label = "Hastegrad, hybrid (bruker hastegrad når den finnes,
                  ellers tidspkt for op.start)",
                  choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=0)),
      uiOutput(outputId = ns('BMI_ui')),
      uiOutput(outputId = ns('tilgang_utvidet_ui')),
      sliderInput(inputId = ns("PRS"), label = "mE-PASS", min = 0,
                  max = 2.2, value = c(0, 2.2), step = 0.05),
      uiOutput(outputId = ns('ASA_ui')),
      selectInput(inputId = ns("modGlasgow"), label = "Modified Glasgow score",
                  choices = 0:2, multiple = TRUE),
      uiOutput(outputId = ns('whoEcog_ui')),
      selectInput(inputId = ns("forbehandling"),
                  label = "Onkologisk forbehandling", multiple = TRUE,
                  choices = c('Cytostatika'=1, 'Stråleterapi'=2,
                              'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
      selectInput(inputId = ns("malign"), label = "Diagnose",
                  choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(tabsetPanel(id = ns("tab"),
                          tabPanel("Figur", value = "fig",
                                   plotOutput(ns("fig_andel_tid"), height="auto"),
                                   downloadButton(ns("lastNedBilde_tid"), "Last ned figur")),
                          tabPanel("Tabell", value = "tab",
                                   uiOutput(ns("utvalg_tid")),
                                   tableOutput(ns("Tabell_tid")),
                                   downloadButton(ns("lastNed_tid"), "Last ned tabell"))
    )
    )

  )
}

#' Server-modul for tidsvisning andeler i NoRGast sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul tidsvisning, andeler
#'
#' @export
tidsvisning <- function(input, output, session, reshID, RegData, userRole,
                        hvd_session, BrValg){

  observeEvent(input$reset_input, {
    shinyjs::reset("id_tid_panel")
  })

  observe(
    if (userRole != 'SC') {
      shinyjs::hide(id = 'valgtShus')
    })

  output$ncsp <- renderUI({
    ns <- session$ns
    if (!is.null(input$op_gruppe)) {
      selectInput(inputId = ns("ncsp_verdi"), label = "NCSP koder  (velg en eller flere)",
                  choices = if (!is.null(input$op_gruppe)) {
                    setNames(substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe)])), 1, 5),
                             sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe)])))
                  }, multiple = TRUE)
    }
  })

  output$valgtShus_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                choices = BrValg$sykehus, multiple = TRUE)
  })

  output$valgtVar_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("valgtVar"), label = "Velg variabel",
                choices = BrValg$varvalg_andel)
  })

  output$op_gruppe_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
                choices = BrValg$reseksjonsgrupper, multiple = TRUE)
  })

  output$tilgang_utvidet_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("tilgang_utvidet"),
                label = "Tilgang i abdomen (inkl. robotassistanse)",
                choices = BrValg$tilgang_utvidet, multiple = TRUE)
  })

  output$BMI_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("BMI"), label = "BMI",
                choices = BrValg$bmi_valg, multiple = TRUE)
  })

  output$ASA_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("ASA"), label = "ASA-grad",
                choices = BrValg$ASA_valg, multiple = TRUE)
  })

  output$whoEcog_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("whoEcog"), label = "WHO ECOG score",
                choices = BrValg$whoEcog_valg, multiple = TRUE)
  })




  output$fig_andel_tid <- renderPlot({
    norgast::NorgastFigAndelTid(RegData,
                                valgtVar=input$valgtVar,
                                datoFra = input$datovalg[1],
                                datoTil = input$datovalg[2],
                                reshID = reshID,
                                enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                minald=as.numeric(input$alder[1]),
                                maxald=as.numeric(input$alder[2]),
                                valgtShus = fiksNULL(input$valgtShus),
                                op_gruppe = fiksNULL(input$op_gruppe),
                                ncsp = fiksNULL(input$ncsp_verdi),
                                BMI = fiksNULL(input$BMI),
                                # tilgang = fiksNULL(input$tilgang),
                                tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
                                minPRS = as.numeric(input$PRS[1]),
                                maxPRS = as.numeric(input$PRS[2]),
                                ASA = fiksNULL(input$ASA),
                                whoEcog = fiksNULL(input$whoEcog),
                                forbehandling = fiksNULL(input$forbehandling),
                                modGlasgow = fiksNULL(input$modGlasgow),
                                malign = as.numeric(input$malign),
                                erMann = as.numeric(input$erMann),
                                elektiv = as.numeric(input$elektiv),
                                tidsenhet = fiksNULL(input$tidsenhet, 'Aar'),
                                inkl_konf = fiksNULL(input$inkl_konf, 99),
                                hastegrad=as.numeric(input$hastegrad),
                                kun_ferdigstilte = input$kun_ferdigstilte,
                                hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
  }, width = 700, height = 700)

  tabellReagerTid <- reactive({
    TabellData_Tid <- norgast::NorgastFigAndelTid(RegData,
                                                  valgtVar=input$valgtVar,
                                                  datoFra = input$datovalg[1],
                                                  datoTil = input$datovalg[2],
                                                  reshID = reshID,
                                                  enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                                  minald=as.numeric(input$alder[1]),
                                                  maxald=as.numeric(input$alder[2]),
                                                  valgtShus = fiksNULL(input$valgtShus),
                                                  op_gruppe = fiksNULL(input$op_gruppe),
                                                  ncsp = fiksNULL(input$ncsp_verdi),
                                                  BMI = fiksNULL(input$BMI),
                                                  # tilgang = fiksNULL(input$tilgang),
                                                  tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
                                                  minPRS = as.numeric(input$PRS[1]),
                                                  maxPRS = as.numeric(input$PRS[2]),
                                                  ASA = fiksNULL(input$ASA),
                                                  whoEcog = fiksNULL(input$whoEcog),
                                                  forbehandling = fiksNULL(input$forbehandling),
                                                  modGlasgow = fiksNULL(input$modGlasgow),
                                                  malign = as.numeric(input$malign),
                                                  erMann = as.numeric(input$erMann),
                                                  elektiv = as.numeric(input$elektiv),
                                                  tidsenhet = fiksNULL(input$tidsenhet, 'Aar'),
                                                  inkl_konf = fiksNULL(input$inkl_konf, 99),
                                                  hastegrad=as.numeric(input$hastegrad),
                                                  kun_ferdigstilte = input$kun_ferdigstilte,
                                                  hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
  })

  output$utvalg_tid <- renderUI({
    TabellData <- tabellReagerTid()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})



  output$Tabell_tid <- function() {

    utdata <- tabellReagerTid()
    if (input$enhetsUtvalg == 1) {
      Tabell_tid <- dplyr::tibble(Tidsperiode = utdata$Tidtxt, Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                           N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                           Konf.int.ovre = utdata$KonfInt$Konf[2,], Antall2 = round(utdata$Andeler$AndelRest*utdata$NTid$NTidRest/100),
                           N2 = utdata$NTid$NTidRest, Andel2 = utdata$Andeler$AndelRest, Konf.int.nedre2 = utdata$KonfInt$KonfRest[1,],
                           Konf.int.ovre2 = utdata$KonfInt$KonfRest[2,])
      names(Tabell_tid) <- c('Tidsperiode', 'Antall', 'N', 'Andel (%)', 'KI_nedre', 'KI_ovre', 'Antall', 'N', 'Andel (%)',
                             'KI_nedre', 'KI_ovre')
      Tabell_tid %>% knitr::kable("html", digits = c(0,0,0,1,1,1,0,0,1,1,1)) %>%
        kableExtra::kable_styling("hover", full_width = F) %>%
        kableExtra::add_header_above(c(" ", "Din avdeling" = 5, "Landet forøvrig" = 5))
    } else {
      Tabell_tid <- dplyr::tibble(Tidsperiode = utdata$Tidtxt,
                           Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                           N = utdata$NTid$NTidHoved, 'Andel (%)'= utdata$Andeler$AndelHoved, KI_nedre = utdata$KonfInt$Konf[1,],
                           KI_ovre = utdata$KonfInt$Konf[2,])
      Tabell_tid %>%
        knitr::kable("html", digits = c(0,0,0,1,1,1)) %>%
        kableExtra::kable_styling("hover", full_width = F)
    }
  }

  output$lastNed_tid <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, '_tid', Sys.time(), '.csv')
    },
    content = function(file){
      utdata <- tabellReagerTid()
      if (input$enhetsUtvalg == 1) {
        Tabell_tid <- dplyr::tibble(Tidsperiode = utdata$Tidtxt, Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                             N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                             Konf.int.ovre = utdata$KonfInt$Konf[2,], Antall2 = round(utdata$Andeler$AndelRest*utdata$NTid$NTidRest/100),
                             N2 = utdata$NTid$NTidRest, Andel2 = utdata$Andeler$AndelRest, Konf.int.nedre2 = utdata$KonfInt$KonfRest[1,],
                             Konf.int.ovre2 = utdata$KonfInt$KonfRest[2,])
      } else {
        Tabell_tid <- dplyr::tibble(Tidsperiode = utdata$Tidtxt,
                             Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                             N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                             Konf.int.ovre = utdata$KonfInt$Konf[2,])
      }
      write.csv3(Tabell_tid, file, row.names = F)
    }
  )

  output$lastNedBilde_tid <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, '_tid', Sys.time(), '.', input$bildeformat)
    },
    content = function(file){
      norgast::NorgastFigAndelTid(RegData,
                                  valgtVar=input$valgtVar,
                                  datoFra = input$datovalg[1],
                                  datoTil = input$datovalg[2],
                                  reshID = reshID,
                                  enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                  minald=as.numeric(input$alder[1]),
                                  maxald=as.numeric(input$alder[2]),
                                  valgtShus = fiksNULL(input$valgtShus),
                                  op_gruppe = fiksNULL(input$op_gruppe),
                                  ncsp = fiksNULL(input$ncsp_verdi),
                                  BMI = fiksNULL(input$BMI),
                                  # tilgang = fiksNULL(input$tilgang),
                                  tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
                                  minPRS = as.numeric(input$PRS[1]),
                                  maxPRS = as.numeric(input$PRS[2]),
                                  ASA = fiksNULL(input$ASA),
                                  hastegrad=as.numeric(input$hastegrad),
                                  whoEcog = fiksNULL(input$whoEcog),
                                  forbehandling = fiksNULL(input$forbehandling),
                                  modGlasgow = fiksNULL(input$modGlasgow),
                                  malign = as.numeric(input$malign),
                                  erMann = as.numeric(input$erMann),
                                  elektiv = as.numeric(input$elektiv),
                                  tidsenhet = fiksNULL(input$tidsenhet, 'Aar'),
                                  inkl_konf = fiksNULL(input$inkl_konf, 99),
                                  kun_ferdigstilte = input$kun_ferdigstilte,
                                  outfile = file,
                                  hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
    }
  )


  shiny::observe({
    if (rapbase::isRapContext()) {
      if (req(input$tab) == "fig") {
        mld_fordeling <- paste0(
          "NoRGast: Figur - tidsvisning, variabel - ",
          input$valgtVar)
      }
      if (req(input$tab) == "tab") {
        mld_fordeling <- paste(
          "NoRGast: tabell - tidsvisning variabel - ",
          input$valgtVar)
      }
      rapbase::repLogger(
        session = hvd_session,
        msg = mld_fordeling
      )
      shinyjs::onclick(
        "lastNedBilde_tid",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NoRGast: nedlasting figur - tidsvisning, variabel -",
            input$valgtVar
          )
        )
      )
      shinyjs::onclick(
        "lastNed_tid",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NoRGast: nedlasting tabell - tidsvisning, variabel -",
            input$valgtVar
          )
        )
      )
    }
  })



}





