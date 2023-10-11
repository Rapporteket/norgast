#' UI-modul for sykehusvise andeler i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul sykehusvisning, andeler
#'
#' @export
sykehusvisning_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      width = 3,
      id = ns("id_shus_panel"),
      checkboxInput(inputId = ns("kun_ferdigstilte"), label = "Inkludér kun
                    komplette forløp (også oppfølging ferdigstilt)", value = TRUE),
      uiOutput(outputId = ns('valgtVar_ui')),
      uiOutput(outputId = ns('valgtVar_gjsn_ui')),
      uiOutput(outputId = ns('valgtVar_andel_stabel_ui')),
      dateRangeInput(
        inputId=ns("datovalg"),
        label = "Dato fra og til",
        min = '2014-01-01',
        language = "nb",
        max = Sys.Date(),
        start  = lubridate::floor_date(lubridate::today() -
                                         lubridate::years(1), unit = "year"),
        end = Sys.Date(), separator = " til "),
      sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                  max = 120, value = c(0, 120)),
      selectInput(inputId = ns("erMann"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      uiOutput(outputId = ns('op_gruppe_ui')),
      uiOutput(outputId = ns('ncsp')),
      selectInput(inputId = ns("inkl_konf"),
                  label = "Inkluder konfidensintervall",
                  choices = c(' '=99, 'Ja'=1, 'Nei'=0)),
      selectInput(inputId = ns("elektiv"),
                  label = "Tidspunkt for operasjonsstart",
                  choices = c('Ikke valgt'=99,
                              'Innenfor normalarbeidstid'=1,
                              'Utenfor normalarbeidstid'=0)),
      selectInput(inputId = ns("hastegrad"), label = "Hastegrad",
                  choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=2)),
      selectInput(inputId = ns("hastegrad_hybrid"),
                  label = "Hastegrad, hybrid (bruker hastegrad når den finnes, ellers tidspkt. for op.start)",
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
                  choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
      selectInput(inputId = ns("malign"), label = "Diagnose",
                  choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(tabsetPanel(
      id = ns("tabs_sykehusvisning"),
      tabPanel("Figur, andeler", value = "fig_andel",
               plotOutput(ns("fig_andel_grvar"), height="auto"),
               downloadButton(ns("lastNedBilde_sykehus_andel"), "Last ned figur")),
      tabPanel("Tabell, andeler", value = "tab_andel",
               uiOutput(ns("utvalg_sykehus_andel")),
               tableOutput(ns("Tabell_sykehus_andel")),
               downloadButton(ns("lastNed_sykehus_andel"), "Last ned tabell")),
      tabPanel("Figur, gjennomsnitt",  value = "fig_gjsn",
               plotOutput(ns("fig_gjsn_grvar"), height="auto"),
               downloadButton(ns("lastNedBilde_sykehus_gjsn"), "Last ned figur")),
      tabPanel("Tabell, gjennomsnitt",  value = "tab_gjsn",
               uiOutput(ns("utvalg_sykehus_gjsn")),
               tableOutput(ns("Tabell_sykehus_gjsn")),
               downloadButton(ns("lastNed_sykehus_gjsn"), "Last ned tabell")),
      tabPanel("Figur, andeler i stabel",  value = "fig_andel_stabel",
               plotOutput(ns("fig_andel_grvar_stabel"), height="auto"),
               downloadButton(ns("lastNedBilde_sykehus_andel_stabel"), "Last ned figur")),
      tabPanel("Tabell, andeler i stabel",  value = "tab_andel_stabel",
               uiOutput(ns("utvalg_sykehus_andel_stabel")),
               tableOutput(ns("Tabell_sykehus_andel_stabel")),
               downloadButton(ns("lastNedStabelTabell"), "Last ned tabell"))
    )
    )

  )
}

#' Server-modul for sykehusvise andeler i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul sykehusvisning, andeler
#'
#' @export
sykehusvisning <- function(input, output, session, reshID, RegData, hvd_session, BrValg){

  observeEvent(input$reset_input, {
    shinyjs::reset("id_shus_panel")
  })

  observe(
    if (!is.null(input$tabs_sykehusvisning)) {
      if (input$tabs_sykehusvisning %in%  c("fig_andel_stabel", "tab_andel_stabel")){
        shinyjs::hide(id = 'valgtVar_ui')
        shinyjs::hide(id = 'valgtVar_gjsn_ui')
        shinyjs::show(id = 'valgtVar_andel_stabel_ui')
        shinyjs::hide(id = 'inkl_konf')
      }
      if (input$tabs_sykehusvisning %in%  c("fig_andel", "tab_andel")) {
        shinyjs::hide(id = 'valgtVar_andel_stabel_ui')
        shinyjs::hide(id = 'valgtVar_gjsn_ui')
        shinyjs::show(id = 'valgtVar_ui')
        shinyjs::show(id = 'inkl_konf')
      }
      if (input$tabs_sykehusvisning %in%  c("fig_gjsn", "tab_gjsn")) {
        shinyjs::hide(id = 'valgtVar_andel_stabel_ui')
        shinyjs::show(id = 'valgtVar_gjsn_ui')
        shinyjs::hide(id = 'valgtVar_ui')
        shinyjs::hide(id = 'inkl_konf')
      }

    }
  )


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

  output$valgtVar_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("valgtVar"), label = "Velg variabel",
                choices = BrValg$varvalg_andel)
  })

  output$valgtVar_gjsn_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("valgtVar_gjsn"), label = "Velg variabel",
                choices = BrValg$varvalg_gjsn)
  })

  output$valgtVar_andel_stabel_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("valgtVar_andel_stabel"), label = "Velg variabel",
                choices = BrValg$varvalg_andel_stabel)
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





  ############### Andeler #######################################################

  output$fig_andel_grvar <- renderPlot({
    norgast::NorgastFigAndelerGrVar(
      RegData,
      valgtVar = if (!is.null(input$valgtVar)) {input$valgtVar} else {'Anastomoselekkasje'},
      datoFra = input$datovalg[1],
      datoTil = input$datovalg[2],
      minald=as.numeric(input$alder[1]),
      maxald=as.numeric(input$alder[2]),
      erMann=as.numeric(input$erMann),
      inkl_konf = fiksNULL(input$inkl_konf),
      malign = as.numeric(input$malign),
      Ngrense=10,
      elektiv=as.numeric(input$elektiv),
      BMI = fiksNULL(input$BMI),
      # tilgang = fiksNULL(input$tilgang),
      tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
      minPRS = as.numeric(input$PRS[1]),
      maxPRS = as.numeric(input$PRS[2]),
      ASA= fiksNULL(input$ASA),
      whoEcog = fiksNULL(input$whoEcog),
      forbehandling = fiksNULL(input$forbehandling),
      modGlasgow = fiksNULL(input$modGlasgow),
      op_gruppe = fiksNULL(input$op_gruppe),
      ncsp = fiksNULL(input$ncsp_verdi),
      hastegrad = as.numeric(input$hastegrad),
      kun_ferdigstilte = input$kun_ferdigstilte,
      hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
  }, width = 700, height = 700)

  tabellReagerSykehusAndel <- reactive({
    TabellData <- norgast::NorgastFigAndelerGrVar(
      RegData,
      # valgtVar=input$valgtVar,
      valgtVar = if (!is.null(input$valgtVar)) {input$valgtVar} else {'Anastomoselekkasje'},
      datoFra = input$datovalg[1],
      datoTil = input$datovalg[2],
      minald=as.numeric(input$alder[1]),
      maxald=as.numeric(input$alder[2]),
      erMann=as.numeric(input$erMann),
      inkl_konf = fiksNULL(input$inkl_konf),
      malign = as.numeric(input$malign),
      Ngrense=10,
      elektiv=as.numeric(input$elektiv),
      BMI = fiksNULL(input$BMI),
      # tilgang = fiksNULL(input$tilgang),
      tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
      minPRS = as.numeric(input$PRS[1]),
      maxPRS = as.numeric(input$PRS[2]),
      ASA= fiksNULL(input$ASA),
      whoEcog = fiksNULL(input$whoEcog),
      forbehandling = fiksNULL(input$forbehandling),
      modGlasgow = fiksNULL(input$modGlasgow),
      op_gruppe = fiksNULL(input$op_gruppe),
      ncsp = fiksNULL(input$ncsp_verdi),
      hastegrad = as.numeric(input$hastegrad),
      kun_ferdigstilte = input$kun_ferdigstilte,
      hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
  })

  output$utvalg_sykehus_andel <- renderUI({
    TabellData <- tabellReagerSykehusAndel()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  output$Tabell_sykehus_andel <- function() {
    utdata <- tabellReagerSykehusAndel()
    Tabell <- dplyr::tibble(Avdeling = names(utdata$Nvar), Antall=utdata$Nvar,
                     'Antall totalt'=utdata$Ngr,
                     'Andel (%)' = as.numeric(utdata$Nvar/utdata$Ngr*100),
                     KI_nedre=utdata$KI[1,], KI_ovre=utdata$KI[2,])
    Tabell[utdata$Andeler==-0.001, 2:6] <- NA
    Tabell <- Tabell[dim(Tabell)[1]:1, ]
    Tabell %>% knitr::kable("html", digits = c(0,0,0,1,1,1)) %>%
      kableExtra::kable_styling("hover", full_width = F)
  }

  output$lastNed_sykehus_andel <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, '_sykehus_andel_', Sys.time(), '.csv')
    },
    content = function(file){
      utdata <- tabellReagerSykehusAndel()
      Tabell <- dplyr::tibble(Avdeling = names(utdata$Nvar), Antall=utdata$Nvar,
                       'Antall totalt'=utdata$Ngr,
                       'Andel (%)' = as.numeric(utdata$Nvar/utdata$Ngr*100),
                       KI_nedre=utdata$KI[1,], KI_ovre=utdata$KI[2,])
      Tabell[utdata$Andeler==-0.001, 2:6] <- NA
      Tabell <- Tabell[dim(Tabell)[1]:1, ]
      write.csv3(Tabell, file, row.names = F, na = '')
    }
  )

  output$lastNedBilde_sykehus_andel <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, '_sykehus_andel_', Sys.time(), '.', input$bildeformat)
    },
    content = function(file){
      norgast::NorgastFigAndelerGrVar(
        RegData,
        # valgtVar=input$valgtVar,
        valgtVar = if (!is.null(input$valgtVar)) {input$valgtVar} else {'Anastomoselekkasje'},
        datoFra = input$datovalg[1],
        datoTil = input$datovalg[2],
        minald=as.numeric(input$alder[1]),
        maxald=as.numeric(input$alder[2]),
        erMann=as.numeric(input$erMann),
        inkl_konf = fiksNULL(input$inkl_konf),
        malign = as.numeric(input$malign),
        Ngrense=10,
        elektiv=as.numeric(input$elektiv),
        BMI = fiksNULL(input$BMI),
        # tilgang = fiksNULL(input$tilgang),
        tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
        minPRS = as.numeric(input$PRS[1]),
        maxPRS = as.numeric(input$PRS[2]),
        ASA= fiksNULL(input$ASA),
        whoEcog = fiksNULL(input$whoEcog),
        forbehandling = fiksNULL(input$forbehandling),
        modGlasgow = fiksNULL(input$modGlasgow),
        op_gruppe = fiksNULL(input$op_gruppe),
        ncsp = fiksNULL(input$ncsp_verdi),
        outfile = file,
        hastegrad = as.numeric(input$hastegrad),
        kun_ferdigstilte = input$kun_ferdigstilte,
        hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
    }
  )

  ############### Andeler i stabler #######################################################

  output$fig_andel_grvar_stabel <- renderPlot({
    norgast::NorgastFigAndelStabelGrVar(
      RegData,
      valgtVar = if (!is.null(input$valgtVar_andel_stabel)) {
        input$valgtVar_andel_stabel
        } else {'ModGlasgowScore'},
      datoFra = input$datovalg[1],
      datoTil = input$datovalg[2],
      minald=as.numeric(input$alder[1]),
      maxald=as.numeric(input$alder[2]),
      erMann=as.numeric(input$erMann),
      malign = as.numeric(input$malign),
      Ngrense=10,
      elektiv=as.numeric(input$elektiv),
      BMI = fiksNULL(input$BMI),
      tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
      minPRS = as.numeric(input$PRS[1]),
      maxPRS = as.numeric(input$PRS[2]),
      ASA= fiksNULL(input$ASA),
      whoEcog = fiksNULL(input$whoEcog),
      forbehandling = fiksNULL(input$forbehandling),
      modGlasgow = fiksNULL(input$modGlasgow),
      op_gruppe = fiksNULL(input$op_gruppe),
      ncsp = fiksNULL(input$ncsp_verdi),
      hastegrad = as.numeric(input$hastegrad),
      kun_ferdigstilte = input$kun_ferdigstilte,
      hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
  }, width = 700, height = 700)


  tabellReagerSykehusAndelStabel <- reactive({
    TabellData <- norgast::NorgastFigAndelStabelGrVar(
      RegData,
      valgtVar = if (!is.null(input$valgtVar_andel_stabel)) {input$valgtVar_andel_stabel} else {'ModGlasgowScore'},
      datoFra = input$datovalg[1],
      datoTil = input$datovalg[2],
      minald=as.numeric(input$alder[1]),
      maxald=as.numeric(input$alder[2]),
      erMann=as.numeric(input$erMann),
      malign = as.numeric(input$malign),
      Ngrense=10,
      elektiv=as.numeric(input$elektiv),
      BMI = fiksNULL(input$BMI),
      # tilgang = fiksNULL(input$tilgang),
      tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
      minPRS = as.numeric(input$PRS[1]),
      maxPRS = as.numeric(input$PRS[2]),
      ASA= fiksNULL(input$ASA),
      whoEcog = fiksNULL(input$whoEcog),
      forbehandling = fiksNULL(input$forbehandling),
      modGlasgow = fiksNULL(input$modGlasgow),
      op_gruppe = fiksNULL(input$op_gruppe),
      ncsp = fiksNULL(input$ncsp_verdi),
      hastegrad = as.numeric(input$hastegrad),
      kun_ferdigstilte = input$kun_ferdigstilte,
      hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
  })

  output$utvalg_sykehus_andel_stabel <- renderUI({
    TabellData <- tabellReagerSykehusAndelStabel()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  output$Tabell_sykehus_andel_stabel <- function() {
    TabellData <- tabellReagerSykehusAndelStabel()
    Tabell <- dplyr::bind_cols(TabellData$antall, TabellData$andeler[, 2:(dim(TabellData$andeler)[2]-1)],
                        .name_repair = "minimal")
    names(Tabell)[dim(Tabell)[2]/2 + 1] <- 'N'

    Tabell %>% knitr::kable("html", digits = c(rep(0, dim(Tabell)[2]/2 + 1), rep(1, dim(Tabell)[2]/2 - 1))) %>%
      kableExtra::kable_styling("hover", full_width = F) %>%
      kableExtra::add_header_above(c(" ", "Antall" = dim(Tabell)[2]/2 - 1, " ", "Andel (%)" = dim(Tabell)[2]/2 - 1))
  }



  output$lastNedStabelTabell <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_andel_stabel, '_sykehus_stabel_', Sys.time(), '.csv')
    },
    content = function(file){
      TabellData <- tabellReagerSykehusAndelStabel()
      Tabell <- dplyr::bind_cols(TabellData$antall, TabellData$andeler[, 2:(dim(TabellData$andeler)[2]-1)],
                          .name_repair = "minimal")
      names(Tabell)[dim(Tabell)[2]/2 + 1] <- 'N'
      write.csv3(Tabell, file, row.names = F, na = '')
    }
  )




  output$lastNedBilde_sykehus_andel_stabel <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_andel_stabel, '_stabel', Sys.time(), '.', input$bildeformat)
    },
    content = function(file){
      norgast::NorgastFigAndelStabelGrVar(
        RegData,
        # valgtVar=input$valgtVar_andel_stabel,
        valgtVar = if (!is.null(input$valgtVar_andel_stabel)) {input$valgtVar_andel_stabel} else {'ModGlasgowScore'},
        datoFra = input$datovalg[1],
        datoTil = input$datovalg[2],
        minald=as.numeric(input$alder[1]),
        maxald=as.numeric(input$alder[2]),
        erMann=as.numeric(input$erMann),
        malign = as.numeric(input$malign),
        Ngrense=10,
        elektiv=as.numeric(input$elektiv),
        BMI = fiksNULL(input$BMI),
        tilgang = fiksNULL(input$tilgang),
        tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
        minPRS = as.numeric(input$PRS[1]),
        maxPRS = as.numeric(input$PRS[2]),
        ASA= fiksNULL(input$ASA),
        whoEcog = fiksNULL(input$whoEcog),
        forbehandling = fiksNULL(input$forbehandling),
        modGlasgow = fiksNULL(input$modGlasgow),
        op_gruppe = fiksNULL(input$op_gruppe),
        ncsp = fiksNULL(input$ncsp_verdi),
        outfile = file,
        hastegrad = as.numeric(input$hastegrad),
        kun_ferdigstilte = input$kun_ferdigstilte,
        hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
    }
  )

  #################### Gjennomsnitt #################################################

  output$fig_gjsn_grvar <- renderPlot({
    norgast::NorgastFigGjsnGrVar(
      RegData,
      # valgtVar=input$valgtVar_gjsn,
      valgtVar = if (!is.null(input$valgtVar_gjsn)) {input$valgtVar_gjsn} else {'BMI'},
      datoFra = input$datovalg[1],
      datoTil = input$datovalg[2],
      minald=as.numeric(input$alder[1]),
      maxald=as.numeric(input$alder[2]),
      erMann=as.numeric(input$erMann),
      # inkl_konf = fiksNULL(input$inkl_konf),
      malign = as.numeric(input$malign),
      Ngrense=10,
      elektiv=as.numeric(input$elektiv),
      BMI = fiksNULL(input$BMI),
      # tilgang = fiksNULL(input$tilgang),
      tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
      minPRS = as.numeric(input$PRS[1]),
      maxPRS = as.numeric(input$PRS[2]),
      ASA= fiksNULL(input$ASA),
      whoEcog = fiksNULL(input$whoEcog),
      forbehandling = fiksNULL(input$forbehandling),
      modGlasgow = fiksNULL(input$modGlasgow),
      op_gruppe = fiksNULL(input$op_gruppe),
      ncsp = fiksNULL(input$ncsp_verdi),
      hastegrad = as.numeric(input$hastegrad),
      kun_ferdigstilte = input$kun_ferdigstilte,
      hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
  }, width = 700, height = 700)

  tabellReagerSykehusGjsn <- reactive({
    TabellData <- norgast::NorgastFigGjsnGrVar(
      RegData,
      # valgtVar=input$valgtVar_gjsn,
      valgtVar = if (!is.null(input$valgtVar_gjsn)) {input$valgtVar_gjsn} else {'BMI'},
      datoFra = input$datovalg[1],
      datoTil = input$datovalg[2],
      minald=as.numeric(input$alder[1]),
      maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
      # inkl_konf = fiksNULL(input$inkl_konf),
      malign = as.numeric(input$malign),
      Ngrense=10,
      elektiv=as.numeric(input$elektiv),
      BMI = fiksNULL(input$BMI),
      # tilgang = fiksNULL(input$tilgang),
      tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
      minPRS = as.numeric(input$PRS[1]),
      maxPRS = as.numeric(input$PRS[2]),
      ASA= fiksNULL(input$ASA),
      whoEcog = fiksNULL(input$whoEcog),
      forbehandling = fiksNULL(input$forbehandling),
      modGlasgow = fiksNULL(input$modGlasgow),
      op_gruppe = fiksNULL(input$op_gruppe),
      ncsp = fiksNULL(input$ncsp_verdi),
      hastegrad = as.numeric(input$hastegrad),
      kun_ferdigstilte = input$kun_ferdigstilte,
      hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
  })

  output$utvalg_sykehus_gjsn <- renderUI({
    TabellData <- tabellReagerSykehusGjsn()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  output$Tabell_sykehus_gjsn <- function() {
    utdata <- tabellReagerSykehusGjsn()
    Tabell <- dplyr::as_tibble(utdata$res, rownames='Avdeling')
    Tabell[Tabell$N < 10, 2:4] <- NA
    Tabell <- Tabell[order(Tabell$Gjsn, decreasing = T, na.last = T), ]
    Tabell %>% knitr::kable("html", digits = c(0,1,1,1,0)) %>%
      kableExtra::kable_styling("hover", full_width = F)
  }

  output$lastNed_sykehus_gjsn <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_gjsn, '_sykehus_gjsn_', Sys.time(), '.csv')
    },
    content = function(file){
      utdata <- tabellReagerSykehusGjsn()
      Tabell <- dplyr::as_tibble(utdata$res, rownames='Sykehusnavn')
      Tabell[Tabell$N < 10, 2:4] <- NA
      Tabell <- Tabell[order(Tabell$Gjsn, decreasing = T, na.last = T), ]
      write.csv3(Tabell, file, row.names = F, na = '')
    }
  )

  output$lastNedBilde_sykehus_gjsn <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_gjsn, '_sykehus_gjsn_', Sys.time(), '.', input$bildeformat)
    },
    content = function(file){
      norgast::NorgastFigGjsnGrVar(
        RegData,
        # valgtVar=input$valgtVar_gjsn,
        valgtVar = if (!is.null(input$valgtVar_gjsn)) {input$valgtVar_gjsn} else {'BMI'},
        datoFra = input$datovalg[1],
        datoTil = input$datovalg[2],
        minald=as.numeric(input$alder[1]),
        maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
        # inkl_konf = fiksNULL(input$inkl_konf),
        malign = as.numeric(input$malign),
        Ngrense=10,
        hastegrad = as.numeric(input$hastegrad),
        elektiv=as.numeric(input$elektiv),
        BMI = fiksNULL(input$BMI),
        # tilgang = fiksNULL(input$tilgang),
        tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
        minPRS = as.numeric(input$PRS[1]),
        maxPRS = as.numeric(input$PRS[2]),
        ASA= fiksNULL(input$ASA),
        whoEcog = fiksNULL(input$whoEcog),
        forbehandling = fiksNULL(input$forbehandling),
        modGlasgow = fiksNULL(input$modGlasgow),
        op_gruppe = fiksNULL(input$op_gruppe),
        ncsp = fiksNULL(input$ncsp_verdi),
        kun_ferdigstilte = input$kun_ferdigstilte,
        outfile = file,
        hastegrad_hybrid = as.numeric(input$hastegrad_hybrid))
    }
  )

  shiny::observe({
    if (rapbase::isRapContext()) {
      if (req(input$tabs_sykehusvisning) == "fig_andel") {
        mld_fordeling <- paste0(
          "NORGAST: Figur - sykehusvisning andeler, variabel - ",
          input$valgtVar)
      }
      if (req(input$tabs_sykehusvisning) == "tab_andel") {
        mld_fordeling <- paste(
          "NORGAST: tabell - sykehusvisning andeler. variabel - ",
          input$valgtVar)
      }
      if (req(input$tabs_sykehusvisning) == "fig_andel_stabel") {
        mld_fordeling <- paste0(
          "NORGAST: Figur - sykehusvisning stablede andeler, variabel - ",
          input$valgtVar_andel_stabel)
      }
      if (req(input$tabs_sykehusvisning) == "tab_andel_stabel") {
        mld_fordeling <- paste(
          "NORGAST: tabell - sykehusvisning stablede andeler. variabel - ",
          input$valgtVar_andel_stabel)
      }
      if (req(input$tabs_sykehusvisning) == "fig_gjsn") {
        mld_fordeling <- paste0(
          "NORGAST: Figur - sykehusvisning gj.snitt, variabel - ",
          input$valgtVar_gjsn)
      }
      if (req(input$tabs_sykehusvisning) == "tab_gjsn") {
        mld_fordeling <- paste(
          "NORGAST: tabell - sykehusvisning gj.snitt. variabel - ",
          input$valgtVar_gjsn)
      }
      rapbase::repLogger(
        session = hvd_session,
        msg = mld_fordeling
      )
      shinyjs::onclick(
        "lastNedBilde_sykehus_andel",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NORGAST: nedlasting figur - sykehusvisning andel. variabel -",
            input$valgtVar
          )
        )
      )
      shinyjs::onclick(
        "lastNed_sykehus_andel",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NORGAST: nedlasting tabell - sykehusvisning andel. variabel -",
            input$valgtVar
          )
        )
      )
      shinyjs::onclick(
        "lastNedBilde_sykehus_gjsn",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NORGAST: nedlasting figur - sykehusvisning gj.snitt. variabel -",
            input$valgtVar_gjsn
          )
        )
      )
      shinyjs::onclick(
        "lastNed_sykehus_gjsn",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NORGAST: nedlasting tabell - sykehusvisning gj.snitt. variabel -",
            input$valgtVar_gjsn
          )
        )
      )
      shinyjs::onclick(
        "lastNedBilde_sykehus_andel_stabel",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NORGAST: nedlasting figur - sykehusvisning stablet andel. variabel -",
            input$valgtVar_andel_stabel
          )
        )
      )
      shinyjs::onclick(
        "lastNedStabelTabell",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NORGAST: nedlasting tabell - sykehusvisning stablet andel. variabel -",
            input$valgtVar_andel_stabel
          )
        )
      )


    }


  })



}
