# Modul for fordelingsfigurer i NoRGast sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @return Modul fordelingsfigur
#
fordelingsfig_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      # shinyjs::useShinyjs(),
      id = ns("id_fordeling_panel"),
      selectInput(inputId = ns("valgtVar"), label = "Velg variabel",
                  choices = BrValg$varvalg),
      bsTooltip(id = ns("valgtVar"), title= 'Velg hvilken variabel du vil generere fordelingsfigur for.'),
      dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01',
                     max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), language = "nb", separator = " til "),
      selectInput(inputId = ns("enhetsUtvalg"), label = "Kjør rapport for",
                  choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1, 'Egen avd.'=2)),
      selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                  choices = BrValg$sykehus, multiple = TRUE),
      selectInput(inputId = ns("tilgang"), label = "Tilgang i abdomen (velg en eller flere)", choices = BrValg$tilgang_valg, multiple = TRUE),
      sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                  max = 120, value = c(0, 120)),
      selectInput(inputId = ns("erMann"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("elektiv"), label = "Tidspunkt for operasjonsstart",
                  choices = c('Ikke valgt'=99, 'Innenfor normalarbeidstid'=1, 'Utenfor normalarbeidstid'=0)),
      selectInput(inputId = ns("hastegrad"), label = "Hastegrad",
                  choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=2)),
      # shinyjs::hidden(
      #   div(
      #     id = ns("avansert"),
      selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
                  choices = BrValg$reseksjonsgrupper, multiple = TRUE),
      uiOutput(outputId = ns('ncsp')),
      selectInput(inputId = ns("BMI"), label = "BMI", choices = BrValg$bmi_valg, multiple = TRUE),
      sliderInput(inputId=ns("PRS"), label = "mE-PASS", min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
      selectInput(inputId = ns("ASA"), label = "ASA-grad", choices = BrValg$ASA_valg, multiple = TRUE),
      selectInput(inputId = ns("modGlasgow"), label = "Modified Glasgow score", choices = 0:2, multiple = TRUE),
      selectInput(inputId = ns("whoEcog"), label = "WHO ECOG score", choices = BrValg$whoEcog_valg, multiple = TRUE),
      selectInput(inputId = ns("forbehandling"), label = "Onkologisk forbehandling", multiple = TRUE,
                  choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
      selectInput(inputId = ns("malign"), label = "Diagnose", choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
      #     )
      # ),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
      # a(id = ns("toggleAdvanced"), "Skjul/vis flere valg", href = "#")
    ),
    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur", value = "fig",
                           plotOutput(ns("Figur1"), height="auto"), downloadButton(ns("lastNedBilde"), "Last ned figur")),
                  tabPanel("Tabell", value = "tab",
                           uiOutput(ns("utvalg")),
                           # textOutput(ns("utvalg")),
                           br(),
                           tableOutput(ns("Tabell1")),
                           downloadButton(ns("lastNed"), "Last ned tabell"))
      )
    )
  )

}




fordelingsfig <- function(input, output, session, reshID, RegData, userRole, hvd_session){

  # shinyjs::onclick("toggleAdvanced",
  #                  shinyjs::toggle(id = "avansert", anim = TRUE))

  observeEvent(input$reset_input, {
    shinyjs::reset("id_fordeling_panel")
  })

  observe(
    if (userRole != 'SC') {
      shinyjs::hide(id = 'valgtShus')
  })

  output$ncsp <- renderUI({
    ns <- session$ns
    if (!is.null(input$op_gruppe)) {
      selectInput(inputId = ns("ncsp_verdi"), label = "NCSP koder (velg en eller flere)",
                  choices = if (!is.null(input$op_gruppe)) {setNames(substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in%
                                                                                 as.numeric(input$op_gruppe)])), 1, 5),
                                     sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe)])))
                  }, multiple = TRUE)
    }
  })


  output$Figur1 <- renderPlot({
    norgast::FigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                        maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                        valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
                        op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
                        ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
                        # ncsp = '',
                        BMI = if (!is.null(input$BMI)) {input$BMI} else {''},
                        tilgang = if (!is.null(input$tilgang)) {input$tilgang} else {''},
                        minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]),
                        ASA = if (!is.null(input$ASA)) {input$ASA} else {''},
                        modGlasgow = fiksNULL(input$modGlasgow),
                        whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
                        forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
                        malign = as.numeric(input$malign),
                        reshID = reshID, enhetsUtvalg = input$enhetsUtvalg, erMann = as.numeric(input$erMann),
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
                                      ASA = if (!is.null(input$ASA)) {input$ASA} else {''}, modGlasgow = fiksNULL(input$modGlasgow),
                                      whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
                                      forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
                                      malign = as.numeric(input$malign),
                                      reshID = reshID, enhetsUtvalg = input$enhetsUtvalg, erMann = as.numeric(input$erMann),
                                      elektiv = as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad))
  })

  output$utvalg <- renderUI({
    TabellData <- tabellReager()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
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
      names(Tabell1) <- c('Kategori', 'Antall i kategori', 'Antall totalt', 'Andel (%)', 'Antall i kategori', 'Antall totalt', 'Andel (%)')
      Tabell1 %>% knitr::kable("html", digits = c(0,0,0,1,0,0,1)) %>%
        kable_styling("hover", full_width = F) %>%
        add_header_above(c(" ", "Din avdeling" = 3, "Landet forøvrig" = 3))
    } else {
      Tabell1 <- TabellData$Antall %>%
        mutate(Kategori = rownames(.)) %>%
        select(Kategori, everything()) %>%
        mutate(AndelHoved = 100*AntHoved/NHoved)
      names(Tabell1) <- c('Kategori', 'Antall i kategori', 'Antall totalt', 'Andel (%)')
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
      # write.csv2(Tabell1, file, row.names = F, fileEncoding = 'latin1')
      write.csv3(Tabell1, file, row.names = F)
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
                          ASA = if (!is.null(input$ASA)) {input$ASA} else {''}, modGlasgow = fiksNULL(input$modGlasgow),
                          whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
                          forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
                          malign = as.numeric(input$malign),
                          reshID = reshID, enhetsUtvalg = input$enhetsUtvalg, erMann = as.numeric(input$erMann),
                          elektiv = as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad), outfile = file)
    }
  )

  shiny::observe({
    if (rapbase::isRapContext()) {
      if (req(input$tab) == "fig") {
        mld_fordeling <- paste0(
          "NoRGast: Figur - fordeling, variabel - ",
          input$valgtVar)
      }
      if (req(input$tab) == "tab") {
        mld_fordeling <- paste(
          "NoRGast: tabell - fordeling. variabel - ",
          input$valgtVar)
      }
      raplog::repLogger(
        session = hvd_session,
        msg = mld_fordeling
      )
      mldLastNedFig <- paste(
        "NoRGast: nedlasting figur - fordeling. variabel -",
        input$valgtVar
      )
      mldLastNedTab <- paste(
        "NoRGast: nedlasting tabell - fordeling. variabel -",
        input$valgtVar
      )
      shinyjs::onclick(
        "lastNedBilde",
        raplog::repLogger(
          session = hvd_session,
          msg = mldLastNedFig
        )
      )
      shinyjs::onclick(
        "lastNedTabell",
        raplog::repLogger(
          session = hvd_session,
          msg = mldLastNedTab
        )
      )
    }
  })






}
