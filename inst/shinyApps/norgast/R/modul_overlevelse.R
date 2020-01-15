# Modul for fordelingsfigurer i NoRGast sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @return Modul fordelingsfigur
#
overlevelse_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  fluidRow(
    # div(class = "container", style = "background-color:#F0F0F0",
        column(2,
               style = "background-color:#ecf0f1",
               id = ns("id_overlevelse_panel"),
               h4(tags$b('Utvalg 1')),
               br(),
               dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01',
                              max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), language = "nb", separator = " til "),
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
               selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
                           choices = BrValg$reseksjonsgrupper, multiple = TRUE),
               uiOutput(outputId = ns('ncsp')),
               selectInput(inputId = ns("BMI"), label = "BMI", choices = BrValg$bmi_valg, multiple = TRUE),
               sliderInput(inputId=ns("PRS"), label = "mE-PASS", min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
               selectInput(inputId = ns("ASA"), label = "ASA-grad", choices = BrValg$ASA_valg, multiple = TRUE),
               selectInput(inputId = ns("whoEcog"), label = "WHO ECOG score", choices = BrValg$whoEcog_valg, multiple = TRUE),
               selectInput(inputId = ns("forbehandling"), label = "Onkologisk forbehandling", multiple = TRUE,
                           choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
               selectInput(inputId = ns("malign"), label = "Diagnose", choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
               # selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
               #             choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
               tags$hr(),
               actionButton(ns("reset_input"), "Nullstill valg")
        # )
    ),
    column(8,
           h2("Overlevelseskurver", align='center'),
           h4("Her kan du plotte overlevelseskurver for to distinkte utvalg. Dersom du kun gjør utvalg på ventresiden så
              vil Utvalg 2 bestå av alle pasienter som ikke faller i Utvalg 1. Dersom det er overlapp mellom Utvalg 1 og Utvalg 2 så fjernes
              pasientene som finnes i begge utvalg fra Utvalg 2. Hvis en pasient opptrer flere ganger i et utvalg beregnes overlevelse med
              utgangspunkt i første operasjon."),
           br(),
           br(),
           plotOutput(ns("Figur_surv"))
    ),
    column(2,
           style = "background-color:#ecf0f1",
           id = ns("id_overlevelse_panel2"),
           h4(tags$b('Utvalg 2')),
           br(),
           dateRangeInput(inputId=ns("datovalg2"), label = "Dato fra og til", min = '2014-01-01',
                          max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), language = "nb", separator = " til "),
           selectInput(inputId = ns("valgtShus2"), label = "Velg sykehus",
                       choices = BrValg$sykehus, multiple = TRUE),
           selectInput(inputId = ns("tilgang2"), label = "Tilgang i abdomen (velg en eller flere)", choices = BrValg$tilgang_valg, multiple = TRUE),
           sliderInput(inputId=ns("alder2"), label = "Alder", min = 0,
                       max = 120, value = c(0, 120)),
           selectInput(inputId = ns("erMann2"), label = "Kjønn",
                       choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
           selectInput(inputId = ns("elektiv2"), label = "Tidspunkt for operasjonsstart",
                       choices = c('Ikke valgt'=99, 'Innenfor normalarbeidstid'=1, 'Utenfor normalarbeidstid'=0)),
           selectInput(inputId = ns("hastegrad2"), label = "Hastegrad",
                       choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=2)),
           selectInput(inputId = ns("op_gruppe2"), label = "Velg reseksjonsgruppe(r)",
                       choices = BrValg$reseksjonsgrupper, multiple = TRUE),
           uiOutput(outputId = ns('ncsp2')),
           selectInput(inputId = ns("BMI2"), label = "BMI", choices = BrValg$bmi_valg, multiple = TRUE),
           sliderInput(inputId=ns("PRS2"), label = "mE-PASS", min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
           selectInput(inputId = ns("ASA2"), label = "ASA-grad", choices = BrValg$ASA_valg, multiple = TRUE),
           selectInput(inputId = ns("whoEcog2"), label = "WHO ECOG score", choices = BrValg$whoEcog_valg, multiple = TRUE),
           selectInput(inputId = ns("forbehandling2"), label = "Onkologisk forbehandling", multiple = TRUE,
                       choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
           selectInput(inputId = ns("malign2"), label = "Diagnose", choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
           # selectInput(inputId = ns("bildeformat2"), label = "Velg bildeformat",
           #             choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
           tags$hr(),
           actionButton(ns("reset_input2"), "Nullstill valg"))
  )

}




overlevelse <- function(input, output, session, reshID, RegData, userRole, hvd_session){

  # shinyjs::onclick("toggleAdvanced",
  #                  shinyjs::toggle(id = "avansert", anim = TRUE))

  observeEvent(input$reset_input, {
    shinyjs::reset("id_overlevelse_panel")
  })
  observeEvent(input$reset_input2, {
    shinyjs::reset("id_overlevelse_panel2")
  })

  observe(
    if (userRole != 'SC') {
      shinyjs::hide(id = 'valgtShus')
      shinyjs::hide(id = 'valgtShus2')
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

  output$ncsp2 <- renderUI({
    ns <- session$ns
    if (!is.null(input$op_gruppe2)) {
      selectInput(inputId = ns("ncsp_verdi2"), label = "NCSP koder (velg en eller flere)",
                  choices = if (!is.null(input$op_gruppe2)) {setNames(substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in%
                                                                                                                 as.numeric(input$op_gruppe2)])), 1, 5),
                                                                     sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe2)])))
                  }, multiple = TRUE)
    }
  })

  output$Figur_surv <- renderPlot({
    RegData <- RegData[-which(RegData$OpDoedTid<0), ] #feilregistreringer: død før operasjon
    RegData <- RegData[!(RegData$Avdod == 1 & is.na(RegData$OpDoedTid)), ] #manglende data: død men ingen dødsdato
    Utvalg1 <- NorgastUtvalg(RegData = RegData, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                             minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann = as.numeric(input$erMann),
                             elektiv = as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad),
                             BMI = if (!is.null(input$BMI)) {input$BMI} else {''},
                             valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
                             op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
                             ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
                             tilgang = if (!is.null(input$tilgang)) {input$tilgang} else {''},
                             minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]),
                             ASA = if (!is.null(input$ASA)) {input$ASA} else {''},
                             whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
                             forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
                             malign = as.numeric(input$malign))
    Utvalg1data <- Utvalg1$RegData
    if (!is.null(input$valgtShus)) {
      Utvalg1data <- Utvalg1data[which(Utvalg1data$AvdRESH %in% as.numeric(input$valgtShus)), ]
    }
    Utvalg1data$Utvalg <- 1
    Utvalg1data <- Utvalg1data[order(Utvalg1data$HovedDato, decreasing = F), ]                  # Hvis pasient opptrer flere ganger, velg
    Utvalg1data <- Utvalg1data[match(unique(Utvalg1data$PasientID), Utvalg1data$PasientID), ]   # første operasjon i utvalget

    Utvalg2 <- NorgastUtvalg(RegData = RegData, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
                             minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]), erMann = as.numeric(input$erMann2),
                             elektiv = as.numeric(input$elektiv2), hastegrad = as.numeric(input$hastegrad2),
                             BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
                             valgtShus = if (!is.null(input$valgtShus2)) {input$valgtShus2} else {''},
                             op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                             ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''},
                             tilgang = if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
                             minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
                             ASA = if (!is.null(input$ASA2)) {input$ASA2} else {''},
                             whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
                             forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
                             malign = as.numeric(input$malign2))
    Utvalg2data <- Utvalg2$RegData
    if (!is.null(input$valgtShus2)) {
      Utvalg2data <- Utvalg2data[which(Utvalg2data$AvdRESH %in% as.numeric(input$valgtShus2)), ]
    }
    Utvalg2data$Utvalg <- 2
    Utvalg2data <- Utvalg2data[order(Utvalg2data$HovedDato, decreasing = F), ]                   # Hvis pasient opptrer flere ganger, velg
    Utvalg2data <- Utvalg2data[match(unique(Utvalg2data$PasientID), Utvalg2data$PasientID), ]   # første operasjon i utvalget

    fellespasienter <- intersect(Utvalg1data$PasientID, Utvalg2data$PasientID)
    Utvalg2data <- Utvalg2data[!(Utvalg2data$PasientID %in% fellespasienter), ]   # Fjerner pasienter som er i utvalg 1 fra utvalg 2

    Samlet <- bind_rows(Utvalg1data, Utvalg2data)

    Samlet$overlev <- difftime(as.Date(Sys.Date()), Samlet$OperasjonsDato, units = 'days')
    Samlet$overlev[Samlet$Avdod==1] <- Samlet$OpDoedTid[Samlet$Avdod==1]
    Samlet$overlev <- as.numeric(Samlet$overlev)
    Samlet$SurvObj <- with(Samlet, Surv(overlev, Avdod == 1))
    fit1 <- survfit(SurvObj ~ Utvalg, data = Samlet)
    ggsurvplot(fit1, data = Samlet, pval = TRUE, conf.int = T, fun = "pct",
               risk.table = TRUE, legend = "bottom")
  }, width = 800, height = 800)


  # tabellReager <- reactive({
  #   TabellData <- norgast::FigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
  #                                     maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
  #                                     valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
  #                                     op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
  #                                     ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
  #                                     BMI = if (!is.null(input$BMI)) {input$BMI} else {''},
  #                                     tilgang = if (!is.null(input$tilgang)) {input$tilgang} else {''},
  #                                     minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]),
  #                                     ASA = if (!is.null(input$ASA)) {input$ASA} else {''},
  #                                     whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
  #                                     forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
  #                                     malign = as.numeric(input$malign),
  #                                     reshID = reshID, enhetsUtvalg = input$enhetsUtvalg, erMann = as.numeric(input$erMann),
  #                                     elektiv = as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad))
  # })
  #
  # output$utvalg <- renderUI({
  #   TabellData <- tabellReager()
  #   tagList(
  #     h3(HTML(paste0(TabellData$tittel, '<br />'))),
  #     h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
  #   )})
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
  #     names(Tabell1) <- c('Kategori', 'Antall i kategori', 'Antall totalt', 'Andel (%)', 'Antall i kategori', 'Antall totalt', 'Andel (%)')
  #     Tabell1 %>% knitr::kable("html", digits = c(0,0,0,1,0,0,1)) %>%
  #       kable_styling("hover", full_width = F) %>%
  #       add_header_above(c(" ", "Din avdeling" = 3, "Landet forøvrig" = 3))
  #   } else {
  #     Tabell1 <- TabellData$Antall %>%
  #       mutate(Kategori = rownames(.)) %>%
  #       select(Kategori, everything()) %>%
  #       mutate(AndelHoved = 100*AntHoved/NHoved)
  #     names(Tabell1) <- c('Kategori', 'Antall i kategori', 'Antall totalt', 'Andel (%)')
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
  #
  # output$lastNedBilde <- downloadHandler(
  #   filename = function(){
  #     paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
  #   },
  #
  #   content = function(file){
  #     norgast::FigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
  #                         maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
  #                         valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
  #                         op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
  #                         ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
  #                         BMI = if (!is.null(input$BMI)) {input$BMI} else {''},
  #                         tilgang = if (!is.null(input$tilgang)) {input$tilgang} else {''},
  #                         minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]),
  #                         ASA = if (!is.null(input$ASA)) {input$ASA} else {''},
  #                         whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
  #                         forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
  #                         malign = as.numeric(input$malign),
  #                         reshID = reshID, enhetsUtvalg = input$enhetsUtvalg, erMann = as.numeric(input$erMann),
  #                         elektiv = as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad), outfile = file)
  #   }
  # )
  #
  # shiny::observe({
  #   if (rapbase::isRapContext()) {
  #     if (req(input$tab) == "fig") {
  #       mld_fordeling <- paste0(
  #         "NoRGast: Figur - fordeling, variabel - ",
  #         input$valgtVar)
  #     }
  #     if (req(input$tab) == "tab") {
  #       mld_fordeling <- paste(
  #         "NoRGast: tabell - fordeling. variabel - ",
  #         input$valgtVar)
  #     }
  #     raplog::repLogger(
  #       session = hvd_session,
  #       msg = mld_fordeling
  #     )
  #     mldLastNedFig <- paste(
  #       "NoRGast: nedlasting figur - fordeling. variabel -",
  #       input$valgtVar
  #     )
  #     mldLastNedTab <- paste(
  #       "NoRGast: nedlasting tabell - fordeling. variabel -",
  #       input$valgtVar
  #     )
  #     shinyjs::onclick(
  #       "lastNedBilde",
  #       raplog::repLogger(
  #         session = hvd_session,
  #         msg = mldLastNedFig
  #       )
  #     )
  #     shinyjs::onclick(
  #       "lastNedTabell",
  #       raplog::repLogger(
  #         session = hvd_session,
  #         msg = mldLastNedTab
  #       )
  #     )
  #   }
  # })
  #





}
