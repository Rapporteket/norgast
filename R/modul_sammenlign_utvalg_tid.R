#' UI-modul for sammenligning av andeler i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul sammenligning av andeler
#'
#' @export
saml_andeler_ui <- function(id){
  ns <- shiny::NS(id)

  fluidRow(
    column(2,
           style = "background-color:#ecf0f1",
           id = ns("id_overlevelse_panel"),
           h4(tags$b('Utvalg 1')),
           br(),
           # selectInput(inputId = ns("enhetsUtvalg"),
           #             label = "Velg enhet",
           #             choices = c('Hele landet'=0, 'Egen avdeling'=2)),
           uiOutput(outputId = ns('enhetsUtvalg_ui')),
           uiOutput(outputId = ns('valgtShus_ui')),
           uiOutput(outputId = ns('tilgang_utvidet_ui')),
           sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                       max = 120, value = c(0, 120)),
           selectInput(inputId = ns("erMann"), label = "Kjønn",
                       choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
           selectInput(inputId = ns("elektiv"),
                       label = "Tidspunkt for operasjonsstart",
                       choices = c('Ikke valgt'=99,
                                   'Innenfor normalarbeidstid'=1,
                                   'Utenfor normalarbeidstid'=0)),
           selectInput(inputId = ns("hastegrad"), label = "Hastegrad",
                       choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=2)),
           selectInput(inputId = ns("hastegrad_hybrid"),
                       label = "Hastegrad, hybrid (bruker hastegrad når den
                       finnes, ellers tidspkt for op.start)",
                       choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=0)),
           uiOutput(outputId = ns('op_gruppe_ui')),
           uiOutput(outputId = ns('ncsp')),
           uiOutput(outputId = ns('BMI_ui')),
           sliderInput(inputId=ns("PRS"), label = "mE-PASS", min = 0,
                       max = 2.2, value = c(0, 2.2), step = 0.05),
           uiOutput(outputId = ns('ASA_ui')),
           selectInput(inputId = ns("modGlasgow"),
                       label = "Modified Glasgow score",
                       choices = 0:2, multiple = TRUE),
           uiOutput(outputId = ns('whoEcog_ui')),
           selectInput(inputId = ns("forbehandling"),
                       label = "Onkologisk forbehandling", multiple = TRUE,
                       choices = c('Cytostatika'=1, 'Stråleterapi'=2,
                                   'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
           selectInput(inputId = ns("malign"), label = "Diagnose",
                       choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
           uiOutput(outputId = ns('icd')),
           selectInput(inputId = ns("accordion"), label = "Accordiongrad",
                       multiple = TRUE,
                       choices = c('<3'=1, '3'=3, '4'=4, '5'=5, '6'=6)),
           tags$hr(),
           actionButton(ns("reset_input"), "Nullstill valg")
    ),
    column(8,
           h2("Sammenlign andeler over tid", align='center'),
           h3("(Under arbeid)", align='center', style = "color:red"),
           h4("Her kan du sammenligne andeler over tid for to distinkte utvalg.
           Det er ikke helt rett frem å bruke verktøyet, og
              brukeren bør være oppmerksom på måten utvalgene gjøres: "),

           div(class = "container", style ="margin-right:(@gutter / 10)" ,
               tags$ul(
                 tags$li(h4("Hvis en pasient har flere forløp i ett enkelt
                 utvalg ('Utvalg 1' eller 'Utvalg 2') benyttes forløpet med den
                            tidligst forekommende operasjonen")),
                 tags$li(h4("Dersom en pasient har forløp i både 'Utvalg 1' og
                            'Utvalg 2' så velges det eldste forløpet.")),
                 tags$li(h4("Hvis det eldste forløpet finnes i både 'Utvalg 1' og
                 'Utvalg 2' eller hvis 'Utvalg 1' og 'Utvalg 2 'sitt
                            eldste forløp faller på samme dato, så knyttes pasienten
                            til 'Utvalg 1'. Dette innebærer at man potensielt
                            kan få litt forskjellige resultater hvis du f.eks. ser
                            på 'Åpen' i 'Utvalg 1' mot 'Laparoskopisk' i 'Utvalg 2'
                            kontra 'Laparoskopisk' i 'Utvalg 1' mot 'Åpen' i 'Utvalg 2'.")),
                 tags$li(h4("Hvis man vil unngå noen av problemene tilknyttet
                 pasienter som finnes i begge utvalg så kan det krysses av for
                            'Fjern pasienter med forløp som tilfredsstiller
                            begge utvalg'")),
                 tags$li(h4("Å ikke gjøre utvalg impliserer at alle pasienter velges.
                 Dette innebærer at hvis man kun gjør utvalg på ventresiden
                            ('Utvalg 1'), så vil 'Utvalg 2' bestå av alle pasienter
                            som ikke er i 'Utvalg 1'. Gjør du imidlertid kun utvalg på
                            høyresiden ('Utvalg 2'), så vil 'Utvalg 2' forbli
                            tom siden alle valgte forløp også finnes i 'Utvalg 1'."))
               )
           ),
           br(),
           fluidRow(
             column(
               3,
               checkboxInput(
                 inputId = ns("kun_ferdigstilte"),
                 label = "Inkludér kun komplette forløp (også oppfølging ferdigstilt)",
                 value = TRUE),
               uiOutput(outputId = ns('valgtVar_ui')),
               dateRangeInput(
                 inputId=ns("datovalg"),
                 label = "Operasjonsdato fra og til",
                 min = '2014-01-01',
                 max = Sys.Date(),
                 start  = lubridate::floor_date(lubridate::today() - lubridate::years(1),
                                                unit = "year"),
                 end = Sys.Date(), language = "nb", separator = " til "),
               selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                           choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))),
             column(3, offset = 1,
                    selectInput(inputId = ns("tidsenhet"), label = "Velg tidsenhet",
                                choices = c('Aar', 'Mnd', 'Kvartal', 'Halvaar'), selected = 'Kvartal'),
                    checkboxInput(ns("inkl_konf"), label = 'Inkluder konfidensintervall'),
                    checkboxInput(ns("fra0"), label = 'Vis y-aksen fra 0'),
                    checkboxInput(ns("inkl_tall"), label = 'Vis nevner i figur'),
                    br(),
                    actionButton(ns("goButton"), "Beregn!"))),
           br(),
           fluidRow(
             column(
               7,
               tabsetPanel(
                 tabPanel("Figur",
                          downloadButton(ns("lastNedBilde"), "Last ned figur"),
                          plotOutput(ns("Figur_andeler"))
                 ),
                 tabPanel("Tabell",
                          downloadButton(ns("lastNed_tabell"), "Last ned tabell"),
                          tableOutput(ns("Tabell_utvalg"))
                 ))),
             column(4, offset = 1,
                    uiOutput(ns("utvalg")),
                    br(),
                    checkboxInput(ns("ekskluder_felles"),
                                  label = 'Fjern pasienter med forløp som
                                  tilfredsstiller begge utvalg')
             )
           )
    ),
    column(
      2,
      style = "background-color:#ecf0f1",
      id = ns("id_overlevelse_panel2"),
      h4(tags$b('Utvalg 2')),
      br(),
      # selectInput(inputId = ns("enhetsUtvalg2"),
      #             label = "Velg enhet",
      #             choices = c('Hele landet'=0, 'Egen avdeling'=2)),
      uiOutput(outputId = ns('enhetsUtvalg2_ui')),
      uiOutput(outputId = ns('valgtShus2_ui')),
      uiOutput(outputId = ns('tilgang_utvidet2_ui')),
      sliderInput(inputId=ns("alder2"), label = "Alder", min = 0,
                  max = 120, value = c(0, 120)),
      selectInput(inputId = ns("erMann2"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("elektiv2"),
                  label = "Tidspunkt for operasjonsstart",
                  choices = c('Ikke valgt'=99,
                              'Innenfor normalarbeidstid'=1,
                              'Utenfor normalarbeidstid'=0)),
      selectInput(inputId = ns("hastegrad2"), label = "Hastegrad",
                  choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=2)),
      selectInput(inputId = ns("hastegrad_hybrid2"),
                  label = "Hastegrad, hybrid (bruker hastegrad når den finnes,
                  ellers tidspkt for op.start)",
                  choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=0)),
      uiOutput(outputId = ns('op_gruppe2_ui')),
      uiOutput(outputId = ns('ncsp2')),
      uiOutput(outputId = ns('BMI2_ui')),
      sliderInput(inputId=ns("PRS2"), label = "mE-PASS", min = 0, max = 2.2,
                  value = c(0, 2.2), step = 0.05),
      uiOutput(outputId = ns('ASA2_ui')),
      selectInput(inputId = ns("modGlasgow2"),
                  label = "Modified Glasgow score",
                  choices = 0:2, multiple = TRUE),
      uiOutput(outputId = ns('whoEcog2_ui')),
      selectInput(inputId = ns("forbehandling2"),
                  label = "Onkologisk forbehandling", multiple = TRUE,
                  choices = c('Cytostatika'=1, 'Stråleterapi'=2,
                              'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
      selectInput(inputId = ns("malign2"), label = "Diagnose",
                  choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
      uiOutput(outputId = ns('icd2')),
      selectInput(inputId = ns("accordion2"), label = "Accordiongrad",
                  multiple = TRUE,
                  choices = c('<3'=1, '3'=3, '4'=4, '5'=5, '6'=6)),
      tags$hr(),
      actionButton(ns("reset_input2"), "Nullstill valg"))
  )

}

#' Server-modul for sammenligning av andeler i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul sammenligning av andeler
#'
#' @export
saml_andeler_server <- function(id, reshID, RegData,
                                userRole, hvd_session, BrValg){
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$reset_input, {
        shinyjs::reset("id_overlevelse_panel")
      })
      observeEvent(input$reset_input2, {
        shinyjs::reset("id_overlevelse_panel2")
      })

      # observeEvent(userRole(), {
      #   if (userRole() == 'SC') {
      #     shinyjs::hide(id = 'enhetsUtvalg')
      #     shinyjs::hide(id = 'enhetsUtvalg2')
      #   } else {
      #     shinyjs::show(id = 'enhetsUtvalg')
      #     shinyjs::show(id = 'enhetsUtvalg2')
      #   }}
      # )

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

      output$ncsp2 <- renderUI({
        ns <- session$ns
        if (!is.null(input$op_gruppe2)) {
          selectInput(inputId = ns("ncsp_verdi2"), label = "NCSP koder (velg en eller flere)",
                      choices = if (!is.null(input$op_gruppe2)) {
                        setNames(substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe2)])), 1, 5),
                                 sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe2)])))
                      }, multiple = TRUE)
        }
      })


      output$enhetsUtvalg_ui <- renderUI({
        ns <- session$ns
        if (userRole() != 'SC') {
          selectInput(inputId = ns("enhetsUtvalg"),
                      label = "Velg enhet",
                      choices = c('Hele landet'=0, 'Egen avdeling'=2))
        }
      })
      output$enhetsUtvalg2_ui <- renderUI({
        ns <- session$ns
        if (userRole() != 'SC') {
          selectInput(inputId = ns("enhetsUtvalg2"),
                      label = "Velg enhet",
                      choices = c('Hele landet'=0, 'Egen avdeling'=2))
        }
      })
      output$valgtShus_ui <- renderUI({
        ns <- session$ns
        if (userRole() == 'SC') {
          selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                      choices = BrValg$sykehus, multiple = TRUE)
        }
      })
      output$valgtShus2_ui <- renderUI({
        ns <- session$ns
        if (userRole() == 'SC') {
          selectInput(inputId = ns("valgtShus2"), label = "Velg sykehus",
                      choices = BrValg$sykehus, multiple = TRUE)
        }
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

      output$op_gruppe2_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("op_gruppe2"), label = "Velg reseksjonsgruppe(r)",
                    choices = BrValg$reseksjonsgrupper, multiple = TRUE)
      })

      output$tilgang_utvidet_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("tilgang_utvidet"),
                    label = "Tilgang i abdomen (inkl. robotassistanse)",
                    choices = BrValg$tilgang_utvidet, multiple = TRUE)
      })

      output$tilgang_utvidet2_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("tilgang_utvidet2"),
                    label = "Tilgang i abdomen (inkl. robotassistanse)",
                    choices = BrValg$tilgang_utvidet, multiple = TRUE)
      })

      output$BMI_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("BMI"), label = "BMI",
                    choices = BrValg$bmi_valg, multiple = TRUE)
      })

      output$BMI2_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("BMI2"), label = "BMI",
                    choices = BrValg$bmi_valg, multiple = TRUE)
      })

      output$ASA_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("ASA"), label = "ASA-grad",
                    choices = BrValg$ASA_valg, multiple = TRUE)
      })

      output$ASA2_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("ASA2"), label = "ASA-grad",
                    choices = BrValg$ASA_valg, multiple = TRUE)
      })

      output$whoEcog_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("whoEcog"), label = "WHO ECOG score",
                    choices = BrValg$whoEcog_valg, multiple = TRUE)
      })

      output$whoEcog2_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("whoEcog2"), label = "WHO ECOG score",
                    choices = BrValg$whoEcog_valg, multiple = TRUE)
      })


      output$icd <- renderUI({
        ns <- session$ns
        Utvalg1 <- NorgastUtvalg(RegData = RegData,
                                 op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
                                 ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
                                 malign = as.numeric(input$malign))
        Utvalg1 <- Utvalg1$RegData
        diagnoser <- names(sort(table(Utvalg1$Hoveddiagnose2), decreasing = T))
        if (!is.null(diagnoser)) {
          selectInput(inputId = ns("icd_verdi"), label = "Spesifiser ICD-10 koder (velg en eller flere)",
                      choices = diagnoser, multiple = TRUE)
        }
      })

      output$icd2 <- renderUI({
        ns <- session$ns
        Utvalg1 <- NorgastUtvalg(RegData = RegData,
                                 op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
                                 ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''},
                                 malign = as.numeric(input$malign2))
        Utvalg1 <- Utvalg1$RegData
        diagnoser <- names(sort(table(Utvalg1$Hoveddiagnose2), decreasing = T))
        if (!is.null(diagnoser)) {
          selectInput(inputId = ns("icd_verdi2"), label = "Spesifiser ICD-10 koder (velg en eller flere)",
                      choices = diagnoser, multiple = TRUE)
        }
      })

      utvalgsfunksjon <- function() {

        input$goButton

        PlotParams <- NorgastPrepVar(RegData=RegData,
                                     valgtVar=if (!is.null(input$valgtVar)) {input$valgtVar} else {'Anastomoselekkasje'})
        RegData <- PlotParams$RegData

        Utvalg1 <- shiny::isolate(
          NorgastUtvalg(
            RegData = RegData, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
            minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
            erMann = as.numeric(input$erMann), kun_ferdigstilte = input$kun_ferdigstilte,
            elektiv = as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad),
            hastegrad_hybrid = as.numeric(input$hastegrad_hybrid),
            BMI = if (!is.null(input$BMI)) {input$BMI} else {''},
            valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
            op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
            ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
            # tilgang = if (!is.null(input$tilgang)) {input$tilgang} else {''},
            tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {input$tilgang_utvidet} else {''},
            minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]),
            ASA = if (!is.null(input$ASA)) {input$ASA} else {''},
            modGlasgow = if (!is.null(input$modGlasgow)) {input$modGlasgow} else {''},
            whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
            forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
            malign = as.numeric(input$malign),
            icd = if (!is.null(input$icd_verdi)) {input$icd_verdi} else {''},
            accordion = if (!is.null(input$accordion)) {input$accordion} else {''}))
        Utvalg1data <- Utvalg1$RegData
        if (dim(Utvalg1data)[1] > 0) {
          shiny::isolate(if (!is.null(input$valgtShus)) {
            Utvalg1data <- Utvalg1data[which(Utvalg1data$AvdRESH %in% as.numeric(input$valgtShus)), ]
          })
          shiny::isolate(if (!is.null(input$enhetsUtvalg)) {
            if (input$enhetsUtvalg == 2) {Utvalg1data <- Utvalg1data[which(Utvalg1data$AvdRESH == reshID()), ]}
          })
          Utvalg1data$Utvalg <- 1
          Utvalg1data <- Utvalg1data[order(Utvalg1data$HovedDato, decreasing = F), ]                  # Hvis pasient opptrer flere ganger, velg
          Utvalg1data <- Utvalg1data[match(unique(Utvalg1data$PasientID), Utvalg1data$PasientID), ]   # første operasjon i utvalget
        }
        Utvalg2 <- shiny::isolate(
          NorgastUtvalg(
            RegData = RegData, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
            minald=as.numeric(input$alder2[1]), maxald=as.numeric(input$alder2[2]),
            erMann = as.numeric(input$erMann2), kun_ferdigstilte = input$kun_ferdigstilte,
            elektiv = as.numeric(input$elektiv2), hastegrad = as.numeric(input$hastegrad2),
            hastegrad_hybrid = as.numeric(input$hastegrad_hybrid2),
            BMI = if (!is.null(input$BMI2)) {input$BMI2} else {''},
            valgtShus = if (!is.null(input$valgtShus2)) {input$valgtShus2} else {''},
            op_gruppe = if (!is.null(input$op_gruppe2)) {input$op_gruppe2} else {''},
            ncsp = if (!is.null(input$ncsp_verdi2)) {input$ncsp_verdi2} else {''},
            # tilgang = if (!is.null(input$tilgang2)) {input$tilgang2} else {''},
            tilgang_utvidet = if (!is.null(input$tilgang_utvidet2)) {input$tilgang_utvidet2} else {''},
            minPRS = as.numeric(input$PRS2[1]), maxPRS = as.numeric(input$PRS2[2]),
            ASA = if (!is.null(input$ASA2)) {input$ASA2} else {''},
            modGlasgow = if (!is.null(input$modGlasgow2)) {input$modGlasgow2} else {''},
            whoEcog = if (!is.null(input$whoEcog2)) {input$whoEcog2} else {''},
            forbehandling = if (!is.null(input$forbehandling2)) {input$forbehandling2} else {''},
            malign = as.numeric(input$malign2),
            icd = if (!is.null(input$icd_verdi2)) {input$icd_verdi2} else {''},
            accordion = if (!is.null(input$accordion2)) {input$accordion2} else {''}))
        Utvalg2data <- Utvalg2$RegData
        if (dim(Utvalg2data)[1] > 0) {
          shiny::isolate(if (!is.null(input$valgtShus2)) {
            Utvalg2data <- Utvalg2data[which(Utvalg2data$AvdRESH %in% as.numeric(input$valgtShus2)), ]
          })
          shiny::isolate(if (!is.null(input$enhetsUtvalg2)) {
            if (input$enhetsUtvalg2 == 2) {Utvalg2data <- Utvalg2data[which(Utvalg2data$AvdRESH == reshID()), ]}
          })
          Utvalg2data$Utvalg <- 2
          Utvalg2data <- Utvalg2data[order(Utvalg2data$HovedDato, decreasing = F), ]                   # Hvis pasient opptrer flere ganger, velg
          Utvalg2data <- Utvalg2data[match(unique(Utvalg2data$PasientID), Utvalg2data$PasientID), ]   # første operasjon i utvalget
        }
        utdata <- list(Utvalg1 = Utvalg1data, Utvalg2 = Utvalg2data, utvalgTxt1 = Utvalg1$utvalgTxt, utvalgTxt2 = Utvalg2$utvalgTxt)

        # Samlet <- dplyr::bind_rows(Utvalg1data, Utvalg2data)
        Samlet <- dplyr::bind_rows(Utvalg1data, Utvalg2data[!(Utvalg2data$ForlopsID %in% Utvalg1data$ForlopsID), ]) # Fjerner forløp fra utvalg 2
        # som finnes i utvalg 1
        Samlet <- Samlet[order(Samlet$HovedDato, Samlet$Utvalg, decreasing = F), ]    # Hvis pasient opptrer flere ganger, velg
        Samlet <- Samlet[match(unique(Samlet$PasientID), Samlet$PasientID), ]         # første operasjon i utvalget

        if (input$ekskluder_felles) {
          Samlet <- Samlet[!(Samlet$PasientID %in% intersect(Utvalg1data$PasientID, Utvalg2data$PasientID)), ]
        }

        PlotParams <- NorgastPrepVar(RegData=Samlet,
                                     valgtVar=if (!is.null(input$valgtVar)) {input$valgtVar} else {'Anastomoselekkasje'})
        Samlet <- PlotParams$RegData
        PlotParams$RegData <- NA
        utdata$PlotParams <- PlotParams

        utdata$Samlet <- Samlet
        # print(dim(utdata$Samlet[utdata$Samlet$Utvalg==1, ]))

        return(utdata)
      }


      output$Figur_andeler <- renderPlot({
        input$goButton
        isolate(norgast::NorgastFigAndelTid_SammenligUtvalg(plotdata = utvalgsfunksjon(), tidsenhet = input$tidsenhet,
                                                            inkl_konf = input$inkl_konf,
                                                            datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                                            fra0 = input$fra0, inkl_tall=input$inkl_tall))
      }, width = 800, height = 800) #


      output$utvalg <- renderUI({
        input$goButton
        utvlgdata <- isolate(utvalgsfunksjon())
        tagList(
          h4('Utvalg 1:'),
          h5(HTML(paste0(utvlgdata$utvalgTxt1, '<br />'))),
          shiny::isolate(h5(if (!is.null(input$valgtShus)) {
            HTML(paste0("Avdeling(er): ", paste(unique(utvlgdata$Utvalg1$Sykehusnavn), collapse=', ')))
          })),
          shiny::isolate(h5(if (!is.null(input$enhetsUtvalg)) {
            if (input$enhetsUtvalg == 2) {HTML(paste0("Avdeling: ", paste(unique(utvlgdata$Utvalg1$Sykehusnavn), collapse=', ')))}
          })),
          br(),
          br(),
          h4('Utvalg 2:'),
          h5(HTML(paste0(utvlgdata$utvalgTxt2, '<br />'))),
          shiny::isolate(h5(if (!is.null(input$valgtShus2)) {
            HTML(paste0("Avdeling(er): ", paste(unique(utvlgdata$Utvalg2$Sykehusnavn), collapse=', ')))
          })),
          shiny::isolate(h5(if (!is.null(input$enhetsUtvalg2)) {
            if (input$enhetsUtvalg2 == 2) {HTML(paste0("Avdeling: ", paste(unique(utvlgdata$Utvalg2$Sykehusnavn), collapse=', ')))}
          })),
          br(),
          br(),
          h4('Merknad:'),
          if (input$ekskluder_felles) {
            h5(paste0(length(setdiff(utvlgdata$Utvalg1$PasientID, utvlgdata$Samlet$PasientID[utvlgdata$Samlet$Utvalg==1])),
                      ' pasienter er ekskludert siden de har forløp som tilfredstiller begge utvalg.'))
          } else {
            h5(paste0(length(setdiff(utvlgdata$Utvalg1$PasientID, utvlgdata$Samlet$PasientID[utvlgdata$Samlet$Utvalg==1])),
                      ' av ', dim(utvlgdata$Utvalg1)[1], ' pasienter er ekskludert fra utvalg 1 siden et forløp med eldre
                operasjonsdato på samme pasient finnes i utvalg 2. ',
                      length(setdiff(utvlgdata$Utvalg2$PasientID, utvlgdata$Samlet$PasientID[utvlgdata$Samlet$Utvalg==2])),
                      ' av ', dim(utvlgdata$Utvalg2)[1], ' pasienter er ekskludert fra utvalg 2 siden samme forløp eller et forløp med eldre eller samme
                operasjonsdato på samme pasient finnes i utvalg 1.'))
          }
        )})

      output$Tabell_utvalg <- function() {
        input$goButton
        utdata <- isolate(norgast::NorgastFigAndelTid_SammenligUtvalg(plotdata = utvalgsfunksjon(), tidsenhet = input$tidsenhet,
                                                                      inkl_konf = input$inkl_konf,
                                                                      datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                                                      fra0 = input$fra0, inkl_tall=input$inkl_tall))
        if (is.na(utdata$Konf$Utvalg2[1])){
          Tabell_tid <- dplyr::tibble(Tidsperiode = utdata$andeler$TidsEnhet,
                                      Antall = utdata$antall[["1"]],
                                      N = utdata$N[["1"]], 'Andel (%)'= utdata$andeler[["1"]], KI_nedre = utdata$Konf$Utvalg1[1,],
                                      KI_ovre = utdata$Konf$Utvalg1[2,])
          Tabell_tid %>%
            knitr::kable("html", digits = c(0,0,0,1,1,1)) %>%
            kableExtra::kable_styling("hover", full_width = F)
        } else {
          Tabell_tid <- dplyr::tibble(Tidsperiode = utdata$andeler$TidsEnhet, Antall = utdata$antall[["1"]],
                                      N = utdata$N[["1"]], Andel = utdata$andeler[["1"]], Konf.int.nedre = utdata$Konf$Utvalg1[1,],
                                      Konf.int.ovre = utdata$Konf$Utvalg1[2,], Antall2 = utdata$antall[["2"]],
                                      N2 = utdata$N[["2"]], Andel2 = utdata$andeler[["2"]], Konf.int.nedre2 = utdata$Konf$Utvalg2[1,],
                                      Konf.int.ovre2 = utdata$Konf$Utvalg2[2,])
          names(Tabell_tid) <- c('Tidsperiode', 'Antall', 'N', 'Andel (%)', 'KI_nedre', 'KI_ovre', 'Antall', 'N', 'Andel (%)',
                                 'KI_nedre', 'KI_ovre')
          Tabell_tid %>% knitr::kable("html", digits = c(0,0,0,1,1,1,0,0,1,1,1)) %>%
            kableExtra::kable_styling("hover", full_width = F) %>%
            kableExtra::add_header_above(c(" ", "Utvalg 1" = 5, "Utvalg 2" = 5))
        }
      }

      output$lastNed_tabell <- downloadHandler(
        filename = function(){
          paste0('sammenlign_andel_', input$valgtVar, Sys.time(), '.csv')
        },
        content = function(file){
          utdata <- isolate(norgast::NorgastFigAndelTid_SammenligUtvalg(plotdata = utvalgsfunksjon(), tidsenhet = input$tidsenhet,
                                                                        inkl_konf = input$inkl_konf,
                                                                        datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                                                        fra0 = input$fra0, inkl_tall=input$inkl_tall))
          if (is.na(utdata$Konf$Utvalg2)[1]){
            Tabell_tid <- dplyr::tibble(Tidsperiode = utdata$andeler$TidsEnhet,
                                        Antall = utdata$antall[["1"]],
                                        N = utdata$N[["1"]], 'Andel (%)'= utdata$andeler[["1"]], KI_nedre = utdata$Konf$Utvalg1[1,],
                                        KI_ovre = utdata$Konf$Utvalg1[2,])
          } else {
            Tabell_tid <- dplyr::tibble(Tidsperiode = utdata$andeler$TidsEnhet, Antall = utdata$antall[["1"]],
                                        N = utdata$N[["1"]], Andel = utdata$andeler[["1"]], Konf.int.nedre = utdata$Konf$Utvalg1[1,],
                                        Konf.int.ovre = utdata$Konf$Utvalg1[2,], Antall2 = utdata$antall[["2"]],
                                        N2 = utdata$N[["2"]], Andel2 = utdata$andeler[["2"]], Konf.int.nedre2 = utdata$Konf$Utvalg2[1,],
                                        Konf.int.ovre2 = utdata$Konf$Utvalg2[2,])
            names(Tabell_tid) <- c('Tidsperiode', 'Antall', 'N', 'Andel (%)', 'KI_nedre', 'KI_ovre', 'Antall', 'N', 'Andel (%)',
                                   'KI_nedre', 'KI_ovre')
          }
          write.csv3(Tabell_tid, file, row.names = F)
        }
      )

      output$lastNedBilde <- downloadHandler(
        filename = function(){
          paste0('sammenlign_andel_', input$valgtVar, Sys.time(), '.', input$bildeformat)
        },

        content = function(file){
          norgast::NorgastFigAndelTid_SammenligUtvalg(plotdata = utvalgsfunksjon(), tidsenhet = input$tidsenhet,
                                                      inkl_konf = input$inkl_konf,
                                                      datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                                      fra0 = input$fra0, inkl_tall=input$inkl_tall, outfile = file)
        }
      )

      shiny::observe({
        if (rapbase::isRapContext()) {
          shinyjs::onclick(
            "goButton",
            rapbase::repLogger(
              session = hvd_session,
              msg = paste0("NORGAST: Sammenlignede andeler, variabel - ", input$valgtVar)
            )
          )

          shinyjs::onclick(
            "lastNedBilde",
            rapbase::repLogger(
              session = hvd_session,
              msg = paste0("NORGAST: Laster ned figur sammenlignede andeler, variabel - ", input$valgtVar)
            )
          )

          shinyjs::onclick(
            "lastNed_tabell",
            rapbase::repLogger(
              session = hvd_session,
              msg = paste0("NORGAST: Laster ned tabell sammenlignede andeler, variabel - ", input$valgtVar)
            )
          )
        }
      })
    }
  )
}
