#' UI-modul for sykehusvise andeler sammen med traktplot i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul traktplot
#'
#' @export
traktplot_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      width = 3,
      id = ns("id_trakt_panel"),
      div(id = ns("br_kontroller1"),
          checkboxInput(inputId = ns("referansepasient"),
                        label = "Velg referansepasient"),
          checkboxInput(inputId = ns("kun_ferdigstilte"),
                        label = "Inkludér kun komplette forløp (også oppfølging ferdigstilt)",
                        value = TRUE),
          uiOutput(ns("valgtVar_ui")),
          numericInput(ns("fjern_n"),
                       "Fjern avdelinger med antall forløp færre enn:",
                       value = 10, min = 0, max = 500, step = 1),
          checkboxInput(inputId = ns("aktiver_justering"),
                        label = "Juster midtverdi"),
          uiOutput(outputId = ns('settMaalnivaa'))),
      uiOutput(ns("slider_to_anim")),
      div(id = ns("br_kontroller2"),
          uiOutput(ns("speed_value")),
          sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                      max = 120, value = c(0, 120)),
          selectInput(inputId = ns("erMann"), label = "Kjønn",
                      choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
          uiOutput(ns("op_gruppe_ui")),
          uiOutput(outputId = ns('ncsp')),
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
          uiOutput(ns("BMI_ui")),
          uiOutput(outputId = ns('tilgang_utvidet_ui')),
          sliderInput(inputId = ns("PRS"), label = "mE-PASS",
                      min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
          uiOutput(outputId = ns('ASA_ui')),
          selectInput(inputId = ns("modGlasgow"),
                      label = "Modified Glasgow score",
                      choices = 0:2, multiple = TRUE),
          uiOutput(outputId = ns('whoEcog_ui')),
          selectInput(inputId = ns("forbehandling"),
                      label = "Onkologisk forbehandling",
                      multiple = TRUE,
                      choices = c('Cytostatika'=1, 'Stråleterapi'=2,
                                  'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
          selectInput(inputId = ns("malign"), label = "Diagnose",
                      choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
          selectInput(inputId = ns("accordion"), label = "Accordiongrad",
                      multiple = TRUE,
                      choices = c('<3'=1, '3'=3, '4'=4, '5'=5, '6'=6)),
          selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                      choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(
      # tabsetPanel(
      # id = ns("tabs_traktplot"),
      tabPanel("Traktplot", value = "Traktplot",

               # this is an extra div used ONLY to create positioned ancestor for tooltip
               # we don't change its position
               div(
                 style = "position:relative",
                 uiOutput(ns("utvalg")),
                 plotOutput(ns("barplot"),
                            click = ns("plot_click2")),
                 plotOutput(ns("fig_trakt"),
                            click = ns("plot_click")),
                 # hover = hoverOpts(ns("plot_hover"), delay = 100, delayType = "debounce")),
                 # uiOutput(ns("hover_info")),
                 uiOutput(ns("click_info_verbatim"))#,
                 # verbatimTextOutput(ns("datoinfo"))
                 # tableOutput(ns("click_info_verbatim2"))
               )
      )
      # )
      , width = 7
    )
  )
}

#' UI-modul for sykehusvise andeler sammen med traktplot i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul traktplot
#'
#' @export
traktplot <- function(input, output, session, reshID, RegData, hvd_session, BrValg){

  observeEvent(input$reset_input, {
    shinyjs::reset("br_kontroller1")
    shinyjs::reset("br_kontroller2")
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



  output$slider_to_anim <- renderUI({
    ns <- session$ns
    # shiny::validate(need(input$speed, F))
    sliderInput(inputId = ns("datovalg"), label = "Velg tidsintervall",
                min=as.Date("2014-01-01"), max = Sys.Date(),
                value=c(lubridate::`%m-%`(Sys.Date(), months(12)), Sys.Date()),
                step = 365,
                animate = animationOptions(interval = if (!is.null(input$speed)) {1000*input$speed} else {1400}, loop = F,
                                           playButton = "Spill av animasjon"),
                timeFormat="%F")
  })


  output$speed_value <- renderUI({
    ns <- session$ns
    numericInput(ns("speed"),"Tid i sekunder mellom hvert animasjonssteg :", value = 1.4, min = 0.1, max = 5, step = 0.1)
  })

  output$settMaalnivaa <- renderUI({
    ns <- session$ns
    if (input$aktiver_justering) {
      numericInput(inputId = ns("settMaalnivaa_verdi"), "Midtverdi", min = 0, max = 100,
                   value = round(vals$benchmark, 2), step = 0.01) #, ticks = F
    }
  })

  observe(
    if (req(input$referansepasient)) {
      shiny::updateSelectInput(inputId = "hastegrad_hybrid", label = "Hastegrad (hybrid)",
                               choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=0), selected = 1)
      shiny::updateSelectInput(inputId = "malign", label = "Diagnose",
                               choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0), selected = 1)
      shiny::updateSelectInput(inputId = "whoEcog", label = "WHO ECOG score",
                               choices = BrValg$whoEcog_valg, selected = c(0, 1))
    })


  output$utvalg <- renderUI({
    tagList(
      h3(HTML(paste0(traktdata()$tittel, '<br />'))),
      h5(HTML(paste0(traktdata()$utvalgTxt, '<br />')))
    )})

  farger<-rapFigurer::figtype()$farger

  ############### Andeler #######################################################

  traktdata <- function() {
    ## Preparer variabler for fremstilling i figur
    PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=req(input$valgtVar))
    RegData <- PlotParams$RegData
    tittel <- PlotParams$tittel
    PlotParams$RegData <- NA

    ## Gjør utvalg basert på brukervalg (LibUtvalg)
    if (!is.null(input$datovalg)) {
      NorgastUtvalg <- NorgastUtvalg(
        RegData=RegData, datoFra = input$datovalg[1],
        datoTil = input$datovalg[2],
        kun_ferdigstilte = input$kun_ferdigstilte,
        minald=as.numeric(input$alder[1]),
        maxald=as.numeric(input$alder[2]),
        erMann=as.numeric(input$erMann),
        elektiv=as.numeric(input$elektiv),
        hastegrad = as.numeric(input$hastegrad),
        hastegrad_hybrid = as.numeric(input$hastegrad_hybrid),
        BMI=fiksNULL(input$BMI), valgtShus='',
        tilgang_utvidet = if (!is.null(input$tilgang_utvidet)) {
          input$tilgang_utvidet} else {''},
        minPRS = as.numeric(input$PRS[1]),
        maxPRS = as.numeric(input$PRS[2]),
        ASA=fiksNULL(input$ASA),
        whoEcog=fiksNULL(input$whoEcog),
        forbehandling = fiksNULL(input$forbehandling),
        malign=as.numeric(input$malign),
        op_gruppe=fiksNULL(input$op_gruppe),
        ncsp = fiksNULL(input$ncsp_verdi),
        modGlasgow=fiksNULL(input$modGlasgow),
        accordion = if (!is.null(input$accordion)) {input$accordion} else {''})
      RegData <- NorgastUtvalg$RegData
      utvalgTxt <- NorgastUtvalg$utvalgTxt} else utvalgTxt <-""

    RegData <- RegData[!is.na(RegData$Variabel), ]
    my_data <- RegData %>% dplyr::group_by(Sykehusnavn) %>%
      dplyr::summarise(n = sum(Variabel),
                       d = dplyr::n())
    my_data <- my_data[my_data$d >= req(input$fjern_n), ]

    if (!is.null(my_data)) {

      if (input$aktiver_justering & !is.null(input$settMaalnivaa_verdi)) {
        my_limits   <- funnelR::fundata(input=my_data,
                                        benchmark= as.numeric(input$settMaalnivaa_verdi)/100,
                                        alpha=0.80,
                                        alpha2=0.95,
                                        method='approximate',
                                        step=1)
      } else {
        my_limits   <- funnelR::fundata(input=my_data,
                                        alpha=0.80,
                                        alpha2=0.95,
                                        method='approximate',
                                        step=1)
      }

      my_limits$lo[my_limits$lo < 0] <- 0
      my_limits$lo2[my_limits$lo2 < 0] <- 0
      my_data$andel <- my_data$n/my_data$d*100
      my_limits[, c("up2", "lo2")] <- 100*my_limits[, c("up2", "lo2")]
      list(my_data=my_data, my_limits=my_limits, utvalgTxt=utvalgTxt, tittel=tittel)
    }
  }

  vals <- reactiveValues(
    shus = data.frame(),
    benchmark = 0,
    klikkinfo = NULL
  )
  observeEvent(input$aktiver_justering, {
    vals$benchmark <- sum(traktdata()$my_data$n)/sum(traktdata()$my_data$d)*100
  })
  observeEvent(input$valgtVar, {
    vals$benchmark <- sum(traktdata()$my_data$n)/sum(traktdata()$my_data$d)*100
  })
  observeEvent(input$plot_click2, {
    vals$shus <- data.frame(traktdata()$my_data[order(traktdata()$my_data$andel), ], color="blue")
    vals$shus$color[round(as.numeric(input$plot_click2$y))] <- 'red'
      vals$klikkinfo <- c(round(as.numeric(input$plot_click2$y)), vals$klikkinfo)
  })
  observeEvent(input$plot_click, {
    vals$shus <- data.frame(traktdata()$my_data[order(traktdata()$my_data$andel), ], color="blue")
    point <- nearPoints(vals$shus, input$plot_click, xvar = "d", yvar = "andel", threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) != 0) {vals$shus$color <- ifelse(vals$shus$Sykehusnavn==point$Sykehusnavn,'red','blue')}
  })

  output$fig_trakt <- renderPlot({
    my_limits <- traktdata()$my_limits
    my_data <- traktdata()$my_data
    if (dim(vals$shus)[1]>0) {
      my_data <- merge(my_data, vals$shus[, c("Sykehusnavn", "color")], by="Sykehusnavn")
    } else {my_data$color <- "blue"}
    my_data$color[is.na(my_data$color)] <- "red"
      ggplot2::ggplot(data=my_data, ggplot2::aes(x=d,y=andel, color=color)) +
        ggplot2::geom_line(data=my_limits, ggplot2::aes(x=d, y=up2), colour=farger[1]) +
        ggplot2::geom_line(data=my_limits, ggplot2::aes(x=d, y=lo2), colour=farger[1]) +
        ggplot2::geom_hline(data=my_limits, ggplot2::aes(yintercept=benchmark*100), colour="red") +
        ggplot2::coord_cartesian(ylim=c(0,max(my_limits$up2, my_data$andel)), clip = "off") +
        ggplot2::annotate("text", x = round(max(my_data$d)[1]*1.1),
                          y = my_limits$benchmark[1]*110,
                          label = as.character(round(my_limits$benchmark[1]*100, 2))) +
        ggplot2::theme_classic()+ ggplot2::geom_point(data=my_data, size=1.5) +
        ggplot2::scale_color_manual(values = c("red" = "red", "blue" = "black"), na.value = "red") +
        ggplot2::labs(x = "Antall forløp",  y = "Andel (%)") + #scale_x_discrete(expand = c(0, 10)) +
        ggplot2::theme(legend.position = "none")
  })

  output$barplot <- renderPlot({
    if (!is.null(traktdata()$my_data)) {
      my_data <- traktdata()$my_data
      if (dim(vals$shus)[1]>0) {
        my_data <- merge(my_data, vals$shus[, c("Sykehusnavn", "color")], by="Sykehusnavn")
      } else {my_data$color <- "blue"}
      my_data <- my_data[order(my_data$andel), ]
      my_data$Sykehusnavn <- factor(my_data$Sykehusnavn, levels = my_data$Sykehusnavn)
      my_data$color[is.na(my_data$color)] <- "red"
        ggplot2::ggplot(data=my_data, ggplot2::aes(x=Sykehusnavn, y=andel, fill = color)) +
          ggplot2::geom_bar(stat = "identity", width = 0.5) +
          ggplot2::coord_flip() +
          ggplot2::theme_classic() +
          ggplot2::scale_x_discrete(expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = c(0,0)) +
          ggplot2::labs(y = "Andel (%)") +
          ggplot2::theme(legend.position = "none",
                         plot.title = ggplot2::element_text(hjust = 0.5),
                         axis.line.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank()) +
          ggplot2::scale_fill_manual(values = c("red" = "red", "blue" = farger[1]),
                                     na.value = "red")
    }
  })

  output$click_info_verbatim <- renderUI({
    if (dim(vals$shus)[1]>0) {
      my_data <- traktdata()$my_data
      my_data <- merge(my_data, vals$shus[, c("Sykehusnavn", "color")], by="Sykehusnavn")
      wellPanel(
        HTML(paste0("<b> Avdeling: </b>", my_data$Sykehusnavn[my_data$color=="red" | is.na(my_data$color)], "<br/>",
                    "<b> Andel: </b>", round(my_data$andel[my_data$color=="red" | is.na(my_data$color)], 1), "<b> % </b>","<br/>",
                    "<b> n: </b>", my_data$n[my_data$color=="red" | is.na(my_data$color)], "<br/>",
                    "<b> N: </b>", my_data$d[my_data$color=="red" | is.na(my_data$color)], "<br/>"))
      )
    }
  })

  shiny::observe({
    if (rapbase::isRapContext()) {
      rapbase::repLogger(
        session = hvd_session,
        msg = paste(
          "NORGAST: Traktplott, variabel -",
          input$valgtVar
        )
      )
    }
  })






}
