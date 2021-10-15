# Modul for sykehusvise andeler i NoRGast sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @return Modul sykehusvisning, andeler
#
tunnelplot_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  pageWithSidebar(
    headerPanel("Brukervalg:"),
    sidebarPanel(width = 3,
                 id = ns("id_tunnel_panel"),
                 selectInput(inputId = ns("valgtVar"), label = "Velg variabel",
                             choices = BrValg$varvalg_andel),
                 dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01', language = "nb",
                                max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), separator = " til "),
                 sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                             max = 120, value = c(0, 120)),
                 selectInput(inputId = ns("erMann"), label = "Kjønn",
                             choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
                 selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
                             choices = BrValg$reseksjonsgrupper, multiple = TRUE),
                 uiOutput(outputId = ns('ncsp')),
                 # selectInput(inputId = ns("inkl_konf"), label = "Inkluder konfidensintervall",
                 #             choices = c(' '=99, 'Ja'=1, 'Nei'=0)),
                 selectInput(inputId = ns("elektiv"), label = "Tidspunkt for operasjonsstart",
                             choices = c('Ikke valgt'=99, 'Innenfor normalarbeidstid'=1, 'Utenfor normalarbeidstid'=0)),
                 selectInput(inputId = ns("hastegrad"), label = "Hastegrad",
                             choices = c('Ikke valgt'=99, 'Elektiv'=1, 'Akutt'=2)),
                 selectInput(inputId = ns("BMI"), label = "BMI", choices = BrValg$bmi_valg, multiple = TRUE),
                 selectInput(inputId = ns("tilgang"), label = "Tilgang i abdomen (velg en eller flere)", choices = BrValg$tilgang_valg, multiple = TRUE),
                 sliderInput(inputId = ns("PRS"), label = "mE-PASS", min = 0, max = 2.2, value = c(0, 2.2), step = 0.05),
                 selectInput(inputId = ns("ASA"), label = "ASA-grad", choices = BrValg$ASA_valg, multiple = TRUE),
                 selectInput(inputId = ns("modGlasgow"), label = "Modified Glasgow score", choices = 0:2, multiple = TRUE),
                 selectInput(inputId = ns("whoEcog"), label = "WHO ECOG score", choices = BrValg$whoEcog_valg, multiple = TRUE),
                 selectInput(inputId = ns("forbehandling"), label = "Onkologisk forbehandling", multiple = TRUE,
                             choices = c('Cytostatika'=1, 'Stråleterapi'=2, 'Komb. kjemo/radioterapi'=3, 'Ingen'=4)),
                 selectInput(inputId = ns("malign"), label = "Diagnose", choices = c('Ikke valgt'=99, 'Malign'=1, 'Benign'=0)),
                 selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                             choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                 tags$hr(),
                 actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(
      tabsetPanel(
        # id = ns("tabs_tunnelplot"),
        tabPanel("Tunnelplot", value = "Tunnelplot",

                 # this is an extra div used ONLY to create positioned ancestor for tooltip
                 # we don't change its position
                 div(
                   style = "position:relative",
                   plotOutput(ns("fig_tunnel"),
                              click = ns("plot_hover")),
                              # hover = hoverOpts(ns("plot_hover"), delay = 100, delayType = "debounce")),
                   # uiOutput(ns("hover_info")),
                   plotOutput(ns("barplot"),
                              click = ns("plot_hover2")),
                              # hover = hoverOpts(ns("plot_hover2"), delay = 100, delayType = "debounce")),
                   # plotlyOutput("testfig")
                   uiOutput(ns("hover_info_verbatim"))
                 )
        )
      )
      , width = 7
    )
  )
}



tunnelplot <- function(input, output, session, reshID, RegData, hvd_session){

  observeEvent(input$reset_input, {
    shinyjs::reset("id_tunnel_panel")
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

  ############### Andeler #######################################################

  traktdata <- function() {
    ## Preparer variabler for fremstilling i figur
    PlotParams <- NorgastPrepVar(RegData=RegData, valgtVar=input$valgtVar)
    RegData <- PlotParams$RegData
    PlotParams$RegData <- NA

    ## Gjør utvalg basert på brukervalg (LibUtvalg)
    NorgastUtvalg <- NorgastUtvalg(RegData=RegData, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                   minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
                                   elektiv=as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad),
                                   BMI=fiksNULL(input$BMI), valgtShus='', tilgang=fiksNULL(input$tilgang),
                                   minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]),
                                   ASA=fiksNULL(input$ASA), whoEcog=fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                   malign=as.numeric(input$malign),
                                   op_gruppe=fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi), modGlasgow=fiksNULL(input$modGlasgow))
    RegData <- NorgastUtvalg$RegData
    utvalgTxt <- NorgastUtvalg$utvalgTxt

    RegData <- RegData[!is.na(RegData$Variabel), ]
    my_data <- RegData %>% dplyr::group_by(Sykehusnavn) %>% dplyr::summarise(n = sum(Variabel),
                                                                             d = n())
    my_data <- my_data[my_data$d >= 10, ]
    my_limits   <- fundata(input=my_data,
                           # benchmark=sum(my_data$n)/sum(my_data$d),
                           alpha=0.80,
                           alpha2=0.95,
                           method='approximate',
                           step=1)
    my_limits$lo[my_limits$lo < 0] <- 0
    my_limits$lo2[my_limits$lo2 < 0] <- 0
    my_data$andel <- my_data$n/my_data$d*100
    my_limits[, c("up2", "lo2")] <- 100*my_limits[, c("up2", "lo2")]
    list(my_data=my_data, my_limits=my_limits, utvalgTxt=utvalgTxt)
  }

  vals <- reactiveValues(
    # shus = data.frame(traktdata()$my_data[order(traktdata()$my_data$andel), ], color="blue")
    shus = data.frame()
  )
  observeEvent(input$plot_hover2, {
    vals$shus <- data.frame(traktdata()$my_data[order(traktdata()$my_data$andel), ], color="blue")
    vals$shus$color[round(input$plot_hover2$y)] <- "red"
    vals$shus$color[-round(input$plot_hover2$y)] <- "blue"
  }) #shiny::debounce
  observeEvent(input$plot_hover, {
    vals$shus <- data.frame(traktdata()$my_data[order(traktdata()$my_data$andel), ], color="blue")
    point <- nearPoints(vals$shus, input$plot_hover, xvar = "d", yvar = "andel", threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) != 0) {vals$shus$color <- ifelse(vals$shus$Sykehusnavn==point$Sykehusnavn,'red','blue')}
  })

  output$fig_tunnel <- renderPlot({
    my_limits <- traktdata()$my_limits
    my_data <- traktdata()$my_data
    if (dim(vals$shus)[1]>0) {
      my_data <- merge(my_data, vals$shus[, c("Sykehusnavn", "color")], by="Sykehusnavn")
    } else {my_data$color <- "blue"}
    ggplot(data=my_data, aes(x=d,y=andel, color=color)) +
      geom_line(data=my_limits, aes(x=d, y=up2), colour="blue") +
      geom_line(data=my_limits, aes(x=d, y=lo2), colour="blue") +
      geom_hline(data=my_limits, aes(yintercept=benchmark*100), colour="red") +
      coord_cartesian(ylim=c(0,max(my_limits$up2))) +
      theme_classic()+ geom_point(data=my_data, size=1.5) +
      scale_color_manual(values = c("red" = "red", "blue" = "black")) +
      labs(x = "Antall forløp",  y = "Andel (%)") +
      theme(legend.position = "none")
  })

  output$barplot <- renderPlot({
    my_data <- traktdata()$my_data
    if (dim(vals$shus)[1]>0) {
      my_data <- merge(my_data, vals$shus[, c("Sykehusnavn", "color")], by="Sykehusnavn")
    } else {my_data$color <- "blue"}
    my_data <- my_data[order(my_data$andel), ]
    my_data$Sykehusnavn <- factor(my_data$Sykehusnavn, levels = my_data$Sykehusnavn)
    ggplot(data=my_data, aes(x=Sykehusnavn, y=andel, fill = color)) +
      geom_bar(stat = "identity", width = 0.5) +
      coord_flip() +
      theme_classic() + scale_y_discrete(expand = c(0, 0)) +
      labs(x = "Sykehusnavn",  y = "andel") +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("red" = "red", "blue" = "blue" ))
    #   + theme(axis.line.y = element_blank(),
    #         axis.ticks.y = element_blank(),
    #         axis.text.x.bottom = andel)
  })


  # output$hover_info <- renderUI({
  #   hover <- input$plot_hover
  #   my_data <-traktdata()$my_data
  #   point <- nearPoints(my_data, hover, xvar = "d", yvar = "andel", threshold = 5, maxpoints = 1, addDist = TRUE)
  #   if (nrow(point) == 0) return(NULL)
  #
  #   # calculate point position INSIDE the image as percent of total dimensions
  #   # from left (horizontal) and from top (vertical)
  #   left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  #   top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  #
  #   # calculate distance from left and bottom side of the picture in pixels
  #   left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  #   top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  #
  #   # create style property fot tooltip
  #   # background color is set so tooltip is a bit transparent
  #   # z-index is set so we are sure are tooltip will be on top
  #   style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
  #                   "left:", left_px + 2, "px; top:", top_px + 2, "px;")
  #
  #   # actual tooltip created as wellPanel
  #   wellPanel(
  #     style = style,
  #     p(HTML(paste0("<b> Avdeling: </b>", point$Sykehusnavn, "<br/>",
  #                   "<b> Andel: </b>", round(point$andel, 1), "<b> % </b>","<br/>",
  #                   "<b> N: </b>", point$d, "<br/>")))
  #   )
  # })

  # output$hover_info_verbatim <- renderPrint({
  #   # ns <- session$ns
  #   # cat("input$plot_hover:\n")
  #   str(input$plot_hover)
  # })

  output$hover_info_verbatim <- renderUI({
    if (dim(vals$shus)[1]>0) {
      HTML(paste0("<b> Avdeling: </b>", vals$shus$Sykehusnavn[vals$shus$color=="red"], "<br/>",
                  "<b> Andel: </b>", round(vals$shus$andel[vals$shus$color=="red"], 1), "<b> % </b>","<br/>",
                  "<b> n: </b>", vals$shus$n[vals$shus$color=="red"], "<br/>",
                  "<b> N: </b>", vals$shus$d[vals$shus$color=="red"], "<br/>"))
    }
  })


}
# output$fig_andel_grvar <- renderPlot({
#   norgast::NorgastFigAndelerGrVar(RegData, valgtVar=input$valgtVar, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
#                                   minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
#                                   inkl_konf = fiksNULL(input$inkl_konf), malign = as.numeric(input$malign), Ngrense=10,
#                                   elektiv=as.numeric(input$elektiv), BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
#                                   minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA= fiksNULL(input$ASA),
#                                   whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling), modGlasgow = fiksNULL(input$modGlasgow),
#                                   op_gruppe = fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi), hastegrad = as.numeric(input$hastegrad))
# }, width = 700, height = 700)

