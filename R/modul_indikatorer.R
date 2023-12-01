#' UI-modul for iindikatorfigurer i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul fordelingsfigur
#'
#' @export
indikatorfig_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      width = 3,
      id = ns("id_indikator_panel"),
      selectInput(
        inputId = ns("valgtVar2"), label = "Velg indikator",
        choices = c("Generell: Premorbid vekttap registrert" = "norgast_vekt_reg",
                    "Generell: Aktiv kontroll" = "norgast_aktivkontroll",
                    "Generell: Sårruptur" = "norgast_saarruptur",
                    "Kolon: Anastomoselekkasje" = "norgast_lekkasje_tykktarm",
                    "Kolon: Reoperasjon" = "relap_kolon",
                    "Kolon: Laparoskopi" = "norgast_kikkhullsteknikk_tykktarm",
                    "Kolon: Konverteringsrate" = "konv_rate_kolon",
                    "Rektum: Anastomoselekkasje" = "norgast_lekkasje_endetarm",
                    "Rektum: Reoperasjon" = "relap_rektum",
                    "Rektum: Konverteringsrate" = "konv_rate_rektum",
                    "Rektum: Laparoskopi" = "norgast_kikkhullsteknikk_endetarm",
                    "Øsofagus: 90-dagers dødelighet" = "norgast_avdoede_spiseroer",
                    "Øsofagus: Anastomoselekkasje" = "anastomoselekk_osofagus",
                    "Ventrikkel: 90-dagers dødelighet" = "norgast_avdoede_magesekk",
                    "Ventrikkel: Anastomoselekkasje" = "anastomoselekk_ventrikkel",
                    "Ventrikkel: Reoperasjon" = "relap_ventrikkel",
                    "Whipple: 90-dagers dødelighet" = "norgast_avdoede_bukspytt_tolv",
                    "Whipple: Postoperativ pankreasfistel" = "CR_POPF_whipple",
                    "Whipple: Reoperasjon" = "relap_whipple",
                    "Distal pankreas: Postoperativ pankreasfistel" = "CR_POPF_distal",
                    "Distal pankreas: Laparoskopi" = "kikkhullsteknikk_distal",
                    "Lever: 90-dagers dødelighet" = "norgast_avdoede_lever",
                    "Lever: Reoperasjon" = "relap_lever",
                    "Lever: Laparoskopi" = "norgast_kikkhullsteknikk_lever")
      ),
      uiOutput(outputId = ns('tilAar_ui')),
      uiOutput(outputId = ns('valgtShus_ui')),
      sliderInput(ns("skriftStr"), "Skriftstørrelse sykehusnavn",
                  min = 0.5, max = 1.8,
                  value = 1.2, step = 0.05, ticks = F),
      checkboxInput(ns("pst_kolonne"), "Prosenttall i kolonner", value = FALSE),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur", value = "fig",
                           plotOutput(ns("Figur1"), height="auto"),
                           downloadButton(ns("lastNedBilde"), "Last ned figur")),
                  tabPanel("Tabell", value = "tab",
                           uiOutput(ns("utvalg")),
                           br(),
                           DT::DTOutput(ns("tabell"))
                  )
      )
    )
  )

}

#' Server-modul for indikatorfigurer i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul fordelingsfigur
#'
#' @export
indikatorfig <- function(input, output, session, reshID, RegData,
                         userRole, hvd_session, BrValg){

  RegData <- RegData[RegData$Op_gr %in% 1:8, ]

  observeEvent(input$reset_input, {
    shinyjs::reset("id_indikator_panel")
  })

  observe(
    if (userRole != 'SC') {
      shinyjs::hide(id = 'valgtShus')
    })

  output$valgtShus_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("valgtShus"), label = "Fjern sykehus pga. lav dekningsgrad",
                choices = sort(unique(RegData$SykehusNavn)), multiple = TRUE)
  })

  output$tilAar_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("tilAar"), label = "T.o.m. år",
                choices = rev((min(RegData$Aar)+2):max(RegData$Aar)))
  })



  indikatordata <- reactive({
    indikatordata <- norgastBeregnIndikator(
      RegData = if(
        !is.null(input$tilAar)) {RegData[which(RegData$Aar <= as.numeric(input$tilAar)), ]
      } else {RegData},
      ind_id = input$valgtVar2
    )
  })

  output$Figur1 <- renderPlot({
    norgastPlotIndikator(AntTilfeller = indikatordata()$AntTilfeller,
                         N = indikatordata()$N,
                         andeler = indikatordata()$andeler,
                         decreasing = indikatordata()$decreasing,
                         terskel = indikatordata()$terskel,
                         minstekrav = indikatordata()$minstekrav,
                         maal = indikatordata()$maal,
                         utvalgTxt = indikatordata()$utvalgTxt,
                         tittel = indikatordata()$tittel,
                         skriftStr = input$skriftStr,
                         lavDG = input$valgtShus,
                         maalretn = indikatordata()$maalretn,
                         prikktall = !input$pst_kolonne,
                         pst_kolonne = input$pst_kolonne)
  }, width = 600, height = 700)



  output$utvalg <- renderUI({
    TabellData <- indikatordata()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  lagTabell <- function() {

    Utdata <- indikatordata()
    ant_tilfeller <- Utdata$AntTilfeller
    N <- Utdata$N
    andeler <- round(ant_tilfeller/N*100, 1)
    names(ant_tilfeller) <- paste0("Antall_", names(ant_tilfeller))
    names(N) <- paste0("N_", names(N))
    aux <- dplyr::bind_cols(ant_tilfeller, N, andeler) %>%
      dplyr::mutate(Avdeling = rownames(.)) %>%
      dplyr::select(Avdeling, dplyr::everything())

    sketch = htmltools::withTags(table(
      # class = 'display yohannes',
      thead(
        tr(
          th(rowspan = 2, 'Avdeling'),
          th(colspan = 2, 'Antall'),
          th(colspan = 2, 'N'),
          th(colspan = 2, 'Andel')
        ),
        tr(
          lapply(rep(names(andeler), 3), th)
        )
      )
    ))
    # {text-align: center;}
    # datatable(aux, container = sketch, rownames = TRUE)
    list(Tabell=aux, sketch=sketch)

  }

  output$tabell <- DT::renderDT(
    DT::datatable(lagTabell()$Tabell,
                  container = lagTabell()$sketch,
                  rownames = FALSE,
                  extensions = 'Buttons',

                  options = list(
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'Bliftsp',
                    buttons = c('copy', 'csv', 'excel'),
                    pageLength = 40
                  ),
                  class = "display")
  )



  output$lastNedBilde <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
    },

    content = function(file){
      norgastPlotIndikator(AntTilfeller = indikatordata()$AntTilfeller,
                           N = indikatordata()$N,
                           andeler = indikatordata()$andeler,
                           decreasing = indikatordata()$decreasing,
                           terskel = indikatordata()$terskel,
                           minstekrav = indikatordata()$minstekrav,
                           maal = indikatordata()$maal,
                           utvalgTxt = indikatordata()$utvalgTxt,
                           tittel = indikatordata()$tittel,
                           skriftStr = input$skriftStr,
                           lavDG = input$valgtShus,
                           maalretn = indikatordata()$maalretn,
                           prikktall = !input$pst_kolonne,
                           pst_kolonne = input$pst_kolonne,
                           outfile = file)
    }
  )

  shiny::observe({
    if (rapbase::isRapContext()) {
      if (req(input$tab) == "fig") {
        mld_fordeling <- paste0(
          "NORGAST: Indikatorfigur, variabel - ",
          input$valgtVar)
      }
      if (req(input$tab) == "tab") {
        mld_fordeling <- paste(
          "NORGAST: Indikatortabell, variabel - ",
          input$valgtVar)
      }
      rapbase::repLogger(
        session = hvd_session,
        msg = mld_fordeling
      )
      shinyjs::onclick(
        "lastNedBilde",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NORGAST: nedlasting indikatorfigur, variabel -",
            input$valgtVar
          )
        )
      )
    }
  })










}
