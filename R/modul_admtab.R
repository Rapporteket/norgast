#' UI-modul for Administrative tabeller-fane i NoRGast sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til Administrative tabeller
#'
#' @export
admtab_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      width = 3,
      id = ns("id_adm_panel"),
      div(
        id = ns("fane1"),
        dateRangeInput(inputId=ns("datovalg_adm"), label = "Dato fra og til",
                       min = '2014-01-01', language = "nb",
                       max = Sys.Date(),
                       start  = lubridate::floor_date(lubridate::today() -
                                                        lubridate::years(1),
                                                      unit = "year"),
                       end = Sys.Date(), separator = " til ")
      ),
      shinyjs::hidden(selectInput(inputId = ns("adm_tidsenhet"),
                                  label = "Velg tidsenhet",
                                  choices = c('Måneder'=1, 'År'=2))),
      shiny::uiOutput(ns("tab_mnd")),
      shiny::uiOutput(ns("tab_aar")),
      uiOutput(outputId = ns('op_gruppe_ui')),
      uiOutput(outputId = ns('ncsp')),
      shinyjs::hidden(
        selectInput(
          inputId = ns("regstatus_tid"),
          label = "Skjemastatus",
          choices = c('Ferdige forløp'=1, 'Oppfølging i kladd'=2,
                      'Ferdig basisreg. oppfølging mangler'=3,
                      'Basisreg. i kladd'=4))),
      checkboxInput(inputId = ns("kun_oblig"),
                    label = "Inkluder kun obligatoriske reseksjoner",
                    value = F),
      uiOutput(outputId = ns('valgtShus_ui')),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(tabsetPanel(
      id= ns("admtabeller"),
      tabPanel(
        "Antall skjema", value = "id_ant_skjema",
        h4(tags$b(tags$u('Denne tabellen gir en avdelingsvis oversikt over
                         innregistreringer i NoRGast:'))),
        h4(tags$b('Ferdige forløp '), 'viser antall forløp med ferdigstilt
           basisregistrering og oppfølging.'),
        h4(tags$b('Oppfølging i kladd '), 'viser antall forløp med ferdigstilt
           basisregistrering og oppfølging i kladd.'),
        h4(tags$b('Ferdig basisreg. oppfølging mangler '), 'viser antall forløp
           med ferdigstilt basisregistrering og ikke påbegynt eller slettet
           oppfølging'),
        h4(tags$b('Basisreg. i kladd '), 'viser antallet basisregistreringer
           i kladd.'),
        br(),
        br(),
        DT::DTOutput(ns("Tabell_adm1")),
        downloadButton(ns("lastNed_adm1"), "Last ned tabell")
      ),
      tabPanel(
        "Registreringer over tid", value = "id_ant_tid",
        DT::DTOutput(ns("Tabell_adm2")),
        downloadButton(ns("lastNed_adm2"), "Last ned tabell")
      )
    )
    )
  )
}

#' Serverdel av modul for Administrative tabeller-fane i NoRGast sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til Administrative tabeller
#'
#' @export
admtab <- function(input, output, session, reshID, RegData, userRole,
                   hvd_session, skjemaoversikt, BrValg){

  observeEvent(input$reset_input, {
    shinyjs::reset("id_adm_panel")
  })

  observe(
    if (userRole != 'SC') {
      shinyjs::hide(id = 'valgtShus_ui')
    })

  output$op_gruppe_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
                choices = BrValg$reseksjonsgrupper, multiple = TRUE)
  })

  output$valgtShus_ui <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                choices = BrValg$sykehus, multiple = TRUE)
  })

  output$ncsp <- renderUI({
    ns <- session$ns
    if (!is.null(input$op_gruppe)) {
      selectInput(inputId = ns("ncsp_verdi"),
                  label = "NCSP koder (velg en eller flere)",
                  choices = if (!is.null(input$op_gruppe)) {
                    RegData %>%
                      dplyr::select(Hovedoperasjon, Op_gr) %>%
                      dplyr::filter(Op_gr %in% as.numeric(input$op_gruppe)) %>%
                      dplyr::select(Hovedoperasjon) %>%
                      unique() %>%
                      dplyr::arrange(Hovedoperasjon) %>%
                      dplyr::mutate(NCSP = substr(Hovedoperasjon, 1, 5)) %>%
                      dplyr::pull(NCSP, Hovedoperasjon)
                  }, multiple = TRUE)
    }
  })

  observe(
    if (input$admtabeller == "id_ant_skjema") {
      shinyjs::hide(id = 'adm_tidsenhet')
      shinyjs::hide(id = 'tab_mnd')
      shinyjs::hide(id = 'tab_aar')
      shinyjs::hide(id = 'regstatus_tid')
      shinyjs::show(id = 'fane1')
    } else if (input$admtabeller == "id_ant_tid") {
      shinyjs::hide(id = 'fane1')
      shinyjs::show(id = 'adm_tidsenhet')
      shinyjs::show(id = 'tab_mnd')
      shinyjs::show(id = 'tab_aar')
      shinyjs::show(id = 'regstatus_tid')
    }
  )

  output$tab_mnd <- shiny::renderUI({
    ns <- session$ns
    req(input$adm_tidsenhet == '1')
    tagList(
      shinyWidgets::airDatepickerInput(
        inputId=ns("datovalg_adm_tid_mnd"),
        label = "Vis til og med måned: ", minDate = '2014-01-01',
        maxDate = Sys.Date(), value = Sys.Date(),
        view = "months", minView = 'months',
        dateFormat = "MM yyyy", language="da"),
      sliderInput(inputId=ns("ant_mnd"),
                  label = "Antall måneder", min = 1, max = 24,
                  value = 12, step = 1)
    )
  })

  output$tab_aar <- shiny::renderUI({
    ns <- session$ns
    req(input$adm_tidsenhet == '2')
    tagList(
      shinyWidgets::airDatepickerInput(
        inputId=ns("datovalg_adm_tid_aar"),
        label = "Vis til og med år: ", minDate = '2014-01-01',
        maxDate = Sys.Date(), value = Sys.Date(),
        view = "years", minView = 'years',
        dateFormat = "yyyy", language="da"),
      sliderInput(inputId= ns("ant_aar"),
                  label = "Antall år", min = 1, max = 10,
                  value = 5, step = 1)
    )
  })


  antskjema <- function() {
    # req(input$admtabeller == "id_ant_skjema")

    tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering', c("ForlopsID", "SkjemaStatus", "HovedDato", "OpprettetDato", "Sykehusnavn", "AvdRESH", "Op_gr", "Hovedoperasjon")],
                 skjemaoversikt[skjemaoversikt$Skjemanavn=='Reinnleggelse/oppføl', c("ForlopsID", "SkjemaStatus")],
                 by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))

    if (input$kun_oblig) {
      # tmp <- tmp[tmp$ForlopsID %in% RegData$ForlopsID[RegData$Op_gr %in% 1:7], ]
      tmp <- tmp[tmp$Op_gr %in% 1:7, ]
    }

    tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
    tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
    tmp$HovedDato[is.na(tmp$HovedDato)] <- tmp$OpprettetDato[is.na(tmp$HovedDato)]
    # tmp <- merge(tmp, RegData[,c("ForlopsID", "Op_gr", "Hovedoperasjon")], by = "ForlopsID", all.x = T)
    if (!is.null(input$op_gruppe)) {tmp <- tmp[which(tmp$Op_gr %in% as.numeric(input$op_gruppe)), ]}
    if (!is.null(input$ncsp_verdi)) {tmp <- tmp[which(substr(tmp$Hovedoperasjon, 1, 5) %in% input$ncsp_verdi), ]}
    if (!is.null(input$valgtShus)) {tmp <- tmp[tmp$AvdRESH %in% as.numeric(input$valgtShus), ]}

    aux <- tmp %>%
      dplyr::filter(HovedDato >= input$datovalg_adm[1] &
                      HovedDato <= input$datovalg_adm[2]) %>%
      dplyr::group_by(Sykehusnavn) %>%
      dplyr::summarise(
        'Ferdige forløp' = sum(SkjemaStatus==1 &
                                 SkjemaStatus_oppf==1, na.rm = T),
        'Oppfølging i kladd' = sum(SkjemaStatus==1 &
                                     SkjemaStatus_oppf==0, na.rm = T),
        'Ferdig basisreg. oppfølging mangler' = sum(SkjemaStatus==1 &
                                                      is.na(SkjemaStatus_oppf), na.rm = T),
        'Basisreg i kladd' = sum(SkjemaStatus==0, na.rm = T),
        'N' = dplyr::n())
    aux2 <- tmp %>%
      dplyr::filter(HovedDato >= input$datovalg_adm[1] &
                      HovedDato <= input$datovalg_adm[2]) %>%
      dplyr::summarise(
        'Ferdige forløp' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==1, na.rm = T),
        'Oppfølging i kladd' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==0, na.rm = T),
        'Ferdig basisreg. oppfølging mangler' = sum(SkjemaStatus==1 & is.na(SkjemaStatus_oppf), na.rm = T),
        'Basisreg i kladd' = sum(SkjemaStatus==0, na.rm = T),
        'N' = dplyr::n())

    ant_skjema <- dplyr::bind_rows(aux, dplyr::bind_cols(dplyr::tibble(Sykehusnavn='Totalt'), aux2))

    sketch <- htmltools::withTags(table(
      DT::tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      DT::tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)


  }

  output$Tabell_adm1 = DT::renderDT(
    DT::datatable(antskjema()$ant_skjema[-dim(antskjema()$ant_skjema)[1], ],
                  container = antskjema()$sketch,
                  rownames = F,
                  options = list(pageLength = 40)
    )
  )


  output$lastNed_adm1 <- downloadHandler(
    filename = function(){
      paste0('Regoversikt', Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- antskjema()$ant_skjema
      write.csv3(TabellData, file, row.names = F)
    }
  )


  andre_adm_tab <- function() {
    if (input$adm_tidsenhet == 1) {
      req(input$datovalg_adm_tid_mnd)
      tilDato <- as.Date(paste0(input$datovalg_adm_tid_mnd))
      # fraDato <- tilDato %m-% months(as.numeric(input$ant_mnd)) %>%
      #   lubridate::floor_date(unit="months")
      fraDato <- lubridate::`%m-%`(tilDato, months(as.numeric(input$ant_mnd))) %>%
        lubridate::floor_date(unit="months")
      tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering',
                                  c("ForlopsID", "SkjemaStatus", "HovedDato",
                                    "OpprettetDato", "Sykehusnavn", "AvdRESH",
                                    "Op_gr", "Hovedoperasjon")],
                   skjemaoversikt[skjemaoversikt$Skjemanavn=='Reinnleggelse/oppføl',
                                  c("ForlopsID", "SkjemaStatus")],
                   by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))

      if (input$kun_oblig) {
        tmp <- tmp[tmp$Op_gr %in% 1:7, ]
      }

      tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
      tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
      tmp$HovedDato[is.na(tmp$HovedDato)] <- as.Date(tmp$OpprettetDato[is.na(tmp$HovedDato)])
      if (!is.null(input$op_gruppe)) {tmp <- tmp[which(tmp$Op_gr %in% as.numeric(input$op_gruppe)), ]}
      if (!is.null(input$ncsp_verdi)) {tmp <- tmp[which(substr(tmp$Hovedoperasjon, 1, 5) %in% input$ncsp_verdi), ]}

      aux <- tmp
      aux$mnd <- factor(format(aux$HovedDato, format='%b-%y'), levels = format(seq(fraDato, tilDato, by="month"), "%b-%y"))

      ant_skjema <- switch (req(input$regstatus_tid),
                            '1' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==1) , c('Sykehusnavn', 'mnd')]))),
                            '2' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==0) , c('Sykehusnavn', 'mnd')]))),
                            '3' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & is.na(aux$SkjemaStatus_oppf)) , c('Sykehusnavn', 'mnd')]))),
                            '4' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==0) , c('Sykehusnavn', 'mnd')])))
      ) %>% dplyr::as_tibble(rownames = 'Sykehusnavn')
    }

    if (input$adm_tidsenhet == 2) {
      req(input$datovalg_adm_tid_aar)
      # print(input$datovalg_adm_tid_aar)
      fraDato <- as.Date(input$datovalg_adm_tid_aar) %m-% years(input$ant_aar) %>% floor_date(unit="years")
      tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering', c("ForlopsID", "SkjemaStatus", "HovedDato",
                                                                               "OpprettetDato", "Sykehusnavn", "AvdRESH",
                                                                               "Op_gr", "Hovedoperasjon")],
                   skjemaoversikt[skjemaoversikt$Skjemanavn=='Reinnleggelse/oppføl', c("ForlopsID", "SkjemaStatus")],
                   by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))

      if (input$kun_oblig) {
        # tmp <- tmp[tmp$ForlopsID %in% RegData$ForlopsID[RegData$Op_gr %in% 1:7], ]
        tmp <- tmp[tmp$Op_gr %in% 1:7, ]
      }

      tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
      tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
      tmp$HovedDato[is.na(tmp$HovedDato)] <- as.Date(tmp$OpprettetDato[is.na(tmp$HovedDato)])
      if (!is.null(input$op_gruppe)) {tmp <- tmp[which(tmp$Op_gr %in% as.numeric(input$op_gruppe)), ]}
      if (!is.null(input$ncsp_verdi)) {tmp <- tmp[which(substr(tmp$Hovedoperasjon, 1, 5) %in% input$ncsp_verdi), ]}
      aux <- tmp

      aux$mnd <- factor(format(aux$HovedDato, format='%Y'), levels = format(seq(as.Date(fraDato),as.Date(input$datovalg_adm_tid_aar), by="year"), "%Y"))

      ant_skjema <- switch (req(input$regstatus_tid),
                            '1' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==1) , c('Sykehusnavn', 'mnd')]))),
                            '2' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==0) , c('Sykehusnavn', 'mnd')]))),
                            '3' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & is.na(aux$SkjemaStatus_oppf)) , c('Sykehusnavn', 'mnd')]))),
                            '4' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==0) , c('Sykehusnavn', 'mnd')])))
      ) %>% dplyr::as_tibble(rownames = 'Sykehusnavn')
    }

    sketch <- htmltools::withTags(table(
      DT::tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      DT::tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)

  }

  output$Tabell_adm2 = DT::renderDT(
    DT::datatable(andre_adm_tab()$ant_skjema[-dim(andre_adm_tab()$ant_skjema)[1], ],
                  container = andre_adm_tab()$sketch,
                  rownames = F,
                  options = list(pageLength = 40)
    )
  )

  output$lastNed_adm2 <- downloadHandler(
    filename = function(){
      paste0('Regoversikt_tid', Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- andre_adm_tab()$ant_skjema
      write.csv3(TabellData, file, row.names = F)
    }
  )

  shiny::observe({
    if (rapbase::isRapContext()) {
      if (req(input$admtabeller) == "id_ant_skjema") {
        mld_adm1 <- paste0(
          "NoRGast: Admin. tabell: Antall skjema, dato ",
          input$datovalg_adm[1], ' til ', input$datovalg_adm[2])
      }
      if (req(input$admtabeller) == "id_ant_tid") {
        mld_adm1 <- paste0(
          "NoRGast: Admin. tabell: Antall skjema pr ",
          c('måned', 'år')[as.numeric(input$adm_tidsenhet)], ". ",
          c('Ferdige forløp', 'Oppfølging i kladd', 'Ferdig basisreg. oppfølging mangler',
            'Basisreg. i kladd')[as.numeric(input$regstatus_tid)])
      }
      rapbase::repLogger(
        session = hvd_session,
        msg = mld_adm1
      )

      shinyjs::onclick(
        "lastNed_adm1",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste0("NoRGast: nedlasting tabell: Antall skjema, dato ",
                       input$datovalg_adm[1], ' til ', input$datovalg_adm[2])
        )
      )
      shinyjs::onclick(
        "lastNed_adm2",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste0("NoRGast: nedlasting tabell: Antall skjema pr ",
                       c('måned', 'år')[as.numeric(input$adm_tidsenhet)], ". ",
                       c('Ferdige forløp', 'Oppfølging i kladd', 'Ferdig basisreg. oppfølging mangler',
                         'Basisreg. i kladd')[as.numeric(input$regstatus_tid)])
        )
      )
    }
  })





}
