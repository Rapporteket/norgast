# Modul for Administrative tabeller-fane i NoRGast sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# inheritParams norgastFigAndeler
#
# return Modulfunksjoner til Administrative tabeller


admtab_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_adm_panel"),
      conditionalPanel(condition = paste0("input['", ns("admtabeller"), "'] == 'id_ant_skjema'"),
                       dateRangeInput(inputId=ns("datovalg_adm"), label = "Dato fra og til", min = '2014-01-01', language = "nb",
                                      max = Sys.Date(), start  = Sys.Date() %m-% months(12), end = Sys.Date(), separator = " til ")#,
      #                  selectInput(inputId = ns("op_gruppe_adm"), label = "Velg reseksjonsgruppe(r)",
      #                              choices = BrValg$reseksjonsgrupper, multiple = TRUE),
      #                  uiOutput(outputId = ns('ncsp_adm'))
      ),
      checkboxInput(inputId = ns("kun_oblig"), label = "Inkluder kun obligatoriske reseksjoner. Merk: Hvis du krysser av for denne så
                    inkluderes kun forløp med ferdigstilt basisregistrering.", value = F),
      # bsTooltip(id = ns("kun_oblig"), title= 'Dette valget filtrerer bort alle forløp som ikke har ferdigstilt basisregistrering
                # siden prosedyrekode kun overføres til Rapporteket for ferdigstilte registreringer.'),
      conditionalPanel(condition = paste0("input['", ns("admtabeller"), "'] == 'id_ant_tid'"),
                       selectInput(inputId = ns("adm_tidsenhet"), label = "Velg tidsenhet",
                                   choices = c('Måneder'=1, 'År'=2)),
                       conditionalPanel(condition = paste0("input['", ns("adm_tidsenhet"), "'] == '1'"),
                                        norgast::dateInput2(inputId=ns("datovalg_adm_tid_mnd"), label = "Vis til og med måned: ", min = '2014-01-01',
                                                            max = Sys.Date(), value = Sys.Date(), minview = 'months', format = "MM yyyy", language="no"),
                                        sliderInput(inputId=ns("ant_mnd"), label = "Antall måneder", min = 1, max = 24, value = 12, step = 1)),
                       conditionalPanel(condition = paste0("input['", ns("adm_tidsenhet"), "'] == '2'"),
                                        norgast::dateInput2(inputId=ns("datovalg_adm_tid_aar"), label = "Vis til og med år: ", min = '2014-01-01',
                                                            max = Sys.Date(), value = Sys.Date(), minview = 'years', format = "yyyy", language="no"),
                                        sliderInput(inputId= ns("ant_aar"), label = "Antall år", min = 1, max = 10, value = 5, step = 1)),
                       selectInput(inputId = ns("regstatus_tid"), label = "Skjemastatus",
                                   choices = c('Ferdige forløp'=1, 'Oppfølging i kladd'=2, 'Ferdig basisreg. oppfølging mangler'=3,
                                               'Basisreg. i kladd'=4))
      ),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(tabsetPanel(id= ns("admtabeller"),
                          tabPanel("Antall skjema", value = "id_ant_skjema",
                                   h4(tags$b(tags$u('Denne tabellen gir en avdelingsvis oversikt over innregistreringer i NoRGast:'))),
                                   h4(tags$b('Ferdige forløp '), 'viser antall forløp med ferdigstilt basisregistrering og oppfølging.'),
                                   h4(tags$b('Oppfølging i kladd '), 'viser antall forløp med ferdigstilt basisregistrering og oppfølging i kladd.'),
                                   h4(tags$b('Ferdig basisreg. oppfølging mangler '), 'viser antall forløp med ferdigstilt basisregistrering og ikke påbegynt eller slettet oppfølging'),
                                   h4(tags$b('Basisreg. i kladd '), 'viser antallet basisregistreringer i kladd.'),
                                   br(),
                                   br(),
                                   DTOutput(ns("Tabell_adm1")), downloadButton(ns("lastNed_adm1"), "Last ned tabell")),
                          tabPanel("Registreringer over tid", value = "id_ant_tid",
                                   # textOutput(ns("debug_greier1")),
                                   # textOutput(ns("debug_greier2")),
                                   DTOutput(ns("Tabell_adm2")), downloadButton(ns("lastNed_adm2"), "Last ned tabell")
                          )
    )
    )

  )
}


admtab <- function(input, output, session, reshID, RegData, userRole, hvd_session, skjemaoversikt){

  observeEvent(input$reset_input, {
    shinyjs::reset("id_adm_panel")
  })

  output$ncsp_adm <- renderUI({
    ns <- session$ns
    if (!is.null(input$op_gruppe_adm)) {
      selectInput(inputId = ns("ncsp_verdi_adm"), label = "NCSP koder (velg en eller flere)",
                  choices = if (!is.null(input$op_gruppe_adm)) {setNames(substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in%
                                                                                                                 as.numeric(input$op_gruppe_adm)])), 1, 5),
                                                                     sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe_adm)])))
                  }, multiple = TRUE)
    }
  })

  antskjema <- function() {

    tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering', c("ForlopsID", "SkjemaStatus", "HovedDato", "OpprettetDato", "Sykehusnavn", "AvdRESH")],
                 skjemaoversikt[skjemaoversikt$Skjemanavn=='Reinnleggelse/oppføl', c("ForlopsID", "SkjemaStatus")],
                 by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))

    if (input$kun_oblig) {
      tmp <- tmp[tmp$ForlopsID %in% RegData$ForlopsID[RegData$Op_gr %in% 1:7], ]
    }

    tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
    tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
    tmp$HovedDato[is.na(tmp$HovedDato)] <- tmp$OpprettetDato[is.na(tmp$HovedDato)]
    tmp <- merge(tmp, RegData[,c("ForlopsID", "Op_gr")], by = "ForlopsID", all.x = T)
    if (!is.null(input$op_gruppe_adm)) {tmp <- tmp[which(RegData$Op_gr %in% as.numeric(input$op_gruppe_adm)), ]}
    if (!is.null(input$ncsp_verdi_adm)) {tmp <- tmp[which(substr(RegData$Hovedoperasjon, 1, 5) %in% input$ncsp_verdi_adm), ]}

    aux <- tmp %>% filter(HovedDato >= input$datovalg_adm[1] & HovedDato <= input$datovalg_adm[2]) %>%
      group_by(Sykehusnavn) %>% summarise('Ferdige forløp' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==1, na.rm = T),
                                          'Oppfølging i kladd' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==0, na.rm = T),
                                          'Ferdig basisreg. oppfølging mangler' = sum(SkjemaStatus==1 & is.na(SkjemaStatus_oppf), na.rm = T),
                                          'Basisreg i kladd' = sum(SkjemaStatus==0, na.rm = T),
                                          'N' = n())
    aux2 <- tmp %>% filter(HovedDato >= input$datovalg_adm[1] & HovedDato <= input$datovalg_adm[2]) %>%
      summarise('Ferdige forløp' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==1, na.rm = T),
                'Oppfølging i kladd' = sum(SkjemaStatus==1 & SkjemaStatus_oppf==0, na.rm = T),
                'Ferdig basisreg. oppfølging mangler' = sum(SkjemaStatus==1 & is.na(SkjemaStatus_oppf), na.rm = T),
                'Basisreg i kladd' = sum(SkjemaStatus==0, na.rm = T),
                'N' = n())

    ant_skjema <- bind_rows(aux, bind_cols(tibble(Sykehusnavn='Totalt'), aux2))

    sketch <- htmltools::withTags(table(
      tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)


  }

  output$Tabell_adm1 = renderDT(
    datatable(antskjema()$ant_skjema[-dim(antskjema()$ant_skjema)[1], ],
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

      tilDato <- as.Date(paste0(input$datovalg_adm_tid_mnd))

      # fraDato <- as.Date(input$datovalg_adm_tid_mnd) %m-% months(input$ant_mnd) %>% floor_date(unit="months")
      fraDato <- tilDato %m-% months(as.numeric(input$ant_mnd)) %>% floor_date(unit="months")
      tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering', c("ForlopsID", "SkjemaStatus", "HovedDato", "OpprettetDato", "Sykehusnavn", "AvdRESH")],
                   skjemaoversikt[skjemaoversikt$Skjemanavn=='Reinnleggelse/oppføl', c("ForlopsID", "SkjemaStatus")],
                   by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))

      if (input$kun_oblig) {
        tmp <- tmp[tmp$ForlopsID %in% RegData$ForlopsID[RegData$Op_gr %in% 1:7], ]
      }

      tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
      tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
      tmp$HovedDato[is.na(tmp$HovedDato)] <- as.Date(tmp$OpprettetDato[is.na(tmp$HovedDato)])

      # aux <- tmp[tmp$HovedDato >= fraDato & tmp$HovedDato <= input$datovalg_adm_tid_mnd, ]
      aux <- tmp

      # aux$mnd <- factor(format(aux$HovedDato, format='%b-%y'), levels = format(seq(as.Date(fraDato),as.Date(input$datovalg_adm_tid_mnd), by="month"), "%b-%y"))
      aux$mnd <- factor(format(aux$HovedDato, format='%b-%y'), levels = format(seq(fraDato, tilDato, by="month"), "%b-%y"))

      ant_skjema <- switch (input$regstatus_tid,
                            '1' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==1) , c('Sykehusnavn', 'mnd')]))),
                            '2' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==0) , c('Sykehusnavn', 'mnd')]))),
                            '3' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & is.na(aux$SkjemaStatus_oppf)) , c('Sykehusnavn', 'mnd')]))),
                            '4' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==0) , c('Sykehusnavn', 'mnd')])))
      ) %>% as_tibble(rownames = 'Sykehusnavn')
    }

    if (input$adm_tidsenhet == 2) {

      fraDato <- as.Date(input$datovalg_adm_tid_aar) %m-% years(input$ant_aar) %>% floor_date(unit="years")
      tmp <- merge(skjemaoversikt[skjemaoversikt$Skjemanavn=='Registrering', c("ForlopsID", "SkjemaStatus", "HovedDato", "OpprettetDato", "Sykehusnavn", "AvdRESH")],
                   skjemaoversikt[skjemaoversikt$Skjemanavn=='Reinnleggelse/oppføl', c("ForlopsID", "SkjemaStatus")],
                   by = 'ForlopsID', all.x = T, suffixes = c('', '_oppf'))

      if (input$kun_oblig) {
        tmp <- tmp[tmp$ForlopsID %in% RegData$ForlopsID[RegData$Op_gr %in% 1:7], ]
      }

      tmp$SkjemaStatus[tmp$SkjemaStatus==-1] <- 0
      tmp$SkjemaStatus_oppf[tmp$SkjemaStatus_oppf==-1] <- 0
      tmp$HovedDato[is.na(tmp$HovedDato)] <- as.Date(tmp$OpprettetDato[is.na(tmp$HovedDato)])

      # aux <- tmp[tmp$HovedDato >= fraDato & tmp$HovedDato <= input$datovalg_adm_tid_aar, ]
      aux <- tmp

      aux$mnd <- factor(format(aux$HovedDato, format='%Y'), levels = format(seq(as.Date(fraDato),as.Date(input$datovalg_adm_tid_aar), by="year"), "%Y"))

      ant_skjema <- switch (input$regstatus_tid,
                            '1' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==1) , c('Sykehusnavn', 'mnd')]))),
                            '2' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & aux$SkjemaStatus_oppf==0) , c('Sykehusnavn', 'mnd')]))),
                            '3' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==1 & is.na(aux$SkjemaStatus_oppf)) , c('Sykehusnavn', 'mnd')]))),
                            '4' = as.data.frame.matrix(addmargins(table(aux[which(aux$SkjemaStatus==0) , c('Sykehusnavn', 'mnd')])))
      ) %>% as_tibble(rownames = 'Sykehusnavn')
    }

    sketch <- htmltools::withTags(table(
      tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)

  }

  output$Tabell_adm2 = renderDT(
    datatable(andre_adm_tab()$ant_skjema[-dim(andre_adm_tab()$ant_skjema)[1], ],
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
      raplog::repLogger(
        session = hvd_session,
        msg = mld_adm1
      )

      shinyjs::onclick(
        "lastNed_adm1",
        raplog::repLogger(
          session = hvd_session,
          msg = paste0("NoRGast: nedlasting tabell: Antall skjema, dato ",
                       input$datovalg_adm[1], ' til ', input$datovalg_adm[2])
        )
      )
      shinyjs::onclick(
        "lastNed_adm2",
        raplog::repLogger(
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
