#' Server-modul for sykehusvise andeler i NoRGast sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @inheritParams norgastFigAndeler
#'
#' @return Serverdelen av sykehusvisning, andeler
#'
sykehusvisning <- function(input, output, session, reshID, RegData, hvd_session){

  fiksNULL <- function(x) {
    if (!is.null(x)) {x} else {''}
  }

  observe(
    if (!is.null(input$tabs_sykehusvisning)) {
      if (input$tabs_sykehusvisning %in%  c("Figur, andeler i stabel", "Tabell, andeler i stabel")){
        shinyjs::hide(id = 'valgtVar')
        shinyjs::hide(id = 'valgtVar_gjsn')
        shinyjs::show(id = 'valgtVar_andel_stabel')
        shinyjs::hide(id = 'inkl_konf')
      }
      if (input$tabs_sykehusvisning %in%  c("Figur, andeler", "Tabell, andeler")) {
        shinyjs::hide(id = 'valgtVar_andel_stabel')
        shinyjs::hide(id = 'valgtVar_gjsn')
        shinyjs::show(id = 'valgtVar')
        shinyjs::show(id = 'inkl_konf')
      }
      if (input$tabs_sykehusvisning %in%  c("Figur, gjennomsnitt", "Tabell, gjennomsnitt")) {
        shinyjs::hide(id = 'valgtVar_andel_stabel')
        shinyjs::show(id = 'valgtVar_gjsn')
        shinyjs::hide(id = 'valgtVar')
        shinyjs::hide(id = 'inkl_konf')
      }

    }
  )


  output$ncsp <- renderUI({
    ns <- session$ns
    if (!is.null(input$op_gruppe)) {
      selectInput(inputId = ns("ncsp_verdi"), label = "Velg NCSP kode(r)",
                  choices = if (!is.null(input$op_gruppe)) {
                    setNames(substr(sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe)])), 1, 5),
                             sort(unique(RegData$Hovedoperasjon[RegData$Op_gr %in% as.numeric(input$op_gruppe)])))
                  }, multiple = TRUE)
    }
  })


  ############### Andeler #######################################################

  output$fig_andel_grvar <- renderPlot({
    norgast::NorgastFigAndelerGrVar(RegData, valgtVar=input$valgtVar, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                    minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
                                    inkl_konf = fiksNULL(input$inkl_konf), malign = as.numeric(input$malign), Ngrense=10,
                                    elektiv=as.numeric(input$elektiv), BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                    minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA= fiksNULL(input$ASA),
                                    whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                    op_gruppe = fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi))
  }, width = 700, height = 700)

  tabellReagerSykehusAndel <- reactive({
    TabellData <- norgast::NorgastFigAndelerGrVar(RegData, valgtVar=input$valgtVar, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                                  minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
                                                  inkl_konf = fiksNULL(input$inkl_konf), malign = as.numeric(input$malign), Ngrense=10,
                                                  elektiv=as.numeric(input$elektiv), BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                                  minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA= fiksNULL(input$ASA),
                                                  whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                                  op_gruppe = fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi))
  })

  output$utvalg_sykehus_andel <- renderUI({
    TabellData <- tabellReagerSykehusAndel()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  output$Tabell_sykehus_andel <- function() {
    utdata <- tabellReagerSykehusAndel()
    Tabell <- tibble(Avdeling = names(utdata$Nvar), Antall=utdata$Nvar, N=utdata$Ngr, Andel = as.numeric(utdata$Nvar/utdata$Ngr*100),
                     KI_nedre=utdata$KI[1,], KI_ovre=utdata$KI[2,])
    Tabell[utdata$Andeler==-0.001, 2:6] <- NA
    Tabell <- Tabell[dim(Tabell)[1]:1, ]
    Tabell %>% knitr::kable("html", digits = c(0,0,0,1,1,1)) %>%
      kable_styling("hover", full_width = F)
  }

  output$lastNed_sykehus_andel <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, '_sykehus_andel_', Sys.time(), '.csv')
    },
    content = function(file){
      utdata <- tabellReagerSykehusAndel()
      Tabell <- tibble(Avdeling = names(utdata$Nvar), Antall=utdata$Nvar, N=utdata$Ngr, Andel = as.numeric(utdata$Nvar/utdata$Ngr*100),
                       KI_nedre=utdata$KI[1,], KI_ovre=utdata$KI[2,])
      Tabell[utdata$Andeler==-0.001, 2:6] <- NA
      Tabell <- Tabell[dim(Tabell)[1]:1, ]
      write.csv2(Tabell, file, row.names = F, na = '')
    }
  )

  output$lastNedBilde_sykehus_andel <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, '_sykehus_andel_', Sys.time(), '.', input$bildeformat)
    },
    content = function(file){
      norgast::NorgastFigAndelerGrVar(RegData, valgtVar=input$valgtVar, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                      minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
                                      inkl_konf = fiksNULL(input$inkl_konf), malign = as.numeric(input$malign), Ngrense=10,
                                      elektiv=as.numeric(input$elektiv), BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                      minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA= fiksNULL(input$ASA),
                                      whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                      op_gruppe = fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi), outfile = file)
    }
  )

  ############### Andeler i stabler #######################################################

  output$fig_andel_grvar_stabel <- renderPlot({
    norgast::NorgastFigAndelStabelGrVar(RegData, valgtVar=input$valgtVar_andel_stabel, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                        minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
                                        malign = as.numeric(input$malign), Ngrense=10,
                                        elektiv=as.numeric(input$elektiv),BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                        minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA= fiksNULL(input$ASA),
                                        whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                        op_gruppe = fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi))
  }, width = 700, height = 700)


  tabellReagerSykehusAndelStabel <- reactive({
    TabellData <- norgast::NorgastFigAndelStabelGrVar(RegData, valgtVar=input$valgtVar_andel_stabel, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                                      minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
                                                      malign = as.numeric(input$malign), Ngrense=10,
                                                      elektiv=as.numeric(input$elektiv),BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                                      minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA= fiksNULL(input$ASA),
                                                      whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                                      op_gruppe = fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi))
  })

  output$utvalg_sykehus_andel_stabel <- renderUI({
    TabellData <- tabellReagerSykehusAndelStabel()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  output$Tabell_sykehus_andel_stabel <- function() {
    TabellData <- tabellReagerSykehusAndelStabel()
    Tabell <- bind_cols(TabellData$antall, TabellData$andeler[, 2:(dim(TabellData$andeler)[2]-1)])
    names(Tabell)[(dim(Tabell)[2]/2 + 2):dim(Tabell)[2]] <- names(Tabell)[2:(dim(Tabell)[2]/2)]
    names(Tabell)[dim(Tabell)[2]/2 + 1] <- 'N'

    Tabell %>% knitr::kable("html", digits = c(rep(0, dim(Tabell)[2]/2 + 1), rep(1, dim(Tabell)[2]/2 - 1))) %>%
      kable_styling("hover", full_width = F) %>%
      add_header_above(c(" ", "Antall" = dim(Tabell)[2]/2 - 1, " ", "Andel (%)" = dim(Tabell)[2]/2 - 1))

  }

  output$lastNedBilde_sykehus_andel_stabel <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_andel_stabel, '_stabel', Sys.time(), '.', input$bildeformat)
    },
    content = function(file){
      norgast::NorgastFigAndelStabelGrVar(RegData, valgtVar=input$valgtVar_andel_stabel, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                          minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
                                          malign = as.numeric(input$malign), Ngrense=10,
                                          elektiv=as.numeric(input$elektiv),BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                          minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA= fiksNULL(input$ASA),
                                          whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                          op_gruppe = fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi), outfile = file)
    }
  )

#################### Gjennomsnitt #################################################

  output$fig_gjsn_grvar <- renderPlot({
    norgast::NorgastFigGjsnGrVar(RegData, valgtVar=input$valgtVar_gjsn, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                 minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
                                 # inkl_konf = fiksNULL(input$inkl_konf),
                                 malign = as.numeric(input$malign), Ngrense=10,
                                 elektiv=as.numeric(input$elektiv), BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                 minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA= fiksNULL(input$ASA),
                                 whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                 op_gruppe = fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi))
  }, width = 700, height = 700)

  tabellReagerSykehusGjsn <- reactive({
    TabellData <- norgast::NorgastFigGjsnGrVar(RegData, valgtVar=input$valgtVar_gjsn, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
                                               # inkl_konf = fiksNULL(input$inkl_konf),
                                               malign = as.numeric(input$malign), Ngrense=10,
                                               elektiv=as.numeric(input$elektiv), BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                               minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA= fiksNULL(input$ASA),
                                               whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                               op_gruppe = fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi))
  })

  output$utvalg_sykehus_gjsn <- renderUI({
    TabellData <- tabellReagerSykehusGjsn()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})

  output$Tabell_sykehus_gjsn <- function() {
    utdata <- tabellReagerSykehusGjsn()
    Tabell <- as_tibble(utdata$res, rownames='Avdeling')
    Tabell[Tabell$N < 10, 2:4] <- NA
    Tabell <- Tabell[order(Tabell$Gjsn, decreasing = T, na.last = T), ]
    Tabell %>% knitr::kable("html", digits = c(0,1,1,1,0)) %>%
      kable_styling("hover", full_width = F)
  }

  output$lastNed_sykehus_gjsn <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_gjsn, '_sykehus_gjsn_', Sys.time(), '.csv')
    },
    content = function(file){
      utdata <- tabellReagerSykehusGjsn()
      Tabell <- as_tibble(utdata$res, rownames='Sykehusnavn')
      Tabell[Tabell$N < 10, 2:4] <- NA
      Tabell <- Tabell[order(Tabell$Gjsn, decreasing = T, na.last = T), ]
      write.csv2(Tabell, file, row.names = F, na = '')
    }
  )

  output$lastNedBilde_sykehus_gjsn <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar_gjsn, '_sykehus_gjsn_', Sys.time(), '.', input$bildeformat)
    },
    content = function(file){
      norgast::NorgastFigGjsnGrVar(RegData, valgtVar=input$valgtVar_gjsn, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                   minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), erMann=as.numeric(input$erMann),
                                   # inkl_konf = fiksNULL(input$inkl_konf),
                                   malign = as.numeric(input$malign), Ngrense=10,
                                   elektiv=as.numeric(input$elektiv), BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                   minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA= fiksNULL(input$ASA),
                                   whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                   op_gruppe = fiksNULL(input$op_gruppe), ncsp = fiksNULL(input$ncsp_verdi), outfile = file)
    }
  )
}










# stabeltabell <- function() {
#   TabellData <- tabellReagerSykehusAndelStabel()
#   aux <- as.data.frame.matrix(TabellData$Antall)
#   aux$N <- TabellData$Ngr
#   aux$Avdeling <- row.names(aux)
#   row.names(aux) <- 1:dim(aux)[1]
#   aux <- aux[, c(dim(aux)[2], dim(aux)[2]-1, 1:(dim(aux)[2]-2))]
#   aux <- rbind(aux, c(Avdeling='Norge', colSums(aux[,-1])))
#   aux[,-1] <- apply(aux[,-1], 2, as.numeric)
#   aux2 <- aux[,-(1:2)]/aux$N*100
#   aux <- cbind(aux, aux2)
#   sketch <- paste0('<table>
#                     <thead>
#                    <tr>
#                    <th rowspan="2">Avdeling</th>
#                    <th rowspan="2">N</th>
#                    <th colspan="', length(TabellData$legendTxt), '">Antall</th>
#                    <th colspan="', length(TabellData$legendTxt), '">Prosent</th>
#                    </tr>
#                    <tr>',
#                    paste(sapply(TabellData$legendTxt,function(i) as.character(tags$th(i))),collapse="\n"),
#                    paste(sapply(TabellData$legendTxt,function(i) as.character(tags$th(i))),collapse="\n"),
#                    '</tr>
#                    </thead>',
#                    tableFooter(c('Totalt' , round(as.numeric(aux[dim(aux)[1], 2:dim(aux)[2]]),1))),
#                    '</table>')
#     # tableFooter(c('Sum' , as.numeric(aux[dim(aux)[1], 2:dim(aux)[2]])))))
#   # sketch <- htmltools::withTags(table(
#   #   tableHeader(aux[-dim(aux)[1], ]),
#   #   tableFooter(c('Totalt' , as.numeric(aux[dim(aux)[1], 2:dim(aux)[2]])))))
#   list(ant_skjema=aux, sketch=sketch)
# }
#
#
# output$Tabell_sykehus_andel_stabel = renderDT(
#   datatable(stabeltabell()$ant_skjema[-dim(stabeltabell()$ant_skjema)[1], ],
#             container = stabeltabell()$sketch,
#             rownames = F,
#             options = list(pageLength = 30)
#   ) %>% formatRound(columns=(dim(stabeltabell()$ant_skjema)[2]/2+2):dim(stabeltabell()$ant_skjema)[2], digits=1)
# )
#
# output$lastNedStabelTabell <- downloadHandler(
#   filename = function(){
#     paste0(input$valgtVar_andel_stabel, '_stabel', Sys.time(), '.csv')
#   },
#   content = function(file){
#     Tabell <- stabeltabell()$ant_skjema
#     write.csv2(Tabell, file, row.names = F, na = '')
#   }
# )
