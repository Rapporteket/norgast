#' Server-modul for sykehusvise andeler i NoRGast sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @inheritParams norgastFigAndeler
#'
#' @return Serverdelen av sykehusvisning, andeler
#'
tidsvisning <- function(input, output, session, reshID, RegData, userRole){

  observe(
    if (userRole != 'SC') {
      shinyjs::hide(id = 'valgtShus')
    })

  fiksNULL <- function(x, erstatt='') {
    if (!is.null(x)) {x} else {erstatt}
  }

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



  output$fig_andel_tid <- renderPlot({
    norgast::NorgastFigAndelTid(RegData, valgtVar=input$valgtVar, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                reshID = reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg), minald=as.numeric(input$alder[1]),
                                maxald=as.numeric(input$alder[2]), valgtShus = fiksNULL(input$valgtShus), op_gruppe = fiksNULL(input$op_gruppe),
                                ncsp = fiksNULL(input$ncsp_verdi), BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA = fiksNULL(input$ASA),
                                whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                malign = as.numeric(input$malign), erMann = as.numeric(input$erMann), elektiv = as.numeric(input$elektiv),
                                tidsenhet = fiksNULL(input$tidsenhet, 'Aar'), inkl_konf = fiksNULL(input$inkl_konf, 99))
  }, width = 700, height = 700)

    tabellReagerTid <- reactive({
      TabellData_Tid <- norgast::NorgastFigAndelTid(RegData, valgtVar=input$valgtVar, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                                    reshID = reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg), minald=as.numeric(input$alder[1]),
                                                    maxald=as.numeric(input$alder[2]), valgtShus = fiksNULL(input$valgtShus), op_gruppe = fiksNULL(input$op_gruppe),
                                                    ncsp = fiksNULL(input$ncsp_verdi), BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                                    minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA = fiksNULL(input$ASA),
                                                    whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                                    malign = as.numeric(input$malign), erMann = as.numeric(input$erMann), elektiv = as.numeric(input$elektiv),
                                                    tidsenhet = fiksNULL(input$tidsenhet, 'Aar'), inkl_konf = fiksNULL(input$inkl_konf, 99))
    })

    output$utvalg_tid <- renderUI({
      TabellData <- tabellReagerTid()
      tagList(
        h3(HTML(paste0(TabellData$tittel, '<br />'))),
        h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
      )})



    output$Tabell_tid <- function() {

      utdata <- tabellReagerTid()
      if (input$enhetsUtvalg == 1) {
        Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt, Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                             N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                             Konf.int.ovre = utdata$KonfInt$Konf[2,], Antall2 = round(utdata$Andeler$AndelRest*utdata$NTid$NTidRest/100),
                             N2 = utdata$NTid$NTidRest, Andel2 = utdata$Andeler$AndelRest, Konf.int.nedre2 = utdata$KonfInt$KonfRest[1,],
                             Konf.int.ovre2 = utdata$KonfInt$KonfRest[2,])
        names(Tabell_tid) <- c('Tidsperiode', 'Antall', 'N', 'Andel', 'Konf.int.nedre', 'Konf.int.ovre', 'Antall', 'N', 'Andel',
                               'Konf.int.nedre', 'Konf.int.ovre')
        Tabell_tid %>% knitr::kable("html", digits = c(0,0,0,1,1,1,0,0,1,1,1)) %>%
          kable_styling("hover", full_width = F) %>%
          add_header_above(c(" ", "Din avdeling" = 5, "Landet forøvrig" = 5))
      } else {
        Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt,
                             Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                             N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                             Konf.int.ovre = utdata$KonfInt$Konf[2,])
        Tabell_tid %>%
          knitr::kable("html", digits = c(0,0,0,1,1,1)) %>%
          kable_styling("hover", full_width = F)
      }
    }

    output$lastNed_tid <- downloadHandler(
      filename = function(){
        paste0(input$valgtVar, '_tid', Sys.time(), '.csv')
      },
      content = function(file){
        utdata <- tabellReagerTid()
        if (input$enhetsUtvalg == 1) {
          Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt, Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                               N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                               Konf.int.ovre = utdata$KonfInt$Konf[2,], Antall2 = round(utdata$Andeler$AndelRest*utdata$NTid$NTidRest/100),
                               N2 = utdata$NTid$NTidRest, Andel2 = utdata$Andeler$AndelRest, Konf.int.nedre2 = utdata$KonfInt$KonfRest[1,],
                               Konf.int.ovre2 = utdata$KonfInt$KonfRest[2,])
        } else {
          Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt,
                               Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                               N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                               Konf.int.ovre = utdata$KonfInt$Konf[2,])
        }
        write.csv2(Tabell_tid, file, row.names = F)
      }
    )

    output$lastNedBilde_tid <- downloadHandler(
      filename = function(){
        paste0(input$valgtVar, '_tid', Sys.time(), '.', input$bildeformat)
      },
      content = function(file){
        norgast::NorgastFigAndelTid(RegData, valgtVar=input$valgtVar, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                    reshID = reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg), minald=as.numeric(input$alder[1]),
                                    maxald=as.numeric(input$alder[2]), valgtShus = fiksNULL(input$valgtShus), op_gruppe = fiksNULL(input$op_gruppe),
                                    ncsp = fiksNULL(input$ncsp_verdi), BMI = fiksNULL(input$BMI), tilgang = fiksNULL(input$tilgang),
                                    minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]), ASA = fiksNULL(input$ASA),
                                    whoEcog = fiksNULL(input$whoEcog), forbehandling = fiksNULL(input$forbehandling),
                                    malign = as.numeric(input$malign), erMann = as.numeric(input$erMann), elektiv = as.numeric(input$elektiv),
                                    tidsenhet = fiksNULL(input$tidsenhet, 'Aar'), inkl_konf = fiksNULL(input$inkl_konf, 99), outfile = file)
      }
    )


}





