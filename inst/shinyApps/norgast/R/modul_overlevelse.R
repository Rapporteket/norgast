# Modul for fordelingsfigurer i NoRGast sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @return Modul fordelingsfigur
#
overlevelse_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  fluidRow(
    column(2,
           style = "background-color:#ecf0f1",
           id = ns("id_overlevelse_panel"),
           h4(tags$b('Utvalg 1')),
           br(),
           dateRangeInput(inputId=ns("datovalg"), label = "Operasjonsdato fra og til", min = '2014-01-01',
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
           uiOutput(outputId = ns('icd')),
           tags$hr(),
           actionButton(ns("reset_input"), "Nullstill valg")
    ),
    column(8,
           h2("Kaplan-Meier overlevelseskurver", align='center'),
           h4("Her kan du plotte overlevelseskurver for to distinkte utvalg. Hvis en pasient har flere forløp i et utvalg benyttes forløpet
              for den eldste operasjonen. Dersom det er overlapp mellom 'Utvalg 1' og 'Utvalg 2' så velges det eldste forløpet. Hvis det eldste
              forløpet finnes i både 'Utvalg 1' og 'Utvalg 2' eller hvis 'Utvalg 1' og 'Utvalg 2 'sitt eldste forløp faller på samme dato, så knyttes
              pasienten til 'Utvalg 1'. Dette innebærer at man potensielt kan få litt forskjellige resultater hvis du f.eks. ser på 'Åpen' i 'Utvalg 1'
              mot 'Laparoskopisk' i 'Utvalg 2' kontra 'Laparoskopisk' i 'Utvalg 1' mot 'Åpen' i 'Utvalg 2'.Hvis dette ikke er ønskelig så kan du krysse
              av for 'Fjern pasienter med forløp som tilfredsstiller begge utvalg'"),
           br(),
           checkboxInput(ns("inkl_konf"), label = 'Inkluder konfidensintervall'),
           br(),
           fluidRow(
             column(7,
                    plotOutput(ns("Figur_surv"))),
             column(4, offset = 1,
                    uiOutput(ns("utvalg")),
                    br(),
                    checkboxInput(ns("ekskluder_felles"), label = 'Fjern pasienter med forløp som tilfredsstiller begge utvalg'),
                    br(),
                    br(),
                    selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                                choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                    textInput(ns("tittel"), "Angi tittel for lagret plot", ""),
                    downloadButton(ns("lastNedBilde"), "Last ned figur"))
           )
    ),
    column(2,
           style = "background-color:#ecf0f1",
           id = ns("id_overlevelse_panel2"),
           h4(tags$b('Utvalg 2')),
           br(),
           dateRangeInput(inputId=ns("datovalg2"), label = "Operasjonsdato fra og til", min = '2014-01-01',
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
           uiOutput(outputId = ns('icd2')),
           tags$hr(),
           actionButton(ns("reset_input2"), "Nullstill valg"))
  )

}




overlevelse <- function(input, output, session, reshID, RegData, userRole, hvd_session){

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

  output$icd <- renderUI({
    ns <- session$ns
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
    Utvalg1 <- Utvalg1$RegData
    diagnoser <- names(sort(table(Utvalg1$Hoveddiagnose2), decreasing = T))
    if (!is.null(diagnoser)) {
      selectInput(inputId = ns("icd_verdi"), label = "Spesifiser ICD-10 koder (velg en eller flere)",
                  choices = diagnoser, multiple = TRUE)
    }
  })

  output$icd2 <- renderUI({
    ns <- session$ns
    Utvalg1 <- NorgastUtvalg(RegData = RegData, datoFra = input$datovalg2[1], datoTil = input$datovalg2[2],
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
    Utvalg1 <- Utvalg1$RegData
    diagnoser <- names(sort(table(Utvalg1$Hoveddiagnose2), decreasing = T))
    if (!is.null(diagnoser)) {
      selectInput(inputId = ns("icd_verdi2"), label = "Spesifiser ICD-10 koder (velg en eller flere)",
                  choices = diagnoser, multiple = TRUE)
    }
  })

  calc_overlevelse <- function() {
    RegData <- RegData[-which(RegData$OpDoedTid<0), ] #feilregistreringer: død før operasjon
    RegData <- RegData[!(RegData$Avdod == 1 & is.na(RegData$OpDoedTid)), ] #manglende data: død men ingen dødsdato
    RegData$OpDoedTid[RegData$OpDoedTid==0] <- 0.5 #Døde ved dag 0 settes til 0.5 for å inkluderes i analysen

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
                             malign = as.numeric(input$malign),
                             icd = if (!is.null(input$icd_verdi)) {input$icd_verdi} else {''})
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
                             malign = as.numeric(input$malign2),
                             icd = if (!is.null(input$icd_verdi2)) {input$icd_verdi2} else {''})
    Utvalg2data <- Utvalg2$RegData
    if (!is.null(input$valgtShus2)) {
      Utvalg2data <- Utvalg2data[which(Utvalg2data$AvdRESH %in% as.numeric(input$valgtShus2)), ]
    }
    Utvalg2data$Utvalg <- 2
    Utvalg2data <- Utvalg2data[order(Utvalg2data$HovedDato, decreasing = F), ]                   # Hvis pasient opptrer flere ganger, velg
    Utvalg2data <- Utvalg2data[match(unique(Utvalg2data$PasientID), Utvalg2data$PasientID), ]   # første operasjon i utvalget

    utdata <- list(Utvalg1 = Utvalg1data, Utvalg2 = Utvalg2data, utvalgTxt1 = Utvalg1$utvalgTxt, utvalgTxt2 = Utvalg2$utvalgTxt)

    Samlet <- bind_rows(Utvalg1data, Utvalg2data)
    Samlet <- Samlet[order(Samlet$HovedDato, decreasing = F), ]                   # Hvis pasient opptrer flere ganger, velg
    Samlet <- Samlet[match(unique(Samlet$PasientID), Samlet$PasientID), ]         # første operasjon i utvalget

    if (input$ekskluder_felles) {
      Samlet <- Samlet[!(Samlet$PasientID %in% intersect(Utvalg1data$PasientID, Utvalg2data$PasientID)), ]
    }

    Samlet$overlev <- difftime(as.Date(Sys.Date()), Samlet$OperasjonsDato, units = 'days')
    Samlet$overlev[Samlet$Avdod==1] <- Samlet$OpDoedTid[Samlet$Avdod==1]
    Samlet$overlev <- as.numeric(Samlet$overlev)
    Samlet$SurvObj <- with(Samlet, Surv(overlev, Avdod == 1))

    fit1 <- survival::survfit(SurvObj ~ Utvalg, data = Samlet)

    utdata$Samlet <- Samlet
    utdata$fit1 <- fit1

    return(utdata)
  }


  output$Figur_surv <- renderPlot({
    overlevdata <- calc_overlevelse()
    survminer::ggsurvplot(overlevdata$fit1, data = overlevdata$Samlet, pval = TRUE, conf.int = input$inkl_konf, fun = "pct",
                          risk.table = TRUE, legend = "bottom")
  }, width = 800, height = 800) #


  output$utvalg <- renderUI({
    utvlgdata <- calc_overlevelse()
    tagList(
      h4('Utvalg 1:'),
      h5(HTML(paste0(utvlgdata$utvalgTxt1, '<br />'))),
      br(),
      br(),
      h4('Utvalg 2:'),
      h5(HTML(paste0(utvlgdata$utvalgTxt2, '<br />'))),
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

  output$lastNedBilde <- downloadHandler(
    filename = function(){
      paste0('KM_kurve', Sys.time(), '.', input$bildeformat)
    },

    content = function(file){
      overlevdata <- calc_overlevelse()
      survp <- survminer::ggsurvplot(overlevdata$fit1, data = overlevdata$Samlet, title = input$tittel, pval = TRUE,
                                     conf.int = input$inkl_konf, fun = "pct", risk.table = TRUE, legend = "bottom")
      rapFigurer::figtype(outfile=file, pointsizePDF=11)
      print(survp, newpage = FALSE)
      dev.off()
    }
  )

  shiny::observe({
    if (rapbase::isRapContext()) {
      raplog::repLogger(
        session = hvd_session,
        msg = "NoRGast: KM-overlevelseskurve."
      )

      shinyjs::onclick(
        "lastNedBilde",
        raplog::repLogger(
          session = hvd_session,
          msg = "NoRGast: Laster ned KM-overlevelseskurve."
        )
      )
    }
  })






}
