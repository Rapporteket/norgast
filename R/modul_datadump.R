#' UI-modul for datadump-fane i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til datadump-fane
#'
#' @export
datadump_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      width = 3,
      id = ns("id_dump_panel"),
      uiOutput(outputId = ns('valgtevar_dump')),
      dateRangeInput(
        inputId=ns("datovalg"), label = "Dato fra og til",
        min = '2014-01-01', language = "nb",
        max = Sys.Date(),
        start  = lubridate::floor_date(lubridate::today() -
                                         lubridate::years(1),
                                       unit = "year"),
        end = Sys.Date(), separator = " til "),
      uiOutput(outputId = ns('op_gruppe_ui')),
      uiOutput(outputId = ns('ncsp')),
      uiOutput(outputId = ns('valgtShus_ui')),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(
      id= ns("datadump"),
      tabPanel(
        "Prosessert data", value = "datadump_pros",
        h2('Datadump prosessert - NORGAST', align='center'),
        br(),
        h4('Her kan du laste ned datadump basert på prosessert og koblet data
          som brukes på Rapporteket. Du kan velge hvilke variabler du vil
             inkludere, samt filtrere på operasjonstype i tillegg til dato.
             Kun ferdigstilte registreringer er inkludert.'),
        downloadButton(ns("lastNed_dump"), "Last ned datadump")
      )
      # )
    )
  )
}

#' Server-modul for datadump-fane i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til datadump-fane
#'
#' @export
datadump_server <- function(id, RegData, user, BrValg){
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$reset_input, {
        shinyjs::reset("id_dump_panel")
      })


      output$op_gruppe_ui <- renderUI({
        ns <- session$ns
        selectInput(
          inputId = ns("op_gruppe"),
          label = "Velg reseksjonsgruppe(r)",
          choices = BrValg$reseksjonsgrupper,
          multiple = TRUE)
      })

      output$valgtShus_ui <- renderUI({
        ns <- session$ns
        if (user$role() == 'SC') {
          selectInput(
            inputId = ns("valgtShus"),
            label = "Velg sykehus",
            choices = BrValg$sykehus,
            multiple = TRUE)
        }
      })

      output$ncsp <- renderUI({
        ns <- session$ns
        if (!is.null(input$op_gruppe)) {
          selectInput(
            inputId = ns("ncsp_verdi"),
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

      output$valgtevar_dump <- renderUI({
        ns <- session$ns
        if (!is.null(names(RegData))) {
          selectInput(
            inputId = ns("valgtevar_dump_verdi"),
            label = "Velg variabler å inkludere (ingen valgt er lik alle)",
            choices = names(RegData), multiple = TRUE)
        }
      })



      output$lastNed_dump <- downloadHandler(
        filename = function(){
          paste0('Datadump_NORGAST', Sys.time(), '.csv')
        },
        content = function(file){
          dumpdata <- RegData[RegData$HovedDato >= input$datovalg[1] &
                                RegData$HovedDato <= input$datovalg[2], ]
          if (user$role() != 'SC') {
            dumpdata <- dumpdata[dumpdata$AvdRESH == user$org(), ]
          } else {
            if (!is.null(input$valgtShus)) {
              dumpdata <- dumpdata[dumpdata$AvdRESH %in%
                                     as.numeric(input$valgtShus), ]}
          }

          if (!is.null(input$op_gruppe)) {
            dumpdata <- dumpdata[which(dumpdata$Op_gr %in%
                                         as.numeric(input$op_gruppe)), ]}
          if (!is.null(input$ncsp_verdi)) {
            dumpdata <- dumpdata[which(substr(dumpdata$Hovedoperasjon, 1, 5) %in% input$ncsp_verdi), ]}
          if (!is.null(input$valgtevar_dump_verdi)) {
            dumpdata <- dumpdata[, input$valgtevar_dump_verdi]}
          rapbase::repLogger2(
            user = user,
            msg = paste0("NORGAST: nedlasting prosessert datadump f.o.m. ",
                         input$datovalg[1], " t.o.m. ", input$datovalg[2])
          )

          write.csv3(dumpdata, file, row.names = F, na = '')
        }
      )

    }
  )
}
