#' UI-modul for datakvalitetstabeller-fane i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til Datakvalitet
#'
#' @export
datakval_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::fluidPage(
    tabsetPanel(
      tabPanel(
        id = ns("id_datakval_panel"),
        h3('Pasienter med flere forløp med samme operasjonsdato'),
        downloadButton(outputId = ns('lastNed_dobbeltreg'), label='Last ned tabell'),
        DT::dataTableOutput(ns('dobbeltreg'))
      )#,
      # tabPanel(
      #   id = ns("id_datakval_panel2"),
      #   h3('En ny fane')
      # )
    )
  )
}

#' Server-modul for datakvalitetstabeller-fane i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modulfunksjoner til Datakvalitet
#'
#' @export
datakval_server <- function(input, output, session, reshID,
                            userRole, RegData, skjemaoversikt, hvd_session) {


  output$dobbeltreg <-
    DT::renderDataTable(
      norgast::dobbelreg(RegData=RegData,
                         skjemaoversikt=skjemaoversikt,
                         usrRole = userRole,
                         reshID = reshID),
      options = list(pageLength = 40), rownames = FALSE)

  output$lastNed_dobbeltreg <- downloadHandler(
    filename = function(){
      paste0('dobbeltreg_norgast_', Sys.time(),'.csv')
    },
    content = function(file, filename){
      write.csv2(norgast::dobbelreg(RegData, skjemaoversikt=skjemaoversikt,
                                    usrRole = userRole, reshID = reshID),
                 file, row.names = F, na = '', fileEncoding = "Latin1")
    })

  shiny::observe({
    if (rapbase::isRapContext()) {
      shinyjs::onclick(
        "lastNed_dobbeltreg",
        rapbase::repLogger(
          session = hvd_session,
          msg = "NORGAST: nedlasting tabell over potensielle duplikater"
        )
      )
    }
  })



}
