# Modul for datakvalitetstabeller-fane i NoRGast sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# inheritParams norgastFigAndeler
#
# return Modulfunksjoner til Datakvalitet


datakval_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      id = ns("id_datakval_panel"),
      h3('Pasienter som har to eller flere forløp med samme operasjonsdato'),
      downloadButton(outputId = ns('lastNed_dobbeltreg'), label='Last ned tabell'),
      DT::dataTableOutput(ns('dobbeltreg'))
    )
  )
}

datakval_server <- function(input, output, session, reshID, userRole, RegData, hvd_session) {


  output$dobbeltreg <-
    DT::renderDataTable(
      norgast::dobbelreg(RegData,
                         usrRole = userRole,
                         reshID = reshID),
      options = list(pageLength = 40), rownames = FALSE)

  output$lastNed_dobbeltreg <- downloadHandler(
    filename = function(){
      paste0('dobbeltreg_norgast_', Sys.time(),'.csv')
    },
    content = function(file, filename){
      write.csv2(norgast::dobbelreg(RegData, usrRole = userRole, reshID = reshID),
                 file, row.names = F, na = '', fileEncoding = "Latin1")
    })

  shiny::observe({
    if (rapbase::isRapContext()) {
      shinyjs::onclick(
        "lastNed_dobbeltreg",
        rapbase::repLogger(
          session = hvd_session,
          msg = "NoRGast: nedlasting tabell over potensielle duplikater"
        )
      )
    }
  })



}














# dobbelreg <- function() { # for å forhindre feilmelding ved autoload fra R-folder
#   flere_sammedato_v2 <- RegData %>% group_by(PasientID, HovedDato) %>% summarise(Op_pr_dag = n())
#   flere_sammedato_v2 <- flere_sammedato_v2[flere_sammedato_v2$Op_pr_dag > 1, ]
#
#   flere_sammedato_v3 <- merge(flere_sammedato_v2, RegData, by = c('PasientID', 'HovedDato'), all.x = T)
#   flere_sammedato_v3 <- flere_sammedato_v3[order(flere_sammedato_v3$PasientID), ]
#   flere_sammedato_v3 <- flere_sammedato_v3[ , c("PasientID", "ForlopsID", "OperasjonsDato", "AvdRESH", "Sykehusnavn","Hovedoperasjon", "Operasjonsgrupper", "Hoveddiagnose")]
# }
#

