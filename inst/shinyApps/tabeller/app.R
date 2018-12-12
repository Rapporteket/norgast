library(shiny)
library(DT)

ui <- shinyUI(fluidPage(
  h1('Testing TableTools'),
  mainPanel(
    dataTableOutput('display')
  )
))

Names <- c("",names(mtcars))
FooterNames <- c(rep("",4),Names[5:6],rep("",6))

server <- function(input, output, session) {

  sketch <- htmltools::withTags(table(
    tableHeader(Names),tableFooter(FooterNames)
  ))

  opts <- list(
    dom = 'Bfrtip', buttons = list('colvis','print',list(extend='collection',text='Download',buttons = list('copy','csv','excel','pdf'))),
    footerCallback = JS(
      "function( tfoot, data, start, end, display ) {",
      "var api = this.api(), data;",
      "$( api.column(5).footer()).html('SubTotal:  '+",
      "api.column(5).data().reduce( function ( a, b ) {",
      "return a + b;",
      "} )",
      ");",
      "$( api.column(4).footer()).html('SubTotal: '+",
      "api.column(4).data().reduce( function ( a, b ) {",
      "return a + b;",
      "} )",
      ");","}")
  )

  output$display <- DT::renderDataTable(container = sketch,extensions = 'Buttons',options = opts,{
    mtcars
  })
}

shinyApp(ui = ui, server = server)
