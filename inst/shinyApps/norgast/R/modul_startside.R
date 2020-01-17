startside_UI <- function(id){
  ns <- NS(id)
  shiny::bootstrapPage(
    div(class = "container",
        div(class = "panel panel-default",
            div(class = "panel-heading" , style = "background-color : #E0E0E0 ",
                h2('Velkommen til Rapporteket - NoRGast', align='center')),
            div(class = "panel-body",style = "background-color:#F0F0F0",
                div(class="panel-text",
                    br(),
                    h4('Du er nå inne på Rapporteket for NoRGast, registerets resultattjeneste.
                Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret.
                       På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert
                       på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database.
                       Dette medfører at nyere data ikke er kvalitetssikret ennå.'),
                    h4('Du kan se på resultater for eget sykehus, nasjonale data og eget sykehus sett opp mot landet for øvrig.
                       Hvis ikke annet oppgis så gjøres alle datovalg basert på operasjonsdato. Alle figurer og
                       tabeller kan lastes ned.'),
                    br(),

                    h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
                    div(class = "container", style ="margin-right:(@gutter / 10)" ,
                        h4(tags$b('Fordelinger '), 'viser fordelinger (figur/tabell) av ulike variabler.
                Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer.'),
                        h4(id = ns("SC1"), tags$b('Sykehusvisning '), 'viser resultater per sykehus.
                           Man kan velge hvilken variabel man vil se på og om man vil se gjennomsnitt, andeler eller stablede andeler.'),
                        h4(tags$b('Tidsvisning '), 'viser tidsutviklingen for valgt variabel for ditt sykehus'),
                        h4(id = ns("SC2"), tags$b('Overlevelse '), 'viser Kaplan-Meier overlevelseskurver for to distinkte utvalg.'),
                        h4(tags$b('Samledokumenter '), 'genererer ulike dokumenter som består av utvalgte figurer og tabeller.'),
                        h4(tags$b('Datadump '), 'gir mulighet til å laste ned din egen avdelings registreringer. Man kan velge hvilke
                           variabler man vil inkludere og for hvilket tidsrom og hvilke reseksjonsgrupper.'),
                        h4(tags$b('Administrative tabeller '), 'er en samling oversikter over antall registreringer.')
                    ),
                    br(),
                    # br(),
                    #h3('HER KAN MAN F.EKS. VISE ANTALL REGISTRERINGER SISTE X MND.'),
                    # br(),
                    br(),
                    div(class="container",
                        fixedRow(
                          column(width = 4, offset = 1,
                                 h4('Oversikt over registerets kvalitetsindikatorer og resultater finner du på www.kvalitetsregistre.no:', #helpText
                                    a("NoRGast", href="https://www.kvalitetsregistre.no/registers/545/resultater"),
                                    target="_blank", align='center')),
                          column(width = 4,offset = 2,
                                 h4('Mer informasjon om registeret finnes på NoRGast sin hjemmeside: ', align='center',
                                    a("www.norgast.no", href="http://www.norgast.no", target="_blank"))
                          )
                        )
                    )
                )
            )
        ))
  )
}

startside <- function(input, output,session, usrRole){
  observe(
    if (usrRole != "SC") {
      shinyjs::hide("SC1")
      shinyjs::hide("SC2")
    }
  )
}
