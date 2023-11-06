#' UI-del av startside for NORGAST sin Rapporteket-app
#'
#' @export
startside_UI <- function(id){
  ns <- NS(id)
  shiny::bootstrapPage(
    div(class = "container",
        div(class = "panel panel-default",
            div(class = "panel-heading" , style = "background-color : #E0E0E0 ",
                h2('Velkommen til Rapporteket - NORGAST', align='center')),
            div(class = "panel-body",style = "background-color:#F0F0F0",
                div(class="panel-text",
                    br(),
                    h4('Du er nå inne på Rapporteket for NORGAST, registerets resultattjeneste.
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
                        shinyjs::hidden(h4(id = ns("SC1"), tags$b('Sykehusvisning '), 'viser resultater per sykehus.
                           Man kan velge hvilken variabel man vil se på og om man vil se gjennomsnitt, andeler eller stablede andeler.')),
                        shinyjs::hidden(h4(id = ns("SC5"), tags$b('Traktplott '), 'viser andeler per sykehus, både som traktplott og
                                           søylediagram.')),
                        h4(tags$b('Tidsvisning '), 'viser tidsutviklingen for valgt variabel. Du kan også velge å sammenligne
                        tidsutviklingen av to forskjellige utvalg.'),
                        shinyjs::hidden(h4(id = ns("SC2"), tags$b('Indikatorer '), 'viser registerets kvalitetsindikatorer.')),
                        h4(tags$b('Overlevelse '), 'viser Kaplan-Meier overlevelseskurver for to distinkte utvalg.'),
                        h4(tags$b('Samledokumenter '), 'genererer ulike dokumenter med tekst, figurer og tabeller.'),
                        h4(tags$b('Datadump '), 'gir mulighet til å laste ned din egen avdelings registreringer. Man kan velge om man
                           vil hente rådata eller koblet og prosessert data.'),
                        h4(tags$b('Administrative tabeller '), 'er en samling oversikter over antall registreringer.'),
                        h4(id = ns("SC6"), tags$b('Datakvalitet '), 'lister opp forløp der samme pasient
                                           har mer enn én operasjon med samme operasjonsdato. SC-bruker ser alle avdelinger, øvrige kun sin egen.'),
                        h4(tags$b('Abonnement '), 'lar brukeren bestille regelmessig utsendelse av rapporter til sin
                        registrerte e-post.'),
                        shinyjs::hidden(h4(id = ns("SC3"), tags$b('Utsending '), 'lar SC-bruker bestille rapporter på vegne
                                           av andre avdelinger og til valgte e-postadresser.')),
                        shinyjs::hidden(h4(id = ns("SC4"), tags$b('Eksport '), 'lar et celebert utvalg brukere laste ned
                        NORGAST-databasen kryptert med sin offentlige ssh-nøkkel. Kan kun leses vha. av vedkommendes private ssh-nøkkel.'))
                    ),
                    br(),
                    br(),
                    div(class="container",
                        fixedRow(
                          column(width = 4, offset = 1,
                                 h4('Oversikt over registerets kvalitetsindikatorer og resultater finner du på www.kvalitetsregistre.no:', #helpText
                                    a("NORGAST", href="https://www.kvalitetsregistre.no/registers/545/resultater"),
                                    target="_blank", align='center')),
                          column(width = 4,offset = 2,
                                 h4('Mer informasjon om registeret finnes på NORGAST sin hjemmeside: ', align='center',
                                    a("www.norgast.no", href="http://www.norgast.no", target="_blank"))
                          )
                        )
                    )
                )
            )
        ))
  )
}

#' Server-del av startside for NORGAST sin Rapporteket-app
#'
#' @export
startside <- function(input, output,session, usrRole){
  observe(
    if (usrRole == "SC") {
      shinyjs::show("SC1")
      shinyjs::show("SC2")
      shinyjs::show("SC3")
      shinyjs::show("SC4")
      shinyjs::show("SC5")
      # shinyjs::show("SC6")
    }
  )
}
