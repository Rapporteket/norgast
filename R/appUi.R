#' Client (ui) for the norgast app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  shiny::addResourcePath('rap', system.file('www', package='rapbase'))
  regTitle = "NORGAST"

  # Define UI for application
  ui <- shiny::navbarPage(
    id = "norgast_app_id",

    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    shiny::tabPanel(
      "Startside",
      # shinyjs::useShinyjs(),
      # rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
      #                              organization = uiOutput("appOrgName"),
      #                              addUserInfo = TRUE),
      rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),

      norgast::startside_UI("startside")
    ),

    shiny::tabPanel(
      "Fordelinger",
      norgast::fordelingsfig_UI(id = "fordelingsfig_id")
    ),

    shiny::tabPanel(
      "Sykehusvisning",
      norgast::sykehusvisning_UI(id = "sykehusvisning_id")
    ),

    shiny::tabPanel(
      "Traktplott",
      norgast::traktplot_UI(id = "traktplot_id")
    ),

    shiny::navbarMenu(
      "Tidsvisning",
      shiny::tabPanel("Andeler over tid",
                      norgast::tidsvisning_UI(id = "tidsvisning_id")
      ),
      shiny::tabPanel("Sammenlign andeler",
                      norgast::saml_andeler_UI(id = "saml_andeler_id")
      )
    ),

    shiny::tabPanel("Indikatorer",
                    norgast::indikatorfig_UI(id = "indikator_id")
    ),

    shiny::tabPanel("Overlevelse",
                    norgast::overlevelse_UI(id = "overlevelse_id")
    ),

    shiny::tabPanel("Samledokumenter",
                    shiny::h2("Samledokumenter", align='center'),
                    shiny::h4("Når du velger ", strong("Last ned samledokument"),
                              " genereres en samlerapport bestående av figurer og
                            tabeller.", align='center'),
                    shiny::br(),
                    shiny::br(),
                    norgast::samledok_UI(id = "samledok_id")
    ),

    shiny::tabPanel(
      "Datadump",
      shiny::h2("Datadump", align='center'),
      shiny::h4("Data på Rapporteket oppdateres én gang i døgnet. Følgelig kan
    det være små avvik i antall forløp som inkluderes i datadump på Rapporteket
              sammenlignet med datadump hentet fra registerets qreg-løsning.",
                align='center'),
      shiny::br(),
      shiny::br(),
      norgast::datadump_UI(id = "datadump_id")
    ),

    shiny::tabPanel("Administrative tabeller",
                    norgast::admtab_UI(id = "admtab_id")
    ),

    shiny::tabPanel("Datakvalitet",
                    norgast::datakval_ui("datakval_id")
    ),

    shiny::tabPanel(
      shiny::span("Abonnement",
                  title="Bestill tilsending av rapporter på e-post"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          rapbase::autoReportInput("norgastSubscription")
        ),
        shiny::mainPanel(
          rapbase::autoReportUI("norgastSubscription")
        )
      )
    ),

    shiny::navbarMenu(
      "Verktøy",
      shiny::tabPanel(
        "Utsending",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportOrgInput("norgastDispatch"),
            rapbase::autoReportInput("norgastDispatch")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("norgastDispatch")
          )
        )
      ),

      shiny::tabPanel(
        "Eksport",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::exportUCInput("norgastExport")
          ),
          shiny::mainPanel(
            rapbase::exportGuideUI("norgastExportGuide")
          )
        )
      ),

      shiny::tabPanel(
        "Bruksstatistikk",
        shiny::sidebarLayout(
          shiny::sidebarPanel(rapbase::statsInput("norgastStats")),
          shiny::mainPanel(
            rapbase::statsUI("norgastStats"),
            rapbase::statsGuideUI("norgastStatsGuide")
          )
        )
      )
    )
  )

}
