#' Client (ui) for the norgast app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  shiny::addResourcePath('rap', system.file('www', package='rapbase'))
  regTitle = "NORGAST"

  # Define UI for application
  ui <- shiny::navbarPage(
    shinyjs::useShinyjs(),
    id = "norgast_app_id",
    title = shiny::div(shiny::a(
      shiny::includeHTML(
        system.file('www/logo.svg', package='rapbase')
      )
    ),
    regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    shiny::tabPanel(
      "Startside",
      rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),

      norgast::startside_ui("startside")
    ),

    # shiny::tabPanel(
    #   "Fordelinger",
    #   norgast::fordelingsfig_ui(id = "fordelingsfig_id")
    # ),
    #
    # shiny::tabPanel(
    #   "Sykehusvisning",
    #   norgast::sykehusvisning_ui(id = "sykehusvisning_id")
    # ),
    #
    # shiny::tabPanel(
    #   "Traktplott",
    #   norgast::traktplot_ui(id = "traktplot_id")
    # ),
    #
    # shiny::navbarMenu(
    #   "Tidsvisning",
    #   shiny::tabPanel("Andeler over tid",
    #                   norgast::tidsvisning_ui(id = "tidsvisning_id")
    #   ),
    #   shiny::tabPanel("Sammenlign andeler",
    #                   norgast::saml_andeler_ui(id = "saml_andeler_id")
    #   )
    # ),
    #
    # shiny::tabPanel("Indikatorer",
    #                 norgast::indikatorfig_ui(id = "indikator_id")
    # ),
    #
    # shiny::tabPanel("Overlevelse",
    #                 norgast::overlevelse_ui(id = "overlevelse_id")
    # ),
    #
    # shiny::tabPanel("Samledokumenter",
    #                 shiny::h2("Samledokumenter", align='center'),
    #                 shiny::h4("Når du velger ", strong("Last ned samledokument"),
    #                           " genereres en samlerapport bestående av figurer og
    #                         tabeller.", align='center'),
    #                 shiny::br(),
    #                 shiny::br(),
    #                 norgast::samledok_ui(id = "samledok_id")
    # ),
    #
    # shiny::tabPanel(
    #   "Datadump",
    #   shiny::h2("Datadump", align='center'),
    #   shiny::h4("Data på Rapporteket oppdateres én gang i døgnet. Følgelig kan
    # det være små avvik i antall forløp som inkluderes i datadump på Rapporteket
    #           sammenlignet med datadump hentet fra registerets qreg-løsning.",
    #             align='center'),
    #   shiny::br(),
    #   shiny::br(),
    #   norgast::datadump_ui(id = "datadump_id")
    # ),
    #
    # shiny::tabPanel("Administrative tabeller",
    #                 norgast::admtab_ui(id = "admtab_id")
    # ),
    #
    # shiny::tabPanel("Datakvalitet",
    #                 norgast::datakval_ui("datakval_id")
    # ),
    #
    # shiny::tabPanel(
    #   shiny::span("Abonnement",
    #               title="Bestill tilsending av rapporter på e-post"),
    #   shiny::sidebarLayout(
    #     shiny::sidebarPanel(
    #       rapbase::autoReportInput("norgastSubscription")
    #     ),
    #     shiny::mainPanel(
    #       rapbase::autoReportUI("norgastSubscription")
    #     )
    #   )
    # ),

    shiny::navbarMenu(
      "Verktøy",
      # shiny::tabPanel(
      #   "Utsending",
      #   shiny::sidebarLayout(
      #     shiny::sidebarPanel(
      #       rapbase::autoReportOrgInput("norgastDispatch"),
      #       rapbase::autoReportInput("norgastDispatch")#,
      #       # shiny::actionButton(inputId = "run_autoreport",
      #       #                     label = "Kjør autorapporter"),
      #       # shiny::dateInput(inputId = "rapportdato",
      #       #                  label = "Kjør rapporter med dato:",
      #       #                  value = Sys.Date(),
      #       #                  min = Sys.Date(),
      #       #                  max = Sys.Date() + 366
      #       # ),
      #       # shiny::checkboxInput(inputId = "dryRun", label = "Send e-post")
      #     ),
      #     shiny::mainPanel(
      #       rapbase::autoReportUI("norgastDispatch")#,
      #       # p(em("System message:")),
      #       # verbatimTextOutput("sysMessage"),
      #       # p(em("Function message:")),
      #       # verbatimTextOutput("funMessage"),
      #       # shiny::h4("Conf-parametre:"),
      #       # shiny::textOutput("confgreier1"),
      #       # shiny::textOutput("confgreier2"),
      #       # shiny::textOutput("confgreier3"),
      #       # shiny::h4("Autorapporttabell:"),
      #       # shiny::tableOutput("autoraptab")
      #     )
      #   )
      # ),
      shiny::tabPanel(
        "Metadata",
        shiny::sidebarLayout(
          shiny::sidebarPanel(uiOutput("metaControl")),
          shiny::mainPanel(htmlOutput("metaData"))
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


# shiny::tabPanel(
#   "debugging",
#   shiny::h4("Test 'rapbase' functions using the session object:"),
#   shiny::textOutput("user"),
#   shiny::textOutput("group"),
#   shiny::textOutput("resh_id"),
#   shiny::textOutput("role"),
#   shiny::textOutput("full_name"),
#   shiny::h4("Environment var R_RAP_INSTANCE:"),
#   shiny::textOutput("instance"),
#   shiny::h4("Environment var R_RAP_CONFIG_PATH:"),
#   shiny::textOutput("config_path"),
#   shiny::h4("Environment var(s) provided by SHINYPROXY (if any):"),
#   shiny::textOutput("sp_usergroups"),
#   shiny::h4("Locale settings:"),
#   shiny::textOutput("locale"),
#   shiny::h4("Database og tabeller:"),
#   shiny::textOutput("database"),
#   shiny::tableOutput("tabeller"),
#   shiny::h4("skjemaoversikt:"),
#   shiny::tableOutput("skjemaoversikt"),
#   shiny::h4("allevarnum:"),
#   shiny::tableOutput("allevarnum"),
#   shiny::h4("forlopsoversikt:"),
#   shiny::tableOutput("forlopsoversikt"),
#   shiny::h4("user:"),
#   shiny::tableOutput("user_tab")
# ),
