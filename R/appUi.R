#' Client (ui) for the norgast app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  regTitle = "NORGAST"

  # Define UI for application
  ui <- shiny::navbarPage(
    shinyjs::useShinyjs(),
    id = "norgast_app_id",
    title = rapbase::title(regTitle),
    windowTitle = regTitle,
    theme = rapbase::theme(),
    header = shiny::modalDialog(
      shiny::h3("ADVARSEL"),
      "Som bruker av Rapporteket i NORGAST får du tilgang til aggregerte tall
      for hele landet i tillegg til din egen avdeling. Dette gir et verdifullt
      sammenligningsgrunnlag for den enkelte avdeling.",
      shiny::hr(),
      "Ved å klikke deg videre i Rapporteket nå godkjenner du det nedenfor
      stående som en bindende avtale:",
      shiny::hr(),
      "Det er IKKE tillatt å dele aggregerte nasjonale tall i fora som er
       tilgjengelig for andre enn kolleger og ledelse i den institusjon du
      representerer som bruker av Rapporteket. Offentliggjøring av aggregerte
      nasjonale tall skal kun gjøres med eksplisitt tillatelse fra NORGAST
      etter formell søknad til registeret. Som offentliggjøring regnes enhver
      gjengivelse av ethvert nasjonalt resultat eller av data for annen
      institusjon enn den du representerer, i aviser, foredrag, sosiale medier,
      tidsskrifter, konferansepostere, offentlig tilgjengelige rapporter og
      rundskriv, samt i intervjuer med media og andre fora som er tilgjengelig
      for andre enn kolleger og ledelse i din institusjon.",
      size = "l",
      label = "Godkjenn",
      easyClose = FALSE,
      footer = shiny::modalButton("Godta og fortsett")
    ),
    shiny::tabPanel(
      "Startside",
      rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),

      norgast::startside_ui("startside")
    ),

    shiny::tabPanel(
      "Fordelinger",
      norgast::fordelingsfig_ui(id = "fordelingsfig_id")
    ),

    shiny::navbarMenu(
      "Tidsvisning",
      shiny::tabPanel("Andeler over tid",
                      norgast::tidsvisning_ui(id = "tidsvisning_id")
      ),
      shiny::tabPanel("Sammenlign andeler",
                      norgast::saml_andeler_ui(id = "saml_andeler_id")
      )
    ),

    shiny::tabPanel("Overlevelse",
                    norgast::overlevelse_ui(id = "overlevelse_id")
    ),

    shiny::tabPanel("Samledokumenter",
                    shiny::h2("Samledokumenter", align='center'),
                    shiny::h4("Når du velger ", strong("Last ned samledokument"),
                              " genereres en samlerapport bestående av figurer og
                            tabeller.", align='center'),
                    shiny::br(),
                    shiny::br(),
                    norgast::samledok_ui(id = "samledok_id")
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
      norgast::datadump_ui(id = "datadump_id")
    ),

    shiny::tabPanel("Administrative tabeller",
                    norgast::admtab_ui(id = "admtab_id")
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
