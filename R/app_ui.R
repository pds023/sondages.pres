#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  polls <- load_app_data(app_poll_source()$path())
  dates <- range(polls$terrain_fin, na.rm = TRUE)
  institutes <- sort(unique(polls$institut))

  chart_card <- function(title, subtitle = NULL, ..., class = NULL) {
    tags$section(
      class = paste("chart-card", class),
      tags$div(class = "card-heading", tags$h2(title),
               if (!is.null(subtitle)) tags$p(subtitle)),
      ...
    )
  }
  note <- function(...) tags$div(class = "context-note", ...)

  bslib::page_fluid(
    title = "Présidentielle 2027 · L’observatoire des sondages",
    lang = "fr",
    theme = bslib::bs_theme(
      version = 5, bg = "#f5f6f8", fg = "#172338", primary = "#087f8c",
      secondary = "#67758a", base_font = "Inter, Segoe UI, Arial, sans-serif",
      border_radius = "0.65rem"
    ),
    golem_add_external_resources(),
    tags$a(class = "skip-link", href = "#main-content", "Aller au contenu"),
    tags$header(
      class = "app-header",
      tags$div(
        class = "brand",
        tags$div(class = "brand-mark", `aria-hidden` = "true",
                 tags$span(), tags$span(), tags$span(), tags$span()),
        tags$div(class = "brand-wordmark", tags$span("PRÉSIDENTIELLE"),
                 tags$strong("2027", tags$span(class = "brand-separator", " / "),
                              tags$span(class = "brand-detail", "OBSERVATOIRE")))
      ),
      tags$div(class = "header-note", tags$span(class = "status-dot"),
               "Données ouvertes", tags$span(class = "header-note-divider", "·"),
               "Exploration indépendante")
    ),
    bslib::layout_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        width = 280, open = "desktop", class = "filters-sidebar",
        tags$div(class = "sidebar-intro", tags$p(class = "eyebrow", "EXPLORER LA BASE"),
                 tags$h2("Votre périmètre"),
                 tags$p("Croisez les résultats. Gardez le contexte.")),
        radioButtons("tour", "Tour de scrutin", choices = c("1er tour" = "1", "2d tour" = "2"),
                     selected = "1", inline = TRUE),
        dateRangeInput("period", "Période de terrain", start = dates[1], end = dates[2],
                       min = dates[1], max = dates[2], language = "fr", format = "dd/mm/yyyy",
                       separator = "au", weekstart = 1),
        selectizeInput("institutes", "Instituts", choices = institutes, selected = institutes,
                       multiple = TRUE, options = list(plugins = list("remove_button"),
                                                       placeholder = "Choisir les instituts")),
        selectizeInput("candidates", "Candidats", choices = NULL, multiple = TRUE,
                       options = list(plugins = list("remove_button"),
                                      placeholder = "Choisir les candidats")),
        tags$div(class = "selection-actions",
                 actionLink("all_candidates", "Tout sélectionner"),
                 actionLink("clear_candidates", "Effacer")),
        conditionalPanel("input.tour == '1'",
                         checkboxInput("main_only", "Configurations principales uniquement", FALSE)),
        tags$hr(),
        selectInput("weighting", "Pondération des synthèses",
                    choices = c("Un sondage, une voix" = "equal",
                                "Par taille d’échantillon" = "sample"), selected = "equal"),
        tags$div(class = "filter-actions",
                 actionButton("reset_filters", "Réinitialiser les filtres", class = "btn-reset"),
                 downloadButton("download_data", "Exporter la sélection", class = "btn-export")),
        tags$div(class = "sidebar-caution",
                 tags$strong("Une mesure, un contexte."),
                 tags$p("Les scores dépendent des candidatures testées. Une moyenne de scénarios différents ne constitue pas une prévision électorale."))
      ),
      tags$main(
        id = "main-content", class = "main-content",
        tags$div(class = "page-heading",
                 tags$div(tags$p(class = "eyebrow", "COMPRENDRE LES RAPPORTS DE FORCE"),
                          tags$h1("L’observatoire des sondages"),
                          tags$p(class = "page-description", "Tendances, candidatures et scénarios de la présidentielle 2027.")),
                 uiOutput("scope_label", class = "scope-label")),
        uiOutput("data_status", `aria-live` = "polite"),
        bslib::navset_tab(
          id = "view", selected = "overview",
          bslib::nav_panel(
            "Vue d’ensemble", value = "overview",
            uiOutput("kpis"),
            tags$div(class = "two-column overview-charts",
                     chart_card("Les dynamiques en un regard", "Moyennes glissantes sur 90 jours dans la période sélectionnée.",
                                plotly::plotlyOutput("overview_trend", height = "380px")),
                     chart_card("Les niveaux observés", "Moyennes sur les 90 derniers jours de la période sélectionnée.",
                                plotly::plotlyOutput("ranking_plot", height = "380px"))),
            uiOutput("overview_insight"),
            chart_card("Les dernières enquêtes", "Cliquez sur une ligne pour explorer les configurations de cette enquête.",
                       DT::DTOutput("latest_table"))
          ),
          bslib::nav_panel(
            "Trajectoires", value = "trends",
            chart_card(
              "Suivre les trajectoires", "Les points montrent les mesures publiées. Les courbes facilitent la lecture des tendances.",
              tags$div(class = "inline-controls",
                       selectInput("trend_window", "Fenêtre de lissage", selected = "90",
                                   choices = c("30 jours" = "30", "60 jours" = "60",
                                               "90 jours" = "90", "120 jours" = "120")),
                       checkboxInput("show_points", "Afficher les mesures individuelles", TRUE)),
              plotly::plotlyOutput("trend_plot", height = "480px")
            ),
            chart_card("La dispersion des résultats", "Un point par sondage et candidat, après moyenne des variantes. Distribution non pondérée.",
                       plotly::plotlyOutput("distribution_plot", height = "420px")),
            chart_card("Chaque candidat en détail", "Nombre d’enquêtes, niveaux moyens et dispersion dans la sélection.",
                       DT::DTOutput("candidate_table"))
          ),
          bslib::nav_panel(
            "Scénarios", value = "scenarios",
            chart_card(
              "Reconstituer une configuration", "Retrouvez toutes les candidatures d’un même scénario, puis comparez deux hypothèses.",
              tags$div(class = "inline-controls scenario-controls",
                       selectizeInput("scenario_id", "Scénario de référence", choices = NULL),
                       selectizeInput("comparison_id", "Comparer avec", choices = NULL,
                                      options = list(placeholder = "Aucune comparaison", allowEmptyOption = TRUE))),
              uiOutput("scenario_metadata"),
              checkboxInput("scenario_intervals", "Afficher les intervalles indicatifs à 95 %", FALSE),
              plotly::plotlyOutput("scenario_plot", height = "460px"),
              note("Cette vue conserve la liste complète des candidatures de chaque configuration, même si certains candidats sont exclus du filtre global. Les intervalles utilisent parfois l’échantillon global de l’enquête : leur précision peut alors être surestimée.")
            ),
            chart_card("À candidatures identiques", "Un historique limité à la même liste de candidats que le scénario de référence.",
                       plotly::plotlyOutput("scenario_history", height = "370px"),
                       note("La période, les instituts, le tour et le filtre des configurations principales restent appliqués. La formulation des questions et les méthodes peuvent toutefois varier.")),
            chart_card("Le détail du scénario", "Résultats publiés et informations de source.",
                       DT::DTOutput("scenario_table"))
          ),
          bslib::nav_panel(
            "Second tour", value = "duels",
            note(tags$strong("Un espace consacré aux duels. "),
                 "Cette vue utilise toujours les données du second tour, avec la période et les instituts sélectionnés. Les filtres de tour, de candidats et de configurations principales ne s’y appliquent pas."),
            chart_card("Face à face", "Suivez l’évolution d’une même affiche de second tour.",
                       selectInput("duel_pair", "Duel à explorer", choices = NULL),
                       plotly::plotlyOutput("duel_plot", height = "430px")),
            chart_card("La carte des duels", "Score du candidat en ligne face à l’adversaire en colonne. Cases vides : confrontation non testée.",
                       plotly::plotlyOutput("duel_matrix", height = "480px")),
            chart_card("Les enquêtes de second tour", "Le détail des confrontations documentées.",
                       DT::DTOutput("duel_table"))
          ),
          bslib::nav_panel(
            "Instituts", value = "institutes",
            chart_card("Qui mesure quoi ?", "Nombre d’enquêtes distinctes par institut dans la sélection.",
                       plotly::plotlyOutput("institute_volume", height = "320px")),
            chart_card("Les écarts à la moyenne sélectionnée", "Écarts descriptifs par candidat et institut, en points de pourcentage.",
                       plotly::plotlyOutput("institute_heatmap", height = "440px"),
                       note("Les écarts peuvent venir des périodes de terrain, des candidatures testées ou des méthodes. Ce graphique n’isole pas un « effet institut » et ne constitue pas un classement de fiabilité.")),
            chart_card("L’activité des instituts", "Couverture des candidats, scores moyens et nombre d’enquêtes.",
                       DT::DTOutput("institute_table"))
          ),
          bslib::nav_panel(
            "Données", value = "data",
            uiOutput("data_quality"),
            chart_card("Explorer les données", "La table ci-dessous reflète vos filtres. Recherchez un candidat, une enquête ou une configuration.",
                       tags$div(class = "table-actions",
                                downloadButton("download_summary", "Exporter la synthèse candidats", class = "btn-export"),
                                downloadButton("download_all", "Télécharger la base complète", class = "btn-outline-secondary")),
                       DT::DTOutput("data_table"))
          ),
          bslib::nav_panel(
            "Méthode", value = "methodology",
            tags$div(
              class = "methodology-intro",
              tags$p(class = "eyebrow", "LES CLÉS DE LECTURE"),
              tags$h2("Explorer sans surinterpréter."),
              tags$p("Un sondage décrit des intentions déclarées, dans un contexte et à une date donnés. Cet observatoire aide à les comparer ; il ne prédit pas le résultat de l’élection.")
            ),
            tags$div(
              class = "methodology-grid",
              chart_card("01 · L’unité de lecture", NULL,
                         tags$p("Une enquête peut tester plusieurs configurations de candidatures. Chaque ligne de la base représente le score d’un candidat dans une configuration d’une enquête, pour un tour donné."),
                         tags$p("Les graphiques temporels et le filtre de période utilisent la date de fin du terrain (terrain_fin), et non la date de publication. Un candidat absent d’un scénario n’a pas un score de zéro.")),
              chart_card("02 · Les moyennes et la pondération", NULL,
                         tags$p("Pour les synthèses, les différentes configurations d’un même candidat dans une même enquête sont d’abord moyennées. Chaque enquête contribue ensuite une fois au score de ce candidat."),
                         tags$p("L’option « Un sondage, une voix » attribue le même poids à chaque enquête. L’option par taille d’échantillon utilise comme poids la moyenne des effectifs connus des variantes, et non leur somme. Les enquêtes sans effectif connu sont exclues des moyennes pondérées. Cette pondération ne corrige ni les méthodes des instituts ni les différences de scénarios.")),
              chart_card("03 · Des hypothèses conditionnelles", NULL,
                         tags$p("Le score dépend de la liste des candidats proposés. Comparer des configurations différentes peut confondre un changement de popularité avec un changement de l’offre électorale."),
                         tags$p("Les moyennes affichées sont donc des résumés descriptifs, pas un classement électoral unique. La vue « Scénarios » permet d’examiner une liste complète, puis de suivre les enquêtes à candidatures identiques.")),
              chart_card("04 · Les intervalles indicatifs", NULL,
                         tags$p("Les intervalles présentés dans les scénarios utilisent l’approximation binomiale à 95 % :"),
                         tags$p(class = "formula", "p ± 1,96 × √[p × (1 − p) / n]"),
                         tags$p("Ici, p est la proportion et n l’effectif disponible. Les quotas, les redressements et les non-réponses ne sont pas modélisés. Si seul l’échantillon global est connu, l’intervalle peut surestimer la précision du score. Il ne représente pas une probabilité de victoire.")),
              chart_card("05 · Les sources et la traçabilité", NULL,
                         tags$p("La base de sondages, publiée sur data.gouv.fr et issue d’une extraction de Wikipédia, est actualisée quotidiennement lorsque l’application fonctionne. Elle conserve, lorsqu’ils sont disponibles, l’identifiant de révision, l’institut, les dates de terrain et l’adresse de la notice source."),
                         tags$p("Pour une analyse approfondie, vérifiez la notice originale : libellé de la question, population interrogée, effectif utilisé, méthode de recueil et redressements. Les liens de source sont accessibles dans les tables.")),
              chart_card("06 · Les limites à garder en tête", NULL,
                         tags$p("La couverture dépend des données présentes dans le fichier. Les valeurs manquantes et les informations de qualité sont signalées dans l’onglet « Données ». Le filtre des configurations principales retient uniquement celles explicitement identifiées comme telles."),
                         tags$p("Le lissage utilise une fenêtre glissante, calculée seulement aux dates observées, sans extrapolation. La table des candidats indique leur couverture et la date de leur dernière observation."),
                         tags$p("Le lissage ne supprime pas l’incertitude. Les variations modestes, les comparaisons entre instituts et les duels rarement testés demandent une prudence particulière."))
            )
          )
        ),
        tags$footer(class = "app-footer", tags$span("PRÉSIDENTIELLE 2027 · OBSERVATOIRE"),
                    tags$span("Intentions déclarées · Scénarios testés · Aucune prévision"))
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    favicon(),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    bundle_resources(path = app_sys("app/www"), app_title = "Présidentielle 2027")
  )
}
