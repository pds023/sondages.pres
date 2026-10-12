# Point d'entrée pour shiny::runApp() et les hébergeurs Shiny.
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Installez pkgload avec install.packages('pkgload') pour lancer le projet.",
       call. = FALSE)
}
pkgload::load_all(".", quiet = TRUE, export_all = FALSE,
                  helpers = FALSE, attach_testthat = FALSE)
run_app()
