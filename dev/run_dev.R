# À exécuter depuis la racine du projet.
options(golem.app.prod = FALSE)
pkgload::load_all(".", quiet = TRUE)
sondages.pres::run_app()
