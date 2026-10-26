# Observatoire · Présidentielle 2027

Une application **Shiny / Golem** pour explorer les intentions de vote, comparer les hypothèses de candidature et consulter les sondages à leur source. L'interface et les analyses sont en français. L'application actualise automatiquement les sondages depuis data.gouv.fr et conserve une copie embarquée utilisable si la source est indisponible.

## Lancer l'application

Ouvrir `sondages.pres.Rproj`, puis exécuter dans la console R :

```r
install.packages(c(
  "shiny", "bslib", "golem", "config", "plotly", "DT", "htmltools", "pkgload"
))
pkgload::load_all(".")
sondages.pres::run_app()
```

R ≥ 4.1, Shiny ≥ 1.8 et bslib ≥ 0.9 sont nécessaires. Une fois les dépendances installées, `shiny::runApp(".")` fonctionne également grâce au fichier `app.R`. Le script `dev/run_dev.R` fournit le même point d'entrée pour le développement.

## Explorer les sept vues

| Vue | Ce qu'elle permet d'examiner |
| --- | --- |
| Vue d'ensemble | Les indicateurs de couverture, les moyennes des 90 derniers jours sélectionnés et les derniers sondages ; sélectionner une enquête ouvre ses scénarios. |
| Trajectoires | Les observations individuelles, les moyennes glissantes à fenêtre réglable, la distribution des scores et la synthèse par candidat. |
| Scénarios | Les résultats d'une configuration complète, la comparaison de deux configurations, les intervalles approximatifs et l'historique à liste de candidats identique. |
| Second tour | Les courbes de chaque duel, la matrice des confrontations et les enquêtes détaillées. |
| Instituts | Le nombre d'enquêtes et les écarts descriptifs de chaque institut à la moyenne des données sélectionnées. |
| Données | Les observations détaillées, les indicateurs de qualité, les notices et l'export CSV de la sélection, de la base complète ou de la synthèse. |
| Méthode | Les règles de calcul, la qualité des données et les limites d'interprétation. |

Les filtres permettent de restreindre les analyses à une période, des instituts, un tour ou des candidats. La vue Scénarios conserve la liste complète des candidats de chaque configuration. La vue Second tour utilise la période et les instituts sélectionnés, avec son propre choix de duel. Les graphiques sont interactifs : survol, légendes, zoom et téléchargement PNG. Les tableaux sont recherchables et les exports CSV permettent de poursuivre l'analyse ailleurs.

## Lire les résultats

La synthèse récente utilise une **fenêtre de 90 jours** se terminant à la fin de la période sélectionnée ; cette date correspond par défaut aux dernières données disponibles. Les tendances utilisent la fenêtre glissante choisie, et la synthèse tabulaire porte sur toute la période sélectionnée. Pour éviter de surreprésenter un sondage qui teste beaucoup d'hypothèses, les variantes sont d'abord agrégées par sondage et candidat, puis entre sondages. Une pondération par la taille d'échantillon peut être activée ; elle ne corrige ni les biais de sélection, ni les différences de protocole, ni les différences entre scénarios. Les enquêtes sans effectif exploitable sont exclues des moyennes pondérées.

Les intervalles affichés sont des **approximations statistiques**, lorsque les informations nécessaires sont disponibles. Ils ne reproduisent pas les marges publiées par les instituts : les enquêtes par quotas, redressements, arrondis et sous-échantillons demandent des précautions particulières. Une petite différence ou un croisement de courbes ne suffit pas à démontrer une évolution de l'opinion.

Les candidatures testées peuvent changer d'un sondage à l'autre. Une moyenne entre hypothèses décrit les données sélectionnées ; elle ne correspond pas nécessairement à un bulletin de vote cohérent. Les différences entre instituts sont descriptives et peuvent refléter leur calendrier ou les scénarios testés. **Aucune projection électorale ni probabilité de victoire n'est calculée.**

## Données et mise à jour

- La source actualisée est le [CSV publié sur data.gouv.fr](https://www.data.gouv.fr/api/1/datasets/r/6b97e100-6031-42e0-9c93-aeb9dd49ef0d).
- `raw-data/sondages-presidentielle-2027.csv` est la copie locale du projet, utilisée en secours ou en mode local.
- `inst/extdata/sondages-presidentielle-2027.csv` est la copie de secours embarquée pour l'application déployée ou le package installé.
- Le jeu de données provient d'une extraction de tableaux Wikipédia. Les liens vers les notices et les sources, lorsqu'ils sont renseignés, permettent de vérifier les enquêtes originales.
- Les valeurs absentes, les scénarios multiples et les variations de libellé font partie des limites du jeu de données. La vue Méthode explicite leur traitement.

Par défaut, l'application télécharge et valide le CSV au démarrage, puis une fois par nouvelle date dans le fuseau **Europe/Paris**, tant qu'une session est active. Les sessions d'un même processus partagent ce téléchargement. En cas d'échec, elle conserve les dernières données valides et réessaie après une heure ; sans téléchargement valide, elle utilise le fichier de `raw-data/` s'il existe, sinon la copie embarquée.

Les téléchargements sont stockés dans un cache temporaire du processus et ne modifient jamais les fichiers de `raw-data/` ou `inst/extdata/`. Ce cache disparaît au redémarrage : un nouveau téléchargement est alors effectué. Sur Connect Cloud, une application arrêtée ou en veille ne peut rien télécharger ; l'actualisation reprend lors de sa prochaine visite. Il n'est pas nécessaire de redéployer pour chaque actualisation du CSV sur data.gouv.fr une fois cette version de l'application publiée.

Les filtres choisis sont conservés lorsqu'ils restent disponibles. Une période allant jusqu'au dernier terrain s'étend aux nouvelles dates ; une sélection de tous les instituts ou de tous les candidats inclut les nouveaux arrivants.

Pour travailler uniquement avec la copie locale du projet, désactiver le téléchargement **avant de démarrer** :

```r
options(sondages.pres.auto_download = FALSE)
pkgload::load_all(".")
sondages.pres::run_app()
```

Remplacer ou enregistrer alors **le même fichier** `raw-data/sondages-presidentielle-2027.csv`, en conservant son nom et ses colonnes. L'application vérifie les modifications toutes les deux secondes et recharge les données dans les sessions ouvertes. Déposer un fichier avec un autre nom ne change pas la source sélectionnée.

Pour utiliser un fichier à un autre emplacement, indiquer son chemin **avant de démarrer** :

```r
options(sondages.pres.data = "C:/chemin/sondages-presidentielle-2027.csv")
pkgload::load_all(".")
sondages.pres::run_app()
```

L'option `sondages.pres.data` donne la priorité à ce fichier et désactive également le téléchargement automatique. Ce fichier est surveillé toutes les deux secondes. Si un fichier local devient temporairement absent ou invalide pendant une sauvegarde, les sessions ouvertes gardent la dernière version valide et affichent un avertissement jusqu'à sa correction.

Si un script produit le CSV ligne par ligne, lui faire écrire un fichier temporaire puis remplacer le fichier surveillé une fois l'export terminé : un fichier partiel contenant déjà des lignes valides ne peut pas être distingué d'une base volontairement plus courte.

Une application hébergée sur **Posit Connect Cloud** n'a pas accès aux fichiers du PC. Pour publier un changement de code, de dépendances ou de copie de secours, exécuter depuis la racine du projet :

```r
source("dev/generate_manifest.R")
```

Ce script valide le CSV de `raw-data/`, le copie dans `inst/extdata/`, vérifie leur identité et régénère `manifest.json`. Publier ensuite sur GitHub les fichiers modifiés de l'application, la copie dans `inst/extdata/` et `manifest.json`, puis redéployer dans Connect Cloud. Cette étape est nécessaire pour installer le mécanisme de téléchargement automatique, puis seulement lors d'un changement des fichiers publiés. Le même script synchronise la copie de secours avant de construire un package ou de déployer sur shinyapps.io.

## Vérifier et déployer

Les tests portent sur la préparation des données et les calculs :

```r
install.packages("testthat")
pkgload::load_all(".")
testthat::test_local(".")
```

Pour un hébergement shinyapps.io, configurer son compte avec `rsconnect`, puis synchroniser les données et déployer depuis la racine :

```r
install.packages("rsconnect")
# Configurer une première fois le compte via rsconnect::setAccountInfo(...).
source("dev/generate_manifest.R")
rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "app.R", "DESCRIPTION", "NAMESPACE", "LICENSE",
    list.files(c("R", "inst"), recursive = TRUE, full.names = TRUE)
  ),
  appName = "observatoire-presidentielle-2027"
)
```

Le point d'entrée charge le package local avec `pkgload` ; cette dépendance doit être disponible sur l'hébergement. Le nom d'application et le compte d'hébergement sont à adapter. Les données embarquées sont incluses dans le déploiement. Une installation classique du package permet également le lancement par `sondages.pres::run_app()`.

Les métadonnées d'auteur et l'adresse `contact@example.org` sont des valeurs génériques à remplacer avant publication. Aucune licence de réutilisation n'a été accordée au code ; les droits et conditions applicables aux données et sources doivent être conservés lors de leur redistribution.
