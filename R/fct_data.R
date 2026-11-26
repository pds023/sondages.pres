#' Read and enrich polling observations
#'
#' Scores remain as published: rounding differences are never normalised.
#' @param path Path to the UTF-8 source CSV.
#' @return A data frame with one row per scenario and candidate.
#' @noRd
read_poll_data <- function(path) {
  if (!file.exists(path)) stop("Fichier de sondages introuvable : ", path, call. = FALSE)
  df <- utils::read.csv(
    path, fileEncoding = "UTF-8-BOM", colClasses = "character",
    check.names = FALSE, na.strings = c("", "NA"), strip.white = TRUE
  )
  df[] <- lapply(df, function(x) {
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  })
  checks <- validate_poll_data(df)
  if (!checks$valid) stop(paste(checks$errors, collapse = "\n"), call. = FALSE)
  if (!nrow(df)) stop("Le fichier de sondages ne contient aucune observation.", call. = FALSE)
  for (nm in c("terrain_debut", "terrain_fin")) df[[nm]] <- as.Date(df[[nm]])
  for (nm in c("echantillon", "tour", "configuration", "configuration_principale",
               "echantillon_configuration", "score")) {
    df[[nm]] <- poll_number(df[[nm]])
  }
  df$scenario_id <- paste(df$sondage_id, df$tour, df$configuration, sep = "::")
  df$sample_n <- ifelse(!is.na(df$echantillon_configuration),
                        df$echantillon_configuration, df$echantillon)
  df$sample_kind <- ifelse(!is.na(df$echantillon_configuration), "configuration",
                           ifelse(!is.na(df$echantillon), "global", "missing"))
  df$scenario_signature <- character(nrow(df))
  df$scenario_label <- character(nrow(df))
  for (idx in split(seq_len(nrow(df)), df$scenario_id)) {
    lineup <- idx[order(df$candidat_id[idx])]
    df$scenario_signature[idx] <- paste0(df$tour[idx[1L]], "::",
                                         paste(df$candidat_id[lineup], collapse = "|"))
    df$scenario_label[idx] <- paste(df$candidat[lineup], collapse = " · ")
  }
  df <- df[order(df$terrain_fin, df$sondage_id, df$tour, df$configuration,
                 -df$score, df$candidat_id), , drop = FALSE]
  rownames(df) <- NULL
  attr(df, "data_warnings") <- checks$warnings
  df
}

poll_number <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))

#' Check the source schema and values without altering observations
#' @param df Source or enriched polling data frame.
#' @return A list containing valid, errors and warnings.
#' @noRd
validate_poll_data <- function(df) {
  errors <- warnings <- character()
  required <- c("sondage_id", "institut", "terrain_debut", "terrain_fin", "echantillon",
                "tour", "configuration", "configuration_principale",
                "echantillon_configuration", "candidat_id", "candidat", "parti",
                "score", "source", "wikipedia_revid", "url_source")
  missing <- setdiff(required, names(df))
  if (length(missing)) return(list(
    valid = FALSE, errors = paste("Colonnes manquantes :", paste(missing, collapse = ", ")),
    warnings = warnings
  ))
  if (!nrow(df)) return(list(valid = TRUE, errors = errors, warnings = "Le fichier est vide."))
  for (nm in c("sondage_id", "institut", "candidat_id", "candidat", "parti")) {
    if (any(is.na(df[[nm]]) | !nzchar(trimws(df[[nm]])))) {
      errors <- c(errors, paste("Valeur obligatoire absente :", nm))
    }
  }
  dates <- lapply(df[c("terrain_debut", "terrain_fin")], function(x) {
    suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))
  })
  for (nm in names(dates)) {
    valid_date <- !is.na(dates[[nm]]) &
      format(dates[[nm]], "%Y-%m-%d") == as.character(df[[nm]])
    if (any(!valid_date)) errors <- c(errors, paste("Date absente ou invalide :", nm))
  }
  if (any(dates$terrain_debut > dates$terrain_fin, na.rm = TRUE)) {
    errors <- c(errors, "Le début du terrain dépasse sa date de fin.")
  }
  numeric_names <- c("echantillon", "tour", "configuration", "configuration_principale",
                     "echantillon_configuration", "score")
  values <- lapply(df[numeric_names], poll_number)
  for (nm in numeric_names) {
    present <- !is.na(df[[nm]]) & nzchar(trimws(as.character(df[[nm]])))
    if (any(present & !is.finite(values[[nm]]))) {
      errors <- c(errors, paste("Nombre invalide :", nm))
    }
  }
  if (any(!is.finite(values$score) | values$score < 0 | values$score > 100)) {
    errors <- c(errors, "Les scores doivent être renseignés et compris entre 0 et 100.")
  }
  if (any(is.na(values$tour) | !values$tour %in% c(1, 2))) {
    errors <- c(errors, "Le tour doit être égal à 1 ou 2.")
  }
  if (any(!is.finite(values$configuration) | values$configuration < 1 |
          values$configuration != floor(values$configuration))) {
    errors <- c(errors, "Les numéros de configuration doivent être des entiers positifs.")
  }
  if (any(!is.na(values$configuration_principale) &
          !values$configuration_principale %in% c(0, 1))) {
    errors <- c(errors, "La configuration principale doit valoir 0, 1 ou être absente.")
  }
  for (nm in c("echantillon", "echantillon_configuration")) {
    if (any(values[[nm]] <= 0, na.rm = TRUE)) {
      errors <- c(errors, paste("L'effectif doit être strictement positif :", nm))
    }
  }
  if (any(values$echantillon_configuration > values$echantillon, na.rm = TRUE)) {
    warnings <- c(warnings, "Un effectif de configuration dépasse l'effectif global.")
  }
  scenario <- paste(df$sondage_id, values$tour, values$configuration, sep = "::")
  key <- paste(scenario, df$candidat_id, sep = "::")
  if (anyDuplicated(key)) errors <- c(errors, "Un candidat apparaît plusieurs fois dans un même scénario.")
  totals <- tapply(values$score, scenario, sum)
  off_total <- sum(abs(totals - 100) > 1, na.rm = TRUE)
  if (off_total) warnings <- c(warnings, sprintf(
    "%s scénario(s) totalisent moins de 99 %% ou plus de 101 %% ; les scores publiés sont conservés.", off_total
  ))
  missing_n <- is.na(values$echantillon) & is.na(values$echantillon_configuration)
  if (any(missing_n)) warnings <- c(warnings, sprintf(
    "%s sondage(s) comportent des observations sans effectif renseigné.",
    length(unique(df$sondage_id[missing_n]))
  ))
  list(valid = !length(errors), errors = unique(errors), warnings = warnings)
}

poll_empty_estimates <- function() {
  data.frame(candidat_id = character(), candidat = character(), parti = character(),
             score = numeric(), n_polls = integer(), min_score = numeric(),
             max_score = numeric(), last_date = as.Date(character()))
}

# Sort by group, latest date first (ties keep their order): the first row of a group is its latest.
poll_latest_first <- function(df, group, date) {
  df <- df[order(group, -as.numeric(date)), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# Each study contributes once per candidate; alternative lineups are not independent samples.
poll_study_estimates <- function(df, weighting) {
  if (!nrow(df)) return(data.frame())
  if (length(unique(df$tour)) > 1L) {
    stop("Sélectionnez un seul tour avant de calculer une synthèse.", call. = FALSE)
  }
  df <- df[is.finite(df$score) & !is.na(df$terrain_fin), , drop = FALSE]
  if (!nrow(df)) return(data.frame())
  key <- paste(df$sondage_id, df$candidat_id, sep = "::")
  df <- poll_latest_first(df, key, df$terrain_fin)
  key <- paste(df$sondage_id, df$candidat_id, sep = "::")
  g <- factor(key, levels = unique(key))
  first <- !duplicated(key)
  sample <- ifelse(is.finite(df$sample_n) & df$sample_n > 0, df$sample_n, NA_real_)
  studies <- data.frame(
    sondage_id = df$sondage_id[first], candidat_id = df$candidat_id[first],
    candidat = df$candidat[first], parti = df$parti[first],
    score = as.vector(tapply(df$score, g, mean)),
    min_score = as.vector(tapply(df$score, g, min)), max_score = as.vector(tapply(df$score, g, max)),
    sample_n = as.vector(tapply(sample, g, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))),
    last_date = df$terrain_fin[first]
  )
  if (weighting == "sample") studies <- studies[is.finite(studies$sample_n), , drop = FALSE]
  rownames(studies) <- NULL
  studies
}

poll_summarise_studies <- function(studies, weighting) {
  if (!nrow(studies)) return(poll_empty_estimates())
  x <- poll_latest_first(studies, studies$candidat_id, studies$last_date)
  g <- factor(x$candidat_id, levels = unique(x$candidat_id))
  first <- !duplicated(x$candidat_id)
  w <- if (weighting == "sample") x$sample_n else rep(1, nrow(x))
  result <- data.frame(
    candidat_id = x$candidat_id[first], candidat = x$candidat[first], parti = x$parti[first],
    score = as.vector(tapply(x$score * w, g, sum) / tapply(w, g, sum)),
    n_polls = as.vector(table(g)), min_score = as.vector(tapply(x$min_score, g, min)),
    max_score = as.vector(tapply(x$max_score, g, max)), last_date = x$last_date[first]
  )
  result <- result[order(-result$score, result$candidat_id), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Descriptive candidate averages after collapsing alternative lineups per study
#' @param df Enriched observations filtered to one round.
#' @param weighting Equal study weights, or mean known sample size per study/candidate.
#' @return Candidate scores and coverage. Missing samples are excluded in sample mode.
#' @noRd
poll_candidate_estimates <- function(df, weighting = c("equal", "sample")) {
  weighting <- match.arg(weighting)
  poll_summarise_studies(poll_study_estimates(df, weighting), weighting)
}

#' Trailing calendar-window averages evaluated only on observed candidate dates
#' @param df Enriched observations filtered to one round.
#' @param window Number of calendar days, including the observation date.
#' @param weighting Study weighting method.
#' @return Candidate estimates with an additional date column; no extrapolation.
#' @noRd
poll_trend <- function(df, window = 90, weighting = c("equal", "sample")) {
  weighting <- match.arg(weighting)
  if (length(window) != 1L || !is.finite(window) || window < 1 || window != floor(window)) {
    stop("La fenêtre doit être un nombre entier de jours strictement positif.", call. = FALSE)
  }
  studies <- poll_study_estimates(df, weighting)
  if (!nrow(studies)) {
    result <- poll_empty_estimates()
    result$date <- as.Date(character())
    return(result)
  }
  result <- do.call(rbind, lapply(split(studies, studies$candidat_id), function(x) {
    x <- x[order(x$last_date), , drop = FALSE]
    t <- as.numeric(x$last_date)
    day <- unique(t)
    # Window (day - window, day]: studies lo..hi of the date-sorted table.
    hi <- findInterval(day, t)
    lo <- findInterval(day - window, t) + 1L
    w <- if (weighting == "sample") x$sample_n else rep(1, nrow(x))
    over <- function(f) vapply(seq_along(day), function(i) f(lo[i]:hi[i]), numeric(1))
    latest <- match(day, t)
    data.frame(candidat_id = x$candidat_id[1L], candidat = x$candidat[latest], parti = x$parti[latest],
               score = over(function(k) sum(x$score[k] * w[k]) / sum(w[k])), n_polls = hi - lo + 1L,
               min_score = over(function(k) min(x$min_score[k])), max_score = over(function(k) max(x$max_score[k])),
               last_date = x$last_date[hi], date = x$last_date[hi])
  }))
  result <- result[order(result$date, result$candidat_id), , drop = FALSE]
  rownames(result) <- NULL
  result
}
