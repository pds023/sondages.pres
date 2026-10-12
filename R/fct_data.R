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

# Each study contributes once per candidate; alternative lineups are not independent samples.
poll_study_estimates <- function(df, weighting) {
  if (!nrow(df)) return(data.frame())
  if (length(unique(df$tour)) > 1L) {
    stop("Sélectionnez un seul tour avant de calculer une synthèse.", call. = FALSE)
  }
  df <- df[is.finite(df$score) & !is.na(df$terrain_fin), , drop = FALSE]
  if (!nrow(df)) return(data.frame())
  groups <- split(seq_len(nrow(df)), paste(df$sondage_id, df$candidat_id, sep = "::"))
  studies <- do.call(rbind, lapply(groups, function(idx) {
    x <- df[idx, , drop = FALSE]
    latest <- which.max(x$terrain_fin)
    sample <- x$sample_n[is.finite(x$sample_n) & x$sample_n > 0]
    data.frame(sondage_id = x$sondage_id[1L], candidat_id = x$candidat_id[1L],
               candidat = x$candidat[latest], parti = x$parti[latest],
               score = mean(x$score), min_score = min(x$score), max_score = max(x$score),
               sample_n = if (length(sample)) mean(sample) else NA_real_,
               last_date = max(x$terrain_fin))
  }))
  if (weighting == "sample") studies <- studies[is.finite(studies$sample_n), , drop = FALSE]
  rownames(studies) <- NULL
  studies
}

poll_summarise_studies <- function(studies, weighting) {
  if (!nrow(studies)) return(poll_empty_estimates())
  result <- do.call(rbind, lapply(split(studies, studies$candidat_id), function(x) {
    latest <- which.max(x$last_date)
    data.frame(candidat_id = x$candidat_id[1L], candidat = x$candidat[latest],
               parti = x$parti[latest],
               score = if (weighting == "sample") stats::weighted.mean(x$score, x$sample_n) else mean(x$score),
               n_polls = nrow(x), min_score = min(x$min_score), max_score = max(x$max_score),
               last_date = max(x$last_date))
  }))
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
    dates <- sort(unique(x$last_date))
    do.call(rbind, lapply(seq_along(dates), function(i) {
      day <- dates[i]
      result <- poll_summarise_studies(x[x$last_date <= day & x$last_date > day - window, , drop = FALSE], weighting)
      result$date <- day
      result
    }))
  }))
  result <- result[order(result$date, result$candidat_id), , drop = FALSE]
  rownames(result) <- NULL
  result
}
