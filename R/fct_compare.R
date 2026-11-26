# Comparaison des campagnes 2017, 2022 et 2027, alignées sur la date du vote.

# 2027 : dates publiées par service-public.gouv.fr, décret de convocation à paraître.
election_dates <- list(
  "2017" = as.Date(c("2017-04-23", "2017-05-07")),
  "2022" = as.Date(c("2022-04-10", "2022-04-24")),
  "2027" = as.Date(c("2027-04-18", "2027-05-02"))
)

# Résultats proclamés par le Conseil constitutionnel, en % des suffrages exprimés.
election_results <- data.frame(
  election = rep(c("2017", "2022"), c(13L, 14L)),
  tour = rep(c(1L, 2L, 1L, 2L), c(11L, 2L, 12L, 2L)),
  candidat_id = c("macron", "le-pen", "fillon", "melenchon", "hamon", "dupont-aignan", "lassalle",
                  "poutou", "asselineau", "arthaud", "cheminade", "macron", "le-pen",
                  "macron", "le-pen", "melenchon", "zemmour", "pecresse", "jadot", "lassalle",
                  "roussel", "dupont-aignan", "hidalgo", "poutou", "arthaud", "macron", "le-pen"),
  parti = c("EM", "FN", "LR", "LFI", "PS", "DLF", "RES", "NPA", "UPR", "LO", "S&P", "EM", "FN",
            "LREM", "RN", "LFI", "REC", "LR", "EELV", "RES", "PCF", "DLF", "PS", "NPA", "LO", "LREM", "RN"),
  score = c(24.01, 21.30, 20.01, 19.58, 6.36, 4.70, 1.21, 1.09, 0.92, 0.64, 0.18, 66.10, 33.90,
            27.85, 23.15, 21.95, 7.07, 4.78, 4.63, 3.13, 2.28, 2.06, 1.75, 0.77, 0.56, 58.55, 41.45)
)

compare_families <- c("gauche" = "Gauche et écologistes", "centre" = "Centre",
                      "droite" = "Droite et centre droit", "extreme-droite" = "Extrême droite",
                      "autres" = "Autres candidatures")
party_families <- c(
  LO = "gauche", NPA = "gauche", EXG = "gauche", LFI = "gauche", FG = "gauche", PCF = "gauche",
  PS = "gauche", PRG = "gauche", EELV = "gauche", LE = "gauche", PP = "gauche", GRS = "gauche",
  DVG = "gauche", EM = "centre", LREM = "centre", RE = "centre", MoDem = "centre", HOR = "centre",
  Agir = "centre", LR = "droite", UMP = "droite", UDI = "droite", PCD = "droite",
  FN = "extreme-droite", RN = "extreme-droite", REC = "extreme-droite", DLF = "extreme-droite",
  DLR = "extreme-droite", LP = "extreme-droite", MPF = "extreme-droite", UDR = "extreme-droite"
)
# Labels that changed during a cycle (Macron: SE, then DVG, then EM).
candidate_families <- c(macron = "centre", hulot = "gauche")
election_colors <- c("2017" = "#86B9C0", "2022" = "#459AA4", "2027" = "#087F8C")

poll_family <- function(candidat_id, parti) {
  family <- unname(party_families[parti])
  override <- unname(candidate_families[candidat_id])
  family[!is.na(override)] <- override[!is.na(override)]
  family[is.na(family)] <- "autres"
  family
}

.history <- new.env(parent = emptyenv())

# 2017 and 2022 never change: read and validate them once per process.
history_polls <- function() {
  if (is.null(.history$polls)) {
    .history$polls <- lapply(c("2017" = "2017", "2022" = "2022"), function(year) {
      file <- paste0("sondages-presidentielle-", year, ".csv")
      path <- app_sys("extdata", file)
      read_poll_data(if (nzchar(path)) path else file.path("inst", "extdata", file))
    })
  }
  .history$polls
}

# The three campaigns in one table; `jours` counts the days left before the round polled.
compare_data <- function(polls_2027, history = history_polls()) {
  data <- c(history, list("2027" = polls_2027))
  columns <- c("sondage_id", "institut", "terrain_fin", "tour", "scenario_id", "candidat_id",
               "candidat", "parti", "score", "sample_n")
  do.call(rbind, lapply(names(data), function(e) {
    d <- data[[e]][, columns]
    d$election <- rep(e, nrow(d))
    d$jours <- as.integer(election_dates[[e]][d$tour] - d$terrain_fin)
    d$famille <- poll_family(d$candidat_id, d$parti)
    d
  }))
}

# First-round studies, one row per election, poll and series. A series is a candidate,
# or a political family ("f:gauche"...) summed within each configuration.
compare_studies <- function(data) {
  d <- data[data$tour == 1, , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  key <- paste(d$scenario_id, d$famille)
  families <- d[!duplicated(key), , drop = FALSE]
  families$score <- rowsum(d$score, key, reorder = FALSE)[, 1L]
  families$candidat_id <- paste0("f:", families$famille)
  families$candidat <- unname(compare_families[families$famille])
  d <- rbind(d, families)
  do.call(rbind, lapply(split(d, d$election), function(x) {
    s <- poll_study_estimates(x, "equal")
    poll <- match(s$sondage_id, x$sondage_id)
    s$election <- x$election[poll]
    s$institut <- x$institut[poll]
    s$jours <- x$jours[poll]
    s
  }))
}

# Mean of the studies run in [day, day + window) days before the vote, each institute
# counting once: daily rolling waves would otherwise outweigh monthly surveys.
compare_average <- function(studies, days, window) {
  hits <- lapply(days, function(day) which(studies$jours >= day & studies$jours < day + window))
  long <- studies[unlist(hits), c("election", "candidat_id", "institut", "sondage_id", "score"), drop = FALSE]
  if (!nrow(long)) {
    return(data.frame(election = character(), candidat_id = character(), jour = integer(),
                      score = numeric(), n_instituts = integer(), n_sondages = integer()))
  }
  long$jour <- rep(days, lengths(hits))
  by_institute <- stats::aggregate(score ~ election + candidat_id + jour + institut, long, mean)
  result <- stats::aggregate(score ~ election + candidat_id + jour, by_institute, mean)
  result$n_instituts <- stats::aggregate(score ~ election + candidat_id + jour, by_institute, length)$score
  result$n_sondages <- stats::aggregate(score ~ election + candidat_id + jour, long, length)$score
  result
}

# Weekly points anchored on `stage`, so that the 2027 line ends on its latest poll.
# A line never runs past the last poll of its election: no extrapolation.
compare_trend <- function(studies, serie, window = 30, stage = NA) {
  s <- studies[studies$candidat_id == serie, , drop = FALSE]
  start <- if (is.finite(stage)) stage %% 7 else 0
  days <- if (nrow(s) && max(s$jours) >= start) seq(start, max(s$jours), by = 7) else integer()
  result <- compare_average(s, days, window)
  last <- tapply(s$jours, s$election, min)
  result[result$jour >= last[result$election], , drop = FALSE]
}

# Official first-round results by family (sum of the family's candidates).
compare_family_results <- function() {
  r <- election_results[election_results$tour == 1L, ]
  r$famille <- poll_family(r$candidat_id, r$parti)
  stats::aggregate(score ~ election + famille, r, sum)
}

# Each institute's last poll in the week before each round, against the official result.
compare_errors <- function(data, days = 7) {
  d <- data[data$election %in% election_results$election & data$jours >= 1 & data$jours <= days, , drop = FALSE]
  # One key: before R 4.5, ave() also ran max() on empty combinations of several factors.
  latest <- stats::ave(as.numeric(d$terrain_fin), paste(d$election, d$tour, d$institut), FUN = max)
  d <- d[as.numeric(d$terrain_fin) == latest, , drop = FALSE]
  studies <- do.call(rbind, lapply(split(d, paste(d$election, d$tour)), function(x) {
    s <- poll_study_estimates(x, "equal")
    poll <- match(s$sondage_id, x$sondage_id)
    s$election <- x$election[poll]
    s$tour <- x$tour[poll]
    s$institut <- x$institut[poll]
    s
  }))
  by_institute <- stats::aggregate(score ~ election + tour + candidat_id + institut, studies, mean)
  result <- stats::aggregate(score ~ election + tour + candidat_id, by_institute, mean)
  result$n_instituts <- stats::aggregate(score ~ election + tour + candidat_id, by_institute, length)$score
  result$candidat <- studies$candidat[match(result$candidat_id, studies$candidat_id)]
  names(result)[names(result) == "score"] <- "sondages"
  result <- merge(result, election_results[c("election", "tour", "candidat_id", "score")],
                  by = c("election", "tour", "candidat_id"))
  names(result)[names(result) == "score"] <- "resultat"
  result$ecart <- result$sondages - result$resultat
  result <- result[order(result$election, result$tour, -result$resultat), ]
  rownames(result) <- NULL
  result
}

# Families, then candidates with at least ten first-round polls in two elections or more.
compare_series_choices <- function(studies) {
  people <- studies[!startsWith(studies$candidat_id, "f:"), , drop = FALSE]
  counts <- table(people$candidat_id, people$election)
  ids <- rownames(counts)[rowSums(counts >= 10) >= 2]
  latest <- people[order(people$last_date, decreasing = TRUE), ]
  labels <- latest$candidat[match(ids, latest$candidat_id)]
  list("Courants politiques" = stats::setNames(paste0("f:", names(compare_families)), compare_families),
       "Candidats présents à plusieurs scrutins" = stats::setNames(ids, labels)[order(labels)])
}

history_trend_chart <- function(trend, results, stage) {
  if (!nrow(trend)) return(empty_chart("Aucun sondage de premier tour pour ce choix."))
  p <- plotly::plot_ly()
  for (e in names(election_colors)) {
    t <- trend[trend$election == e, , drop = FALSE]
    if (!nrow(t)) next
    t <- t[order(t$jour, decreasing = TRUE), , drop = FALSE]
    hover <- paste0("<b>", e, "</b> · ", fmt_number(t$score, 1), " %<br>J-", t$jour, " (",
                    fmt_date(election_dates[[e]][1] - t$jour), ") · ", t$n_sondages, " sondage(s), ",
                    t$n_instituts, " institut(s)")
    p <- plotly::add_trace(p, x = -t$jour, y = t$score, type = "scatter", mode = "lines",
      name = paste("Campagne", e), legendgroup = e, line = list(color = election_colors[[e]], width = 2),
      text = hover, hoverinfo = "text")
    r <- results[results$election == e, , drop = FALSE]
    if (nrow(r)) p <- plotly::add_trace(p, x = 0, y = r$score, type = "scatter", mode = "markers",
      name = paste("Résultat", e), legendgroup = e, cliponaxis = FALSE,
      marker = list(symbol = "diamond", size = 12, color = election_colors[[e]], line = list(color = "#FFFFFF", width = 2)),
      text = paste0("<b>Résultat ", e, "</b> · ", fmt_number(r$score, 2), " %"), hoverinfo = "text")
  }
  months <- c(24, 18, 12, 6)
  p <- plotly::layout(chart_style(p),
    xaxis = list(range = c(-745, 20), tickvals = c(-months * 30.4375, 0),
                 ticktext = c(paste0("−", months, " mois"), "1er tour"), showgrid = TRUE, gridcolor = "#E9EDF2"),
    yaxis = list(rangemode = "tozero"))
  if (!is.finite(stage)) return(p)
  plotly::layout(p,
    shapes = list(list(type = "line", x0 = -stage, x1 = -stage, y0 = 0, y1 = 1, yref = "paper",
                       line = list(color = "#99A5B5", width = 1))),
    annotations = list(list(x = -stage, y = 1, yref = "paper", xanchor = "right", yanchor = "bottom",
                            showarrow = FALSE, text = paste0("Dernier sondage 2027 · J-", stage, " "),
                            font = list(size = 11, color = "#617185"))))
}

history_snapshot_chart <- function(snapshot, results, stage) {
  if (!nrow(snapshot)) return(empty_chart("Aucun sondage 2027 de premier tour pour situer la campagne."))
  families <- rev(names(compare_families))
  offset <- c("2017" = 0.27, "2022" = 0, "2027" = -0.27)
  p <- plotly::plot_ly()
  for (e in names(election_colors)) {
    s <- snapshot[snapshot$election == e, , drop = FALSE]
    if (!nrow(s)) next
    family <- sub("^f:", "", s$candidat_id)
    p <- plotly::add_trace(p, type = "bar", orientation = "h", x = s$score, y = match(family, families) + offset[[e]],
      width = 0.24, name = paste0(e, " · J-", stage), marker = list(color = election_colors[[e]]),
      text = paste0(fmt_number(s$score, 1), " %"), textposition = "outside", cliponaxis = FALSE,
      hovertext = paste0("<b>", compare_families[family], "</b> · ", e, "<br>", fmt_number(s$score, 1),
                         " % à J-", stage, " · ", s$n_sondages, " sondage(s), ", s$n_instituts, " institut(s)"),
      hoverinfo = "text")
  }
  r <- results[results$election %in% names(offset), , drop = FALSE]
  if (nrow(r)) p <- plotly::add_trace(p, type = "scatter", mode = "markers", x = r$score,
    y = match(r$famille, families) + unname(offset[r$election]), name = "Résultat du 1er tour",
    marker = list(symbol = "line-ns", size = 20, line = list(color = "#172338", width = 2)),
    text = paste0("<b>", compare_families[r$famille], "</b> · résultat ", r$election, " : ", fmt_number(r$score, 2), " %"),
    hoverinfo = "text")
  plotly::layout(chart_style(p, "", TRUE), barmode = "overlay",
    xaxis = list(title = "Moyenne par institut (%)", rangemode = "tozero"),
    yaxis = list(tickvals = seq_along(families), ticktext = unname(compare_families[families]),
                 range = c(0.5, length(families) + 0.5), showgrid = FALSE))
}

history_error_chart <- function(errors) {
  if (!nrow(errors)) return(empty_chart())
  elections <- unique(errors$election)
  panels <- lapply(elections, function(e) {
    d <- errors[errors$election == e, , drop = FALSE]
    d <- d[order(d$resultat), , drop = FALSE]
    hover <- paste0("<b>", safe_text(d$candidat), "</b> · ", e, "<br>Derniers sondages : ", fmt_number(d$sondages, 1),
                    " %<br>Résultat : ", fmt_number(d$resultat, 2), " %<br>Écart : ",
                    ifelse(d$ecart > 0, "+", ""), fmt_number(d$ecart, 1), " pt")
    p <- plotly::add_segments(plotly::plot_ly(), x = d$sondages, xend = d$resultat, y = d$candidat, yend = d$candidat,
      line = list(color = "#C1CCD7", width = 2), showlegend = FALSE, hoverinfo = "none")
    p <- plotly::add_trace(p, x = d$sondages, y = d$candidat, type = "scatter", mode = "markers",
      name = "Derniers sondages", legendgroup = "sondages", showlegend = e == elections[1],
      marker = list(color = "#8C99AB", size = 10, line = list(color = "#FFFFFF", width = 2)), text = hover, hoverinfo = "text")
    p <- plotly::add_trace(p, x = d$resultat, y = d$candidat, type = "scatter", mode = "markers",
      name = "Résultat officiel", legendgroup = "resultat", showlegend = e == elections[1],
      marker = list(color = "#172338", size = 10, line = list(color = "#FFFFFF", width = 2)), text = hover, hoverinfo = "text")
    # Title above the names column, clear of the toolbar shown on hover.
    plotly::layout(p, yaxis = list(categoryorder = "array", categoryarray = d$candidat),
      annotations = list(list(text = paste0("<b>", e, "</b>"), x = 0, y = 1, xref = "paper", yref = "paper",
                              xanchor = "right", yanchor = "bottom", showarrow = FALSE,
                              font = list(size = 13, color = "#172338"))))
  })
  # Stacked panels: names share the left margin and the x scale is common to all elections.
  rows <- as.vector(table(errors$election)[elections])
  p <- plotly::subplot(panels, nrows = length(panels), shareX = TRUE, heights = rows / sum(rows),
                       margin = c(0, 0, 0.08, 0.08))
  p <- chart_style(p, "", TRUE)
  y <- list(title = "", showgrid = FALSE, zeroline = FALSE, automargin = TRUE)
  ids <- c("", seq_along(elections)[-1])
  p <- do.call(plotly::layout, c(list(p), stats::setNames(rep(list(y), length(ids)), paste0("yaxis", ids))))
  plotly::layout(p, xaxis = list(range = c(0, max(errors$sondages, errors$resultat) * 1.08), showgrid = TRUE,
                                 gridcolor = "#E9EDF2", ticksuffix = " %"))
}
