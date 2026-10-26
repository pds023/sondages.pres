# Presentation helpers shared by the application views.
app_data_path <- function() {
  path <- getOption("sondages.pres.data")
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1L || is.na(path) ||
        !nzchar(path) || !file.exists(path) || dir.exists(path)) {
      stop("L'option sondages.pres.data doit désigner un fichier CSV existant.", call. = FALSE)
    }
    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }
  filename <- "sondages-presidentielle-2027.csv"
  root <- app_sys()
  if (!nzchar(root)) root <- getwd()
  if (basename(root) == "inst") root <- dirname(root)
  paths <- c(file.path(root, "raw-data", filename), file.path("raw-data", filename),
             app_sys("extdata", filename), file.path("inst", "extdata", filename))
  paths <- paths[file.exists(paths) & !dir.exists(paths)]
  if (!length(paths)) stop("Fichier de sondages introuvable : ", filename, call. = FALSE)
  normalizePath(paths[1L], winslash = "/", mustWork = TRUE)
}

load_app_data <- function(path = app_data_path()) {
  read_poll_data(path)
}

watch_poll_data <- function(path, session, intervalMillis = 2000, status = function() NULL) {
  last_good <- NULL
  get_path <- if (is.function(path)) path else function() path
  current_path <- NULL
  file_hash <- function() suppressWarnings(unname(tools::md5sum(current_path)))
  state <- shiny::reactivePoll(intervalMillis, session,
    checkFunc = function() {
      current_path <<- get_path()
      list(path = current_path, hash = file_hash(), status = status())
    },
    valueFunc = function() {
      tryCatch({
        before <- file_hash()
        data <- read_poll_data(current_path)
        after <- file_hash()
        if (anyNA(c(before, after)) || !identical(before, after)) {
          stop("Le fichier de sondages a changé pendant sa lecture. Nouvelle tentative au prochain contrôle.", call. = FALSE)
        }
        last_good <<- data
        list(data = last_good, error = NULL, status = status())
      }, error = function(e) list(data = last_good, error = conditionMessage(e), status = status()))
    })
  list(
    data = shiny::reactive({
      value <- state()
      shiny::validate(shiny::need(!is.null(value$data), value$error))
      value$data
    }),
    error = shiny::reactive(state()$error),
    status = shiny::reactive(state()$status)
  )
}

poll_colors <- function(data) {
  ids <- sort(unique(data$candidat_id))
  palette <- c("#137C8B", "#D06B48", "#6474BA", "#C05278", "#778D45", "#BF9041",
               "#488FBB", "#796296", "#397E66", "#A87962", "#535C86", "#B26157")
  result <- stats::setNames(rep(palette, length.out = length(ids)), ids)
  key <- c("le-pen" = "#234C78", "bardella" = "#538BC0", "philippe" = "#D49935",
           "attal" = "#BD7755", "melenchon" = "#C25764", "glucksmann" = "#8F6CAF",
           "retailleau" = "#087F8C", "tondelier" = "#509376", "zemmour" = "#626C84")
  result[intersect(names(key), ids)] <- key[intersect(names(key), ids)]
  result
}

fmt_number <- function(x, digits = 0) {
  format(round(x, digits), big.mark = " ", decimal.mark = ",", nsmall = digits, trim = TRUE)
}

fmt_date <- function(x) format(as.Date(x), "%d/%m/%Y")
safe_text <- function(x) as.character(htmltools::htmlEscape(x))

notice_link <- function(url) {
  vapply(url, function(x) {
    if (is.na(x) || !grepl("^https?://", x, ignore.case = TRUE)) return("")
    as.character(shiny::tags$a(href = x, target = "_blank", rel = "noopener noreferrer", "Consulter ↗"))
  }, character(1))
}

chart_style <- function(p, y_title = "Intentions de vote (%)", horizontal = FALSE) {
  p <- plotly::layout(p,
    font = list(family = "Inter, Segoe UI, Arial, sans-serif", size = 12, color = "#526078"),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = if (horizontal) 150 else 55, r = 25, t = 20, b = 65),
    xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, automargin = TRUE),
    yaxis = list(title = y_title, gridcolor = "#E9EDF2", zeroline = FALSE, automargin = TRUE),
    legend = list(orientation = "h", x = 0, y = -0.18, font = list(size = 11)),
    hoverlabel = list(bgcolor = "#FFFFFF", font = list(color = "#172338")),
    hovermode = "closest", dragmode = "zoom"
  )
  plotly::config(p, displaylogo = FALSE, responsive = TRUE, locale = "fr",
    modeBarButtonsToRemove = c("select2d", "lasso2d"),
    toImageButtonOptions = list(format = "png", filename = "observatoire-2027", scale = 2))
}

empty_chart <- function(message = "Aucune observation pour cette sélection.") {
  p <- plotly::plot_ly(type = "scatter", mode = "markers", x = numeric(), y = numeric())
  p <- chart_style(p, "")
  plotly::layout(p, xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
    annotations = list(list(text = message, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                            showarrow = FALSE, font = list(size = 15, color = "#7B8798"))))
}

trend_chart <- function(data, colors, window = 90, weighting = "equal", points = TRUE) {
  if (!nrow(data)) return(empty_chart())
  trends <- poll_trend(data, window = window, weighting = weighting)
  p <- plotly::plot_ly()
  for (id in unique(data$candidat_id)) {
    d <- data[data$candidat_id == id, , drop = FALSE]
    t <- trends[trends$candidat_id == id, , drop = FALSE]
    label <- d$candidat[1]
    if (points) {
      hover <- paste0("<b>", safe_text(d$candidat), "</b> · ", fmt_number(d$score, 1), " %",
        "<br>", safe_text(d$institut), " · ", fmt_date(d$terrain_fin),
        "<br>Configuration ", safe_text(d$configuration), " · tour ", d$tour)
      p <- plotly::add_trace(p, x = d$terrain_fin, y = d$score, type = "scatter", mode = "markers",
        name = label, legendgroup = id, showlegend = !nrow(t),
        marker = list(color = unname(colors[id]), size = 6, opacity = 0.28), text = hover, hoverinfo = "text")
    }
    if (nrow(t)) {
      hover <- paste0("<b>", safe_text(t$candidat), "</b> · ", fmt_number(t$score, 1), " %",
        "<br>", fmt_date(t$date), " · fenêtre ", window, " jours",
        "<br>", t$n_polls, " sondage(s) · moyenne des configurations")
      p <- plotly::add_trace(p, x = t$date, y = t$score, type = "scatter", mode = "lines+markers",
        name = label, legendgroup = id, line = list(color = unname(colors[id]), width = 2.8),
        marker = list(color = unname(colors[id]), size = 4), text = hover, hoverinfo = "text")
    }
  }
  if (!points && !nrow(trends)) return(empty_chart("Aucun effectif exploitable pour cette pondération."))
  chart_style(p)
}

ranking_chart <- function(data, colors, weighting = "equal") {
  estimates <- poll_candidate_estimates(data, weighting)
  if (!nrow(estimates)) return(empty_chart())
  estimates <- estimates[order(estimates$score), , drop = FALSE]
  p <- plotly::plot_ly(x = estimates$score, y = estimates$candidat, type = "bar", orientation = "h",
    marker = list(color = unname(colors[estimates$candidat_id])),
    text = paste0(fmt_number(estimates$score, 1), " %"), textposition = "outside", cliponaxis = FALSE,
    customdata = estimates$n_polls, hovertemplate = "%{y}<br>%{x:.1f} %<br>%{customdata} sondage(s)<extra></extra>")
  p <- chart_style(p, "", TRUE)
  plotly::layout(p, xaxis = list(title = "Moyenne des observations (%)", range = c(0, max(estimates$score) * 1.2)),
    yaxis = list(categoryorder = "array", categoryarray = estimates$candidat), showlegend = FALSE)
}

poll_table <- function(data, page_length = 8, escape = TRUE, selection = "none") {
  DT::datatable(data, rownames = FALSE, escape = escape, selection = selection,
    class = "compact stripe hover", filter = "none",
    options = list(pageLength = page_length, lengthMenu = c(8, 15, 30, 50), scrollX = TRUE,
      autoWidth = TRUE, order = list(), dom = "ft<'table-bottom'lip>",
      language = list(search = "Rechercher :", lengthMenu = "_MENU_ lignes",
        info = "_START_–_END_ sur _TOTAL_ lignes", infoEmpty = "Aucune ligne",
        infoFiltered = "(sur _MAX_ au total)", zeroRecords = "Aucun résultat pour cette recherche",
        emptyTable = "Aucune observation pour cette sélection", paginate = list(first = "Début", last = "Fin", "next" = "Suivant", previous = "Précédent"))))
}

write_poll_csv <- function(data, file) {
  con <- file(file, open = "wt", encoding = "UTF-8")
  on.exit(close(con))
  writeLines("\ufeff", con, sep = "")
  utils::write.table(data, con, sep = ";", dec = ",", row.names = FALSE, na = "", qmethod = "double")
}

scenario_choices <- function(data) {
  if (!nrow(data)) return(stats::setNames(character(), character()))
  meta <- data[!duplicated(data$scenario_id), , drop = FALSE]
  meta <- meta[order(meta$terrain_fin, meta$sondage_id, decreasing = TRUE), , drop = FALSE]
  stats::setNames(meta$scenario_id,
    paste0(fmt_date(meta$terrain_fin), " · ", meta$institut, " · n°", meta$configuration, " · ", meta$scenario_label))
}

duel_observations <- function(data) {
  data <- data[data$tour == 2, , drop = FALSE]
  if (!nrow(data)) return(data.frame())
  rows <- lapply(split(data, data$scenario_id), function(d) {
    if (nrow(d) != 2L || length(unique(d$candidat_id)) != 2L) return(NULL)
    d <- d[order(d$candidat_id), , drop = FALSE]
    data.frame(pair_id = paste(d$candidat_id, collapse = "|"),
      pair_label = paste(d$candidat, collapse = " / "),
      sondage_id = d$sondage_id[1], scenario_id = d$scenario_id[1], institut = d$institut[1],
      date = d$terrain_fin[1], a_id = d$candidat_id[1], a = d$candidat[1], a_score = d$score[1],
      b_id = d$candidat_id[2], b = d$candidat[2], b_score = d$score[2],
      sample_n = d$sample_n[1], url_source = d$url_source[1], stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, rows)
  if (is.null(result)) data.frame() else result[order(result$date), , drop = FALSE]
}
