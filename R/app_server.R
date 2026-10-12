#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  polls <- load_app_data()
  colors <- poll_colors(polls)
  date_limits <- range(polls$terrain_fin)
  default_candidates <- function(tour) {
    d <- polls[polls$tour == as.integer(tour), , drop = FALSE]
    e <- poll_candidate_estimates(d[d$terrain_fin >= max(d$terrain_fin) - 89, , drop = FALSE])
    utils::head(e$candidat_id[order(e$score, decreasing = TRUE)], 8)
  }
  observeEvent(input$tour, {
    d <- polls[polls$tour == as.integer(input$tour), , drop = FALSE]
    d <- d[!duplicated(d$candidat_id), , drop = FALSE]
    d <- d[order(d$candidat), , drop = FALSE]
    updateSelectizeInput(session, "candidates", choices = stats::setNames(d$candidat_id, d$candidat),
      selected = default_candidates(input$tour), server = TRUE)
  }, priority = 10)
  observeEvent(input$all_candidates, {
    updateSelectizeInput(session, "candidates", selected = unique(polls$candidat_id[polls$tour == as.integer(input$tour)]))
  })
  observeEvent(input$clear_candidates, updateSelectizeInput(session, "candidates", selected = character()))
  observeEvent(input$reset_filters, {
    updateRadioButtons(session, "tour", selected = "1")
    updateDateRangeInput(session, "period", start = date_limits[1], end = date_limits[2])
    updateSelectizeInput(session, "institutes", selected = sort(unique(polls$institut)))
    updateSelectizeInput(session, "candidates", selected = default_candidates("1"))
    updateCheckboxInput(session, "main_only", value = FALSE)
    updateSelectInput(session, "weighting", selected = "equal")
    updateSelectInput(session, "trend_window", selected = "90")
    updateCheckboxInput(session, "show_points", value = TRUE)
  })
  period_data <- reactive({
    req(input$period, length(input$period) == 2L)
    if (anyNA(input$period) || input$period[1] > input$period[2]) return(polls[FALSE, ])
    polls[polls$terrain_fin >= input$period[1] & polls$terrain_fin <= input$period[2] &
      polls$institut %in% input$institutes, , drop = FALSE]
  })
  scenario_data <- reactive({
    d <- period_data()
    d <- d[d$tour == as.integer(input$tour), , drop = FALSE]
    if (identical(input$tour, "1") && isTRUE(input$main_only)) d <- d[!is.na(d$configuration_principale) & d$configuration_principale == 1, , drop = FALSE]
    d
  })
  filtered_data <- reactive({
    d <- scenario_data()
    d[d$candidat_id %in% input$candidates, , drop = FALSE]
  })
  recent_data <- reactive({
    d <- filtered_data()
    d[d$terrain_fin >= as.Date(input$period[2]) - 89, , drop = FALSE]
  })
  weighting <- reactive(if (identical(input$weighting, "sample")) "sample" else "equal")
  output$scope_label <- renderUI({
    d <- if (identical(input$view, "duels")) period_data()[period_data()$tour == 2, , drop = FALSE] else filtered_data()
    tags$div(class = "scope-label", tags$span(class = "status-dot"),
      paste0(if (identical(input$view, "duels") || identical(input$tour, "2")) "Second tour" else "Premier tour", " · ",
        length(unique(d$sondage_id)), " sondages · ", fmt_date(input$period[1]), " — ", fmt_date(input$period[2])))
  })
  output$kpis <- renderUI({
    d <- filtered_data()
    metric <- function(label, value, detail) tags$div(class = "metric-card",
      tags$div(class = "metric-label", label), tags$div(class = "metric-value", value), tags$div(class = "metric-detail", detail))
    tags$div(class = "kpi-grid",
      metric("SONDAGES", length(unique(d$sondage_id)), paste(length(unique(d$institut)), "instituts dans la sélection")),
      metric("CONFIGURATIONS", length(unique(d$scenario_id)), "Configurations testées dans les enquêtes"),
      metric("CANDIDATS", length(unique(d$candidat_id)), paste(fmt_number(nrow(d)), "intentions de vote observées")),
      metric("DERNIER TERRAIN", if (nrow(d)) format(max(d$terrain_fin), "%d.%m.%Y") else "—", "Date de fin de collecte"))
  })
  output$overview_trend <- plotly::renderPlotly(trend_chart(filtered_data(), colors, 90, weighting(), FALSE))
  output$ranking_plot <- plotly::renderPlotly(ranking_chart(recent_data(), colors, weighting()))
  output$overview_insight <- renderUI({
    d <- recent_data()
    missing <- length(unique(d$sondage_id[is.na(d$sample_n)]))
    tags$div(class = "insight-box", tags$strong("Une photographie des hypothèses testées."),
      tags$p(paste0("La synthèse récente couvre les 90 jours se terminant le ", fmt_date(input$period[2]),
        " : ", length(unique(d$sondage_id)), " sondages et ", length(unique(d$scenario_id)),
        " configurations. Les candidats ne sont pas toujours testés ensemble : leurs moyennes ne constituent pas un bulletin de vote et ne s’additionnent pas à 100 %.")),
      if (weighting() == "sample" && missing > 0) tags$p(paste(missing, "sondage(s) sans effectif exploitable exclus des moyennes pondérées.")))
  })
  latest_polls <- reactive({
    d <- filtered_data()
    if (!nrow(d)) return(data.frame())
    rows <- lapply(split(d, d$sondage_id), function(x) data.frame(
      id = x$sondage_id[1], date = x$terrain_fin[1], Institut = x$institut[1],
      `Échantillon global` = x$echantillon[1], Configurations = length(unique(x$scenario_id)),
      `Candidats sélectionnés` = length(unique(x$candidat_id)), Notice = notice_link(x$url_source[1]), check.names = FALSE))
    d <- do.call(rbind, rows)
    d[order(d$date, decreasing = TRUE), , drop = FALSE]
  })
  output$latest_table <- DT::renderDT({
    d <- latest_polls()
    if (!nrow(d)) return(poll_table(data.frame(Message = "Aucun sondage pour cette sélection.")))
    d$`Fin du terrain` <- fmt_date(d$date)
    d <- d[, c("Fin du terrain", "Institut", "Échantillon global", "Configurations", "Candidats sélectionnés", "Notice")]
    poll_table(d, escape = setdiff(names(d), "Notice"), selection = "single")
  }, server = TRUE)
  observeEvent(input$latest_table_rows_selected, {
    d <- latest_polls(); index <- input$latest_table_rows_selected[1]
    req(length(index), index <= nrow(d))
    ids <- scenario_data()$scenario_id[scenario_data()$sondage_id == d$id[index]]
    req(length(ids))
    updateSelectizeInput(session, "scenario_id", selected = ids[1])
    bslib::nav_select("view", "scenarios", session = session)
  })
  output$trend_plot <- plotly::renderPlotly({
    window <- if (is.null(input$trend_window)) 90 else as.integer(input$trend_window)
    trend_chart(filtered_data(), colors, window, weighting(), isTRUE(input$show_points))
  })
  output$distribution_plot <- plotly::renderPlotly({
    d <- filtered_data()
    if (!nrow(d)) return(empty_chart())
    p <- plotly::plot_ly()
    for (id in unique(d$candidat_id)) {
      x <- d[d$candidat_id == id, , drop = FALSE]
      values <- vapply(split(x$score, x$sondage_id), mean, numeric(1))
      p <- plotly::add_trace(p, y = values, x = rep(x$candidat[1], length(values)),
        type = "box", name = x$candidat[1], boxpoints = "all", jitter = 0.25,
        marker = list(color = unname(colors[id]), size = 5), line = list(color = unname(colors[id])),
        hovertemplate = "%{x}<br>%{y:.1f} %<extra></extra>")
    }
    plotly::layout(chart_style(p), showlegend = FALSE)
  })
  candidate_summary <- reactive({
    e <- poll_candidate_estimates(filtered_data(), weighting())
    e <- e[order(e$score, decreasing = TRUE), , drop = FALSE]
    data.frame(Candidat = e$candidat, Parti = e$parti, `Moyenne (%)` = round(e$score, 2),
      Sondages = e$n_polls, `Minimum observé (%)` = e$min_score, `Maximum observé (%)` = e$max_score,
      `Dernier terrain` = fmt_date(e$last_date), check.names = FALSE)
  })
  output$candidate_table <- DT::renderDT(poll_table(candidate_summary(), 15), server = TRUE)
  observeEvent(scenario_data(), {
    choices <- scenario_choices(scenario_data()); current <- isolate(input$scenario_id)
    updateSelectizeInput(session, "scenario_id", choices = choices,
      selected = if (length(current) && current %in% choices) current else utils::head(unname(choices), 1), server = TRUE)
    comparison <- isolate(input$comparison_id)
    updateSelectizeInput(session, "comparison_id", choices = c("Aucune comparaison" = "", choices),
      selected = if (length(comparison) && comparison %in% choices) comparison else "", server = TRUE)
  })
  selected_scenario <- reactive({d <- scenario_data(); d[d$scenario_id %in% input$scenario_id, , drop = FALSE]})
  comparison_scenario <- reactive({d <- scenario_data(); d[d$scenario_id %in% input$comparison_id, , drop = FALSE]})
  output$scenario_plot <- plotly::renderPlotly({
    a <- selected_scenario(); b <- comparison_scenario()
    if (!nrow(a)) return(empty_chart("Sélectionnez une configuration dans la liste."))
    order_names <- a$candidat[order(a$score)]
    order_names <- c(setdiff(b$candidat, order_names), order_names)
    p <- plotly::plot_ly()
    for (k in seq_len(if (nrow(b)) 2L else 1L)) {
      d <- if (k == 1L) a else b
      margin <- 1.96 * sqrt((d$score / 100) * (1 - d$score / 100) / d$sample_n) * 100
      args <- list(p = p, x = d$score, y = d$candidat, type = "bar", orientation = "h",
        name = paste(d$institut[1], fmt_date(d$terrain_fin[1]), paste0("n°", d$configuration[1])),
        marker = list(color = if (nrow(b)) if (k == 1L) "#087F8C" else "#C1CCD7" else unname(colors[d$candidat_id])),
        text = paste0(fmt_number(d$score, 1), " %"), textposition = if (isTRUE(input$scenario_intervals)) "none" else "outside", cliponaxis = FALSE,
        hovertemplate = "%{y}<br>%{x:.1f} %<extra>%{fullData.name}</extra>")
      if (isTRUE(input$scenario_intervals)) args$error_x <- list(type = "data", array = pmin(margin, 100 - d$score),
        arrayminus = pmin(margin, d$score), symmetric = FALSE, color = "#526078", thickness = 1)
      p <- do.call(plotly::add_trace, args)
    }
    plotly::layout(chart_style(p, "", TRUE), barmode = "group",
      xaxis = list(title = "Intentions de vote (%)", range = c(0, min(100, max(c(a$score, b$score)) * 1.25))),
      yaxis = list(categoryorder = "array", categoryarray = order_names))
  })
  output$scenario_metadata <- renderUI({
    a <- selected_scenario(); b <- comparison_scenario()
    if (!nrow(a)) return(NULL)
    meta <- function(d, title) tags$div(class = "quality-card", tags$strong(title),
      tags$p(paste0(d$institut[1], " · terrain du ", fmt_date(d$terrain_debut[1]), " au ", fmt_date(d$terrain_fin[1]))),
      tags$p(paste0("Échantillon : ", if (is.na(d$sample_n[1])) "non renseigné" else fmt_number(d$sample_n[1]),
        if (d$sample_kind[1] == "configuration") " (configuration)" else if (d$sample_kind[1] == "global") " (global du sondage)" else "",
        " · Somme des scores : ", fmt_number(sum(d$score), 1), " %.")), HTML(notice_link(d$url_source[1])))
    tags$div(class = "scenario-meta", tags$div(class = "quality-grid", meta(a, "Configuration sélectionnée"), if (nrow(b)) meta(b, "Configuration comparée")),
      if (nrow(b)) tags$p(class = "method-note", if (identical(a$scenario_signature[1], b$scenario_signature[1]))
        "Même liste de candidats. La comparaison reste sensible au terrain et à l’institut."
        else "Listes de candidats différentes : les écarts décrivent des hypothèses distinctes, pas une évolution à candidatures constantes."))
  })
  output$scenario_table <- DT::renderDT({
    a <- selected_scenario(); b <- comparison_scenario()
    if (!nrow(a)) return(poll_table(data.frame(Message = "Aucune configuration sélectionnée.")))
    d <- data.frame(Candidat = a$candidat, Parti = a$parti, `Score A (%)` = a$score, check.names = FALSE)
    if (nrow(b)) {
      d <- merge(d, data.frame(Candidat = b$candidat, `Score B (%)` = b$score, check.names = FALSE), by = "Candidat", all = TRUE, sort = FALSE)
      d$`Écart A − B (points)` <- d$`Score A (%)` - d$`Score B (%)`
    }
    poll_table(d, 15)
  }, server = TRUE)
  output$scenario_history <- plotly::renderPlotly({
    a <- selected_scenario()
    if (!nrow(a)) return(empty_chart())
    d <- scenario_data(); d <- d[d$scenario_signature == a$scenario_signature[1], , drop = FALSE]
    if (length(unique(d$sondage_id)) < 2L) return(empty_chart("Une seule enquête disponible avec exactement ces candidatures."))
    trend_chart(d, colors, 1, weighting(), TRUE)
  })
  duels <- reactive(duel_observations(period_data()))
  observeEvent(duels(), {
    d <- duels()
    if (!nrow(d)) {updateSelectInput(session, "duel_pair", choices = character()); return()}
    d <- d[order(d$date, decreasing = TRUE), , drop = FALSE]; d <- d[!duplicated(d$pair_id), , drop = FALSE]
    current <- isolate(input$duel_pair)
    updateSelectInput(session, "duel_pair", choices = stats::setNames(d$pair_id, d$pair_label),
      selected = if (length(current) && current %in% d$pair_id) current else d$pair_id[1])
  })
  selected_duel <- reactive({d <- duels(); if (!nrow(d)) return(d); d[d$pair_id %in% input$duel_pair, , drop = FALSE]})
  output$duel_plot <- plotly::renderPlotly({
    d <- selected_duel()
    if (!nrow(d)) return(empty_chart("Aucun duel disponible sur cette période."))
    p <- plotly::plot_ly()
    for (side in c("a", "b")) {
      score <- d[[paste0(side, "_score")]]
      p <- plotly::add_trace(p, x = d$date, y = score, type = "scatter", mode = "lines+markers",
        name = d[[side]][1], line = list(color = unname(colors[d[[paste0(side, "_id")]][1]]), width = 3), marker = list(size = 8),
        text = paste0(safe_text(d$institut), " · ", fmt_date(d$date), "<br>", fmt_number(score, 1), " %"), hoverinfo = "text+name")
    }
    plotly::layout(chart_style(p), yaxis = list(range = c(0, 100)),
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 50, y1 = 50, line = list(color = "#99A5B5", dash = "dot"))))
  })
  output$duel_matrix <- plotly::renderPlotly({
    d <- duels()
    if (!nrow(d)) return(empty_chart("Aucune confrontation de second tour dans la sélection."))
    candidates <- sort(unique(c(d$a, d$b)))
    z <- matrix(NA_real_, length(candidates), length(candidates), dimnames = list(candidates, candidates))
    labels <- matrix("Non testé", nrow(z), ncol(z), dimnames = dimnames(z))
    for (pair in unique(d$pair_id)) {
      x <- d[d$pair_id == pair, , drop = FALSE]
      m <- do.call(rbind, lapply(split(x, x$sondage_id), function(q) c(a = mean(q$a_score), b = mean(q$b_score),
        n = if (all(is.na(q$sample_n))) NA_real_ else mean(q$sample_n, na.rm = TRUE))))
      w <- if (weighting() == "sample") m[, "n"] else rep(1, nrow(m))
      valid <- is.finite(w) & w > 0
      if (!any(valid)) next
      a <- x$a[1]; b <- x$b[1]
      z[a, b] <- stats::weighted.mean(m[valid, "a"], w[valid]); z[b, a] <- stats::weighted.mean(m[valid, "b"], w[valid])
      labels[a, b] <- paste0(a, " face à ", b, "<br>", fmt_number(z[a, b], 1), " % · ", sum(valid), " sondage(s)")
      labels[b, a] <- paste0(b, " face à ", a, "<br>", fmt_number(z[b, a], 1), " % · ", sum(valid), " sondage(s)")
    }
    if (!any(is.finite(z))) return(empty_chart("Aucun effectif exploitable pour cette pondération."))
    p <- plotly::plot_ly(x = candidates, y = candidates, z = z, text = labels, type = "heatmap",
      zmin = 25, zmax = 75, zmid = 50, colorscale = list(c(0, "#C46B76"), c(0.5, "#F3F5F6"), c(1, "#087F8C")),
      hovertemplate = "%{text}<extra></extra>", hoverongaps = FALSE, colorbar = list(title = "%", thickness = 12))
    plotly::layout(chart_style(p, "Candidat observé", TRUE), xaxis = list(title = "Adversaire"))
  })
  output$duel_table <- DT::renderDT({
    d <- selected_duel()
    if (!nrow(d)) return(poll_table(data.frame(Message = "Aucun duel sélectionné.")))
    d <- d[order(d$date, decreasing = TRUE), , drop = FALSE]
    tab <- data.frame(`Fin du terrain` = fmt_date(d$date), Institut = d$institut, `Candidat A` = d$a, `Score A (%)` = d$a_score,
      `Candidat B` = d$b, `Score B (%)` = d$b_score, `Écart A − B` = d$a_score - d$b_score,
      Échantillon = d$sample_n, Notice = notice_link(d$url_source), check.names = FALSE)
    poll_table(tab, escape = setdiff(names(tab), "Notice"))
  }, server = TRUE)
  output$institute_volume <- plotly::renderPlotly({
    d <- filtered_data()
    if (!nrow(d)) return(empty_chart())
    counts <- sort(table(unique(d[, c("sondage_id", "institut")])$institut))
    p <- plotly::plot_ly(x = as.integer(counts), y = names(counts), type = "bar", orientation = "h", marker = list(color = "#087F8C"),
      text = as.integer(counts), textposition = "outside", hovertemplate = "%{y}<br>%{x} sondage(s)<extra></extra>")
    plotly::layout(chart_style(p, "", TRUE), xaxis = list(title = "Nombre de sondages distincts", dtick = 1),
      yaxis = list(categoryorder = "array", categoryarray = names(counts)), showlegend = FALSE)
  })
  institute_estimates <- reactive({
    d <- filtered_data()
    if (!nrow(d)) return(data.frame())
    do.call(rbind, lapply(split(d, d$institut), function(x) {
      e <- poll_candidate_estimates(x, weighting()); e$institut <- rep(x$institut[1], nrow(e)); e
    }))
  })
  output$institute_heatmap <- plotly::renderPlotly({
    e <- institute_estimates(); ref <- poll_candidate_estimates(filtered_data(), weighting())
    if (!nrow(e) || !nrow(ref)) return(empty_chart())
    institutes <- sort(unique(e$institut)); candidates <- sort(unique(e$candidat))
    z <- matrix(NA_real_, length(institutes), length(candidates), dimnames = list(institutes, candidates))
    text <- matrix("", nrow(z), ncol(z), dimnames = dimnames(z))
    for (i in seq_len(nrow(e))) {
      delta <- e$score[i] - ref$score[match(e$candidat_id[i], ref$candidat_id)]
      z[e$institut[i], e$candidat[i]] <- delta
      text[e$institut[i], e$candidat[i]] <- paste0(safe_text(e$institut[i]), " · ", safe_text(e$candidat[i]),
        "<br>Moyenne : ", fmt_number(e$score[i], 1), " %<br>Écart à l’ensemble : ", fmt_number(delta, 1), " pt · ", e$n_polls[i], " sondage(s)")
    }
    bound <- max(1, abs(z), na.rm = TRUE)
    p <- plotly::plot_ly(x = candidates, y = institutes, z = z, text = text, type = "heatmap",
      zmin = -bound, zmax = bound, colorscale = list(c(0, "#C77682"), c(0.5, "#F7F8FA"), c(1, "#087F8C")),
      hoverongaps = FALSE, hovertemplate = "%{text}<extra></extra>", colorbar = list(title = "Points", thickness = 12))
    chart_style(p, "", TRUE)
  })
  output$institute_table <- DT::renderDT({
    e <- institute_estimates()
    if (!nrow(e)) return(poll_table(data.frame(Message = "Aucun institut pour cette sélection.")))
    poll_table(data.frame(Institut = e$institut, Candidat = e$candidat, `Moyenne (%)` = round(e$score, 2),
      Sondages = e$n_polls, `Dernier terrain` = fmt_date(e$last_date), check.names = FALSE), 15)
  }, server = TRUE)
  output$data_quality <- renderUI({
    d <- scenario_data(); sums <- tapply(d$score, d$scenario_id, sum)
    card <- function(value, label) tags$div(class = "quality-card", tags$strong(value), tags$p(label))
    tags$div(class = "quality-grid", card(fmt_number(nrow(filtered_data())), "lignes après tous les filtres"),
      card(length(unique(d$sondage_id[is.na(d$sample_n)])), "sondages avec effectif manquant"),
      card(sum(abs(sums - 100) > 1, na.rm = TRUE), "configurations dont la somme s’écarte de 100 % de plus de 1 point"),
      card(length(unique(d$sondage_id[is.na(d$url_source) | !nzchar(d$url_source)])), "sondages sans lien de notice"))
  })
  output$data_table <- DT::renderDT({
    d <- filtered_data(); d <- d[order(d$terrain_fin, decreasing = TRUE), , drop = FALSE]
    tab <- data.frame(Sondage = d$sondage_id, Institut = d$institut, `Début terrain` = fmt_date(d$terrain_debut),
      `Fin terrain` = fmt_date(d$terrain_fin), Tour = d$tour, Configuration = d$configuration,
      Principale = ifelse(is.na(d$configuration_principale), "Non renseigné", ifelse(d$configuration_principale == 1, "Oui", "Non")),
      Candidat = d$candidat, Parti = d$parti, `Score (%)` = d$score, `Échantillon utilisé` = d$sample_n,
      `Type d’échantillon` = c(configuration = "Configuration", global = "Global", missing = "Manquant")[d$sample_kind],
      Source = d$source, `Révision Wikipédia` = d$wikipedia_revid, Notice = notice_link(d$url_source), check.names = FALSE)
    poll_table(tab, 15, escape = setdiff(names(tab), "Notice"))
  }, server = TRUE)
  output$download_data <- downloadHandler(filename = function() paste0("sondages-2027-selection-", Sys.Date(), ".csv"),
    content = function(file) write_poll_csv(filtered_data(), file), contentType = "text/csv;charset=utf-8")
  output$download_all <- downloadHandler(filename = "sondages-2027-base-complete.csv",
    content = function(file) write_poll_csv(polls, file), contentType = "text/csv;charset=utf-8")
  output$download_summary <- downloadHandler(filename = "sondages-2027-synthese-candidats.csv",
    content = function(file) write_poll_csv(candidate_summary(), file), contentType = "text/csv;charset=utf-8")
}
