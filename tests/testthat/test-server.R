widget_payload <- function(value) {
  testthat::expect_type(value, "character")
  result <- jsonlite::fromJSON(value, simplifyVector = FALSE)
  testthat::expect_type(result$x, "list")
  result$x
}

initial_poll_inputs <- function(session, data) {
  first_scenario <- data$scenario_id[which(data$tour == 1)[1L]]
  session$setInputs(
    tour = "1", period = range(data$terrain_fin), institutes = unique(data$institut),
    candidates = c("le-pen", "philippe", "bardella", "melenchon"),
    weighting = "equal", trend_window = "90", show_points = TRUE, main_only = FALSE,
    scenario_id = first_scenario, comparison_id = "", scenario_intervals = TRUE,
    duel_pair = "le-pen|melenchon"
  )
}

testthat::test_that("every view builds real Plotly and DT output for ordinary selections", {
  shiny::testServer(app_server, {
    initial_poll_inputs(session, polls)
    charts <- c("overview_trend", "ranking_plot", "trend_plot", "distribution_plot",
                "scenario_plot", "scenario_history", "duel_plot", "duel_matrix",
                "institute_volume", "institute_heatmap")
    tables <- c("latest_table", "candidate_table", "scenario_table", "duel_table",
                "institute_table", "data_table")
    for (id in c(charts, tables)) {
      testthat::expect_no_error(widget_payload(output[[id]]))
    }
    for (id in c("scope_label", "kpis", "overview_insight", "scenario_metadata", "data_quality")) {
      testthat::expect_true(nzchar(output[[id]]$html), info = id)
    }
    trend <- widget_payload(output$overview_trend)
    bardella <- Filter(function(trace) identical(trace$name, "Jordan Bardella"), trend$data)
    testthat::expect_length(bardella, 1L)
    testthat::expect_equal(max(as.Date(unlist(bardella[[1]]$x))), as.Date("2026-06-24"))
    ranking <- widget_payload(output$ranking_plot)
    testthat::expect_false("Jordan Bardella" %in% unlist(ranking$data[[1]]$y))
    matrix <- widget_payload(output$duel_matrix)$data[[1L]]$z
    testthat::expect_true(length(matrix) > 1L)
    testthat::expect_true(all(vapply(seq_along(matrix), function(i) is.null(matrix[[i]][[i]]), logical(1))))
    testthat::expect_true(all(candidate_summary()$`Moyenne (%)` > 0))
    session$setInputs(candidates = c("le-pen", "candidat-non-teste"))
    testthat::expect_equal(candidate_summary()$Candidat, "Marine Le Pen")
    testthat::expect_equal(length(widget_payload(output$ranking_plot)$data[[1]]$y), 1L)
  })
})

testthat::test_that("empty periods, institutes and candidates remain usable", {
  shiny::testServer(app_server, {
    initial_poll_inputs(session, polls)
    charts <- c("overview_trend", "ranking_plot", "trend_plot", "distribution_plot",
                "scenario_plot", "scenario_history", "duel_plot", "duel_matrix",
                "institute_volume", "institute_heatmap")
    tables <- c("latest_table", "candidate_table", "scenario_table", "duel_table",
                "institute_table", "data_table")
    empty_period <- as.Date(c("2025-01-01", "2025-01-31"))
    session$setInputs(period = empty_period)
    testthat::expect_equal(nrow(filtered_data()), 0L)
    testthat::expect_equal(nrow(candidate_summary()), 0L)
    for (id in c(charts, tables)) testthat::expect_no_error(widget_payload(output[[id]]))
    testthat::expect_true(nzchar(output$data_quality$html))
    session$setInputs(period = range(polls$terrain_fin), institutes = character())
    testthat::expect_equal(nrow(period_data()), 0L)
    for (id in c(charts, tables)) testthat::expect_no_error(widget_payload(output[[id]]))
    session$setInputs(institutes = unique(polls$institut), candidates = character())
    testthat::expect_equal(nrow(filtered_data()), 0L)
    testthat::expect_equal(nrow(candidate_summary()), 0L)
    testthat::expect_gt(nrow(scenario_data()), 0L)
    testthat::expect_gt(nrow(duels()), 0L)
    for (id in c(charts, tables)) testthat::expect_no_error(widget_payload(output[[id]]))
  })
})

testthat::test_that("sample weighting handles a study with no sample without inventing scores", {
  shiny::testServer(app_server, {
    initial_poll_inputs(session, polls)
    missing_scenario <- polls$scenario_id[which(polls$sondage_id == "ifop-2026-07-08" & polls$tour == 1)[1]]
    session$setInputs(period = as.Date(c("2026-07-08", "2026-07-08")), institutes = "Ifop",
                      weighting = "sample", show_points = FALSE, scenario_id = missing_scenario)
    testthat::expect_gt(nrow(filtered_data()), 0L)
    testthat::expect_true(all(is.na(filtered_data()$sample_n)))
    testthat::expect_equal(nrow(candidate_summary()), 0L)
    testthat::expect_equal(nrow(institute_estimates()), 0L)
    for (id in c("overview_trend", "ranking_plot", "trend_plot", "institute_heatmap",
                 "candidate_table", "institute_table", "data_table", "duel_matrix", "scenario_plot")) {
      testthat::expect_no_error(widget_payload(output[[id]]))
    }
    testthat::expect_match(output$scenario_metadata$html, "non renseigné", fixed = TRUE)
    testthat::expect_match(output$overview_insight$html, "exclus des moyennes pondérées", fixed = TRUE)
    session$setInputs(show_points = TRUE)
    points <- widget_payload(output$trend_plot)$data
    testthat::expect_true(all(vapply(points, function(trace) identical(trace$mode, "markers"), logical(1))))
    session$setInputs(weighting = "equal")
    testthat::expect_true(all(candidate_summary()$Sondages == 1L))
    testthat::expect_true(all(is.finite(candidate_summary()$`Moyenne (%)`)))
  })
})

testthat::test_that("scenario comparison preserves absent candidates and exact-lineup history", {
  shiny::testServer(app_server, {
    initial_poll_inputs(session, polls)
    meta <- scenario_data()[!duplicated(scenario_data()$scenario_id), ]
    first <- meta$scenario_id[1]
    other <- meta$scenario_id[meta$sondage_id == meta$sondage_id[1] &
                               meta$scenario_signature != meta$scenario_signature[1]][1]
    testthat::expect_false(is.na(other))
    session$setInputs(scenario_id = first, comparison_id = other)
    a <- selected_scenario()
    b <- comparison_scenario()
    testthat::expect_gt(nrow(a), length(input$candidates))
    testthat::expect_true(length(setdiff(a$candidat_id, b$candidat_id)) > 0L ||
                            length(setdiff(b$candidat_id, a$candidat_id)) > 0L)
    comparison <- widget_payload(output$scenario_plot)$data
    testthat::expect_length(comparison, 2L)
    testthat::expect_equal(sort(unlist(comparison[[1]]$y)), sort(a$candidat))
    testthat::expect_equal(sort(unlist(comparison[[2]]$y)), sort(b$candidat))
    testthat::expect_equal(sort(unlist(comparison[[1]]$x)), sort(a$score))
    testthat::expect_equal(sort(unlist(comparison[[2]]$x)), sort(b$score))
    testthat::expect_no_error(widget_payload(output$scenario_table))
    testthat::expect_match(output$scenario_metadata$html, "Listes de candidats différentes", fixed = TRUE)
    frequency <- vapply(split(meta$sondage_id, meta$scenario_signature), function(x) length(unique(x)), integer(1))
    signature <- names(which.max(frequency))
    comparable <- meta[meta$scenario_signature == signature, ]
    session$setInputs(scenario_id = comparable$scenario_id[1], comparison_id = tail(comparable$scenario_id, 1))
    testthat::expect_match(output$scenario_metadata$html, "Même liste de candidats", fixed = TRUE)
    history <- widget_payload(output$scenario_history)$data
    expected <- scenario_data()[scenario_data()$scenario_signature == signature, ]
    tested_dates <- unique(as.character(expected$terrain_fin))
    testthat::expect_gt(length(tested_dates), 1L)
    for (trace in history) {
      testthat::expect_true(all(unlist(trace$x) %in% tested_dates))
      testthat::expect_true(trace$name %in% expected$candidat)
    }
    session$setInputs(comparison_id = "", main_only = TRUE)
    testthat::expect_true(all(scenario_data()$configuration_principale == 1))
    testthat::expect_no_error(widget_payload(output$scenario_plot))
    session$setInputs(tour = "2", main_only = FALSE, candidates = c("le-pen", "melenchon"))
    testthat::expect_true(all(filtered_data()$tour == 2))
    for (id in c("trend_plot", "duel_plot", "duel_matrix", "duel_table")) {
      testthat::expect_no_error(widget_payload(output[[id]]))
    }
  })
})

testthat::test_that("downloads are UTF-8 BOM CSVs containing the actual filtered observations", {
  shiny::testServer(app_server, {
    initial_poll_inputs(session, polls)
    session$setInputs(institutes = "Harris", candidates = c("philippe", "le-pen"),
                      period = as.Date(c("2026-09-01", "2026-10-08")))
    path <- output$download_data
    testthat::expect_true(file.exists(path))
    testthat::expect_equal(readBin(path, "raw", n = 3), as.raw(c(239, 187, 191)))
    downloaded <- utils::read.csv2(path, fileEncoding = "UTF-8-BOM", check.names = FALSE)
    testthat::expect_equal(nrow(downloaded), nrow(filtered_data()))
    testthat::expect_true(all(downloaded$institut == "Harris"))
    testthat::expect_setequal(downloaded$candidat_id, c("le-pen", "philippe"))
    testthat::expect_true("Édouard Philippe" %in% downloaded$candidat)
    testthat::expect_equal(downloaded$score, filtered_data()$score)
    testthat::expect_equal(downloaded$scenario_id, filtered_data()$scenario_id)
    testthat::expect_equal(downloaded$url_source, filtered_data()$url_source)
    summary <- utils::read.csv2(output$download_summary, fileEncoding = "UTF-8-BOM", check.names = FALSE)
    testthat::expect_equal(summary, candidate_summary(), ignore_attr = TRUE)
    complete <- utils::read.csv2(output$download_all, fileEncoding = "UTF-8-BOM", check.names = FALSE)
    testthat::expect_equal(nrow(complete), nrow(polls))
    session$setInputs(candidates = character())
    empty <- utils::read.csv2(output$download_data, fileEncoding = "UTF-8-BOM", check.names = FALSE)
    testthat::expect_equal(nrow(empty), 0L)
    testthat::expect_true(all(c("sondage_id", "scenario_id", "score", "url_source") %in% names(empty)))
  })
})
