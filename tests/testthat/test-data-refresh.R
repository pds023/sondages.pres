refresh_poll_fixture <- function(include_new_poll = FALSE) {
  data <- data.frame(
    sondage_id = rep(c("october-1", "october-8"), each = 2),
    institut = rep(c("Alpha", "Beta"), each = 2),
    terrain_debut = rep(c("2026-10-01", "2026-10-08"), each = 2),
    terrain_fin = rep(c("2026-10-01", "2026-10-08"), each = 2),
    echantillon = 1000, tour = 1, configuration = 1, configuration_principale = 1,
    echantillon_configuration = NA, candidat_id = rep(c("a", "b"), 2),
    candidat = rep(c("Alice", "Bob"), 2), parti = rep(c("A", "B"), 2),
    score = c(40, 60, 42, 58), source = "Notice", wikipedia_revid = "1",
    url_source = "https://example.org/notice"
  )
  if (include_new_poll) {
    added <- data[c(1, 2, 1), ]
    added$sondage_id <- "october-15"
    added$institut <- "Gamma"
    added$terrain_debut <- added$terrain_fin <- "2026-10-15"
    added$candidat_id <- c("a", "b", "c")
    added$candidat <- c("Alice", "Bob", "Chloé")
    added$parti <- c("A", "B", "C")
    added$score <- c(20, 50, 30)
    data <- rbind(data, added)
  }
  data
}

write_refresh_fixture <- function(data, path) {
  utils::write.csv(data, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

refresh_initial_inputs <- function(session) {
  session$setInputs(tour = "1", period = as.Date(c("2026-10-01", "2026-10-08")),
    institutes = c("Alpha", "Beta"), candidates = c("a", "b"), weighting = "equal",
    trend_window = "90", show_points = TRUE, main_only = FALSE,
    scenario_id = "october-1::1::1", comparison_id = "", scenario_intervals = TRUE)
}

testthat::test_that("new polls update filter bounds, charts and downloaded data in an open session", {
  path <- tempfile(fileext = ".csv")
  old_options <- options(sondages.pres.data = path)
  on.exit({ options(old_options); unlink(path) })
  write_refresh_fixture(refresh_poll_fixture(), path)
  shiny::testServer(app_server, {
    messages <- list()
    session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
    refresh_initial_inputs(session)
    testthat::expect_equal(nrow(polls()), 4L)
    write_refresh_fixture(refresh_poll_fixture(TRUE), path)
    session$elapse(2100)
    testthat::expect_equal(nrow(polls()), 7L)
    testthat::expect_equal(date_limits(), as.Date(c("2026-10-01", "2026-10-15")))
    testthat::expect_equal(messages$period$value, list(start = "2026-10-01", end = "2026-10-15"))
    testthat::expect_equal(messages$period$max, "2026-10-15")
    testthat::expect_setequal(messages$institutes$value, c("Alpha", "Beta", "Gamma"))
    testthat::expect_setequal(messages$candidates$value, c("a", "b", "c"))
    # Mock sessions record updates; the browser normally applies these values.
    session$setInputs(period = as.Date(unlist(messages$period$value)),
      institutes = messages$institutes$value, candidates = messages$candidates$value)
    trend <- jsonlite::fromJSON(output$overview_trend, simplifyVector = FALSE)$x$data
    testthat::expect_true("Chloé" %in% vapply(trend, function(x) x$name, character(1)))
    dates <- unlist(lapply(trend, function(x) x$x))
    testthat::expect_true("2026-10-15" %in% dates)
    selected <- utils::read.csv2(output$download_data, fileEncoding = "UTF-8-BOM")
    complete <- utils::read.csv2(output$download_all, fileEncoding = "UTF-8-BOM")
    testthat::expect_equal(nrow(selected), 7L)
    testthat::expect_equal(nrow(complete), 7L)
    testthat::expect_true("october-15" %in% selected$sondage_id)
    testthat::expect_equal(selected$score[selected$candidat_id == "c"], 30)
    testthat::expect_match(output$data_status$html, "15/10/2026", fixed = TRUE)
  })
})

testthat::test_that("refreshing preserves custom dates and partially selected filters", {
  path <- tempfile(fileext = ".csv")
  old_options <- options(sondages.pres.data = path)
  on.exit({ options(old_options); unlink(path) })
  write_refresh_fixture(refresh_poll_fixture(), path)
  shiny::testServer(app_server, {
    messages <- list()
    session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
    refresh_initial_inputs(session)
    session$setInputs(period = as.Date(c("2026-10-02", "2026-10-07")),
      institutes = "Alpha", candidates = "a")
    write_refresh_fixture(refresh_poll_fixture(TRUE), path)
    session$elapse(2100)
    testthat::expect_equal(messages$period$value, list(start = "2026-10-02", end = "2026-10-07"))
    testthat::expect_equal(messages$institutes$value, "Alpha")
    testthat::expect_equal(messages$candidates$value, "a")
  })
})

testthat::test_that("a malformed save displays a warning and preserves the last valid view until recovery", {
  path <- tempfile(fileext = ".csv")
  old_options <- options(sondages.pres.data = path)
  on.exit({ options(old_options); unlink(path) })
  write_refresh_fixture(refresh_poll_fixture(), path)
  shiny::testServer(app_server, {
    refresh_initial_inputs(session)
    original <- polls()
    original_selection <- filtered_data()
    writeLines("incomplete-save", path)
    session$elapse(2100)
    testthat::expect_identical(polls(), original)
    testthat::expect_identical(filtered_data(), original_selection)
    testthat::expect_match(output$data_status$html, "alert-warning", fixed = TRUE)
    testthat::expect_match(output$data_status$html, "dernière version valide", fixed = TRUE)
    preserved <- utils::read.csv2(output$download_data, fileEncoding = "UTF-8-BOM")
    testthat::expect_equal(preserved$score, original_selection$score)
    corrected <- refresh_poll_fixture()
    corrected$score[1:2] <- c(45, 55)
    write_refresh_fixture(corrected, path)
    session$elapse(2100)
    testthat::expect_null(source$error())
    testthat::expect_false(grepl("alert-warning", output$data_status$html, fixed = TRUE))
    recovered <- utils::read.csv2(output$download_data, fileEncoding = "UTF-8-BOM")
    testthat::expect_equal(recovered$score[recovered$sondage_id == "october-1" & recovered$candidat_id == "a"], 45)
  })
})
