remote_poll_fixture <- function(scores = c(40, 60)) {
  data.frame(
    sondage_id = "remote-poll", institut = "Institut", terrain_debut = "2026-10-01",
    terrain_fin = "2026-10-02", echantillon = 1000, tour = 1,
    configuration = 1, configuration_principale = 1,
    echantillon_configuration = NA, candidat_id = c("a", "b"),
    candidat = c("Alice", "Bob"), parti = c("A", "B"), score = scores,
    source = "Fixture", wikipedia_revid = "1", url_source = "https://example.org"
  )
}

testthat::test_that("local mode and explicit files bypass remote downloads", {
  seed <- tempfile(fileext = ".csv")
  utils::write.csv(remote_poll_fixture(), seed, row.names = FALSE)
  old_options <- options(sondages.pres.data = NULL, sondages.pres.auto_download = FALSE)
  on.exit({ options(old_options); unlink(seed) })
  testthat::local_mocked_bindings(
    app_data_path = function() seed, .poll_download = new.env(parent = emptyenv())
  )
  testthat::local_mocked_bindings(download.file = function(...) {
    stop("Unexpected network access")
  }, .package = "utils")
  local <- app_poll_source()
  testthat::expect_identical(local$path(), seed)
  testthat::expect_null(local$status())
  options(sondages.pres.auto_download = TRUE, sondages.pres.data = seed)
  explicit <- app_poll_source()
  testthat::expect_identical(explicit$path(), seed)
  testthat::expect_null(explicit$status())
  testthat::expect_null(.poll_download$state)
})

testthat::test_that("UI and sessions share daily downloads and retain usable views on failures", {
  seed <- tempfile(fileext = ".csv")
  control <- new.env(parent = emptyenv())
  control$data <- remote_poll_fixture()
  control$fail <- FALSE
  utils::write.csv(control$data, seed, row.names = FALSE)
  original_seed_hash <- unname(tools::md5sum(seed))
  shared <- new.env(parent = emptyenv())
  calls <- 0L
  old_options <- options(sondages.pres.data = NULL, sondages.pres.auto_download = TRUE)
  on.exit({ options(old_options); unlink(seed) })
  testthat::local_mocked_bindings(app_data_path = function() seed, .poll_download = shared)
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    calls <<- calls + 1L
    if (control$fail) stop("Service unavailable")
    utils::write.csv(control$data, destfile, row.names = FALSE)
    0L
  }, .package = "utils")

  testthat::expect_no_error(app_ui(NULL))
  first <- app_poll_source()
  second <- app_poll_source()
  testthat::expect_identical(first$path(), second$path())
  testthat::expect_identical(calls, 1L)
  testthat::expect_equal(first$status()$last_success, as.Date(Sys.time(), tz = "Europe/Paris"))

  shiny::testServer(app_server, {
    session$setInputs(tour = "1", period = as.Date(c("2026-10-01", "2026-10-02")),
      institutes = "Institut", candidates = c("a", "b"), weighting = "equal",
      trend_window = "90", show_points = TRUE, main_only = FALSE,
      scenario_id = "remote-poll::1::1", comparison_id = "", scenario_intervals = TRUE)
    testthat::expect_identical(calls, 1L)
    testthat::expect_equal(sort(polls()$score), c(40, 60))
    testthat::expect_match(output$data_status$html, "Mise à jour quotidienne", fixed = TRUE)

    # Mark yesterday's check stale without changing Shiny's own test clock.
    shared$state$last_success <- as.Date(Sys.time(), tz = "Europe/Paris") - 1
    shared$state$last_attempt <- NULL
    old_path <- shared$state$path
    control$data <- remote_poll_fixture(c(45, 55))
    session$elapse(2100)
    testthat::expect_identical(calls, 2L)
    testthat::expect_false(identical(shared$state$path, old_path))
    testthat::expect_false(file.exists(old_path))
    testthat::expect_true(file.exists(shared$state$path))
    testthat::expect_null(source$error())
    testthat::expect_equal(sort(polls()$score), c(45, 55))
    ranking <- jsonlite::fromJSON(output$ranking_plot, simplifyVector = FALSE)$x$data
    testthat::expect_equal(sort(unlist(ranking[[1]]$x)), c(45, 55))
    exported <- utils::read.csv2(output$download_data, fileEncoding = "UTF-8-BOM")
    testthat::expect_equal(sort(exported$score), c(45, 55))
    testthat::expect_true(all(exported$sondage_id == "remote-poll"))

    last_good <- polls()
    last_path <- shared$state$path
    shared$state$last_success <- as.Date(Sys.time(), tz = "Europe/Paris") - 1
    shared$state$last_attempt <- NULL
    control$fail <- TRUE
    session$elapse(2100)
    testthat::expect_identical(calls, 3L)
    testthat::expect_identical(shared$state$path, last_path)
    testthat::expect_identical(polls(), last_good)
    testthat::expect_null(source$error())
    testthat::expect_match(source$status()$error, "Service unavailable")
    testthat::expect_match(output$data_status$html, "alert-warning", fixed = TRUE)
    testthat::expect_match(output$data_status$html, "Dernières données valides conservées", fixed = TRUE)
    preserved <- jsonlite::fromJSON(output$ranking_plot, simplifyVector = FALSE)$x$data
    testthat::expect_equal(preserved, ranking)
    exported <- utils::read.csv2(output$download_all, fileEncoding = "UTF-8-BOM")
    testthat::expect_equal(sort(exported$score), c(45, 55))
    session$elapse(2100)
    testthat::expect_identical(calls, 3L)

    control$fail <- FALSE
    control$data <- remote_poll_fixture(c(30, 70))
    shared$state$last_attempt <- Sys.time() - 3601
    session$elapse(2100)
    testthat::expect_identical(calls, 4L)
    testthat::expect_null(source$status()$error)
    testthat::expect_false(grepl("alert-warning", output$data_status$html, fixed = TRUE))
    testthat::expect_equal(sort(polls()$score), c(30, 70))
    testthat::expect_identical(unname(tools::md5sum(seed)), original_seed_hash)
  })
})
