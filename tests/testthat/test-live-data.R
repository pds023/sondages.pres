live_poll_fixture <- function() {
  data.frame(
    sondage_id = "test-2026", institut = "Institut", terrain_debut = "2026-10-01",
    terrain_fin = "2026-10-02", echantillon = 1000, tour = 1, configuration = 1,
    configuration_principale = 1, echantillon_configuration = NA,
    candidat_id = c("a", "b"), candidat = c("Alice", "Bob"), parti = c("A", "B"),
    score = c(40, 60), source = "Notice", wikipedia_revid = "1",
    url_source = "https://example.org/notice"
  )
}

write_live_poll_fixture <- function(data, path) {
  utils::write.csv(data, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

testthat::test_that("the source file overrides the bundled snapshot from a nested working directory", {
  root <- tempfile("poll-project-")
  dir.create(file.path(root, "inst", "extdata"), recursive = TRUE)
  dir.create(file.path(root, "raw-data"))
  dir.create(file.path(root, "tests", "testthat"), recursive = TRUE)
  old_dir <- getwd()
  old_options <- options(sondages.pres.data = NULL)
  on.exit({ setwd(old_dir); options(old_options); unlink(root, recursive = TRUE) })
  filename <- "sondages-presidentielle-2027.csv"
  raw <- file.path(root, "raw-data", filename)
  bundled <- file.path(root, "inst", "extdata", filename)
  data <- live_poll_fixture()
  write_live_poll_fixture(data, bundled)
  data$score <- c(45, 55)
  write_live_poll_fixture(data, raw)
  testthat::local_mocked_bindings(app_sys = function(...) file.path(root, "inst", ...))
  setwd(file.path(root, "tests", "testthat"))
  testthat::expect_equal(app_data_path(), normalizePath(raw, winslash = "/"))
  testthat::expect_equal(load_app_data()$score, c(55, 45))
  unlink(raw)
  testthat::expect_equal(app_data_path(), normalizePath(bundled, winslash = "/"))
  testthat::expect_equal(load_app_data()$score, c(60, 40))
})

testthat::test_that("an explicit data path wins and an invalid override never silently falls back", {
  path <- tempfile(fileext = ".csv")
  old_options <- options(sondages.pres.data = path)
  on.exit({ options(old_options); unlink(path) })
  write_live_poll_fixture(live_poll_fixture(), path)
  testthat::expect_equal(app_data_path(), normalizePath(path, winslash = "/"))
  testthat::expect_equal(nrow(load_app_data()), 2L)
  for (invalid in list("", NA_character_, c(path, path), paste0(path, ".missing"), dirname(path))) {
    options(sondages.pres.data = invalid)
    testthat::expect_error(app_data_path(), "sondages.pres.data")
  }
})

testthat::test_that("installed packages and a plain project use their available bundled file", {
  root <- tempfile("poll-bundle-")
  dir.create(file.path(root, "extdata"), recursive = TRUE)
  dir.create(file.path(root, "inst", "extdata"), recursive = TRUE)
  old_dir <- getwd()
  old_options <- options(sondages.pres.data = NULL)
  on.exit({ setwd(old_dir); options(old_options); unlink(root, recursive = TRUE) })
  filename <- "sondages-presidentielle-2027.csv"
  installed <- file.path(root, "extdata", filename)
  source <- file.path(root, "inst", "extdata", filename)
  write_live_poll_fixture(live_poll_fixture(), installed)
  write_live_poll_fixture(live_poll_fixture(), source)
  setwd(root)
  testthat::local_mocked_bindings(app_sys = function(...) file.path(root, ...))
  testthat::expect_equal(app_data_path(), normalizePath(installed, winslash = "/"))
  testthat::local_mocked_bindings(app_sys = function(...) "")
  testthat::expect_equal(app_data_path(), normalizePath(source, winslash = "/"))
  unlink(source)
  testthat::expect_error(app_data_path(), "introuvable")
})

testthat::test_that("polling detects equal-size replacements even when the timestamp is unchanged", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  data <- live_poll_fixture()
  write_live_poll_fixture(data, path)
  shiny::testServer(function(input, output, session) {
    watched <- watch_poll_data(path, session)
  }, {
    testthat::expect_null(watched$error())
    testthat::expect_equal(watched$data()$score, c(60, 40))
    before <- file.info(path)
    data$score <- c(45, 55)
    write_live_poll_fixture(data, path)
    Sys.setFileTime(path, before$mtime)
    testthat::expect_equal(file.info(path)$size, before$size)
    session$elapse(2000)
    testthat::expect_equal(watched$data()$score, c(55, 45))
    testthat::expect_null(watched$error())
  })
})

testthat::test_that("missing, empty and invalid writes preserve the last valid data and recover", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  data <- live_poll_fixture()
  write_live_poll_fixture(data, path)
  shiny::testServer(function(input, output, session) {
    watched <- watch_poll_data(path, session)
  }, {
    original <- watched$data()
    unlink(path)
    session$elapse(2000)
    testthat::expect_match(watched$error(), "introuvable")
    testthat::expect_identical(watched$data(), original)
    writeLines(character(), path)
    session$elapse(2000)
    testthat::expect_type(watched$error(), "character")
    testthat::expect_identical(watched$data(), original)
    write_live_poll_fixture(data[FALSE, ], path)
    session$elapse(2000)
    testthat::expect_match(watched$error(), "aucune observation")
    testthat::expect_identical(watched$data(), original)
    data$score[1] <- 101
    write_live_poll_fixture(data, path)
    session$elapse(2000)
    testthat::expect_match(watched$error(), "scores")
    testthat::expect_identical(watched$data(), original)
    data$score <- c(30, 70)
    write_live_poll_fixture(data, path)
    session$elapse(2000)
    testthat::expect_null(watched$error())
    testthat::expect_equal(watched$data()$score, c(70, 30))
  })
})

testthat::test_that("polling recovers when no valid version was initially available", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  shiny::testServer(function(input, output, session) {
    watched <- watch_poll_data(path, session)
  }, {
    testthat::expect_match(watched$error(), "introuvable")
    testthat::expect_error(watched$data(), "introuvable", class = "shiny.silent.error")
    write_live_poll_fixture(live_poll_fixture(), path)
    session$elapse(2000)
    testthat::expect_null(watched$error())
    testthat::expect_equal(watched$data()$score, c(60, 40))
  })
})

testthat::test_that("a file changed during reading never replaces the last valid snapshot", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  data <- live_poll_fixture()
  write_live_poll_fixture(data, path)
  reader <- read_poll_data
  control <- new.env(parent = emptyenv())
  control$change_during_read <- FALSE
  testthat::local_mocked_bindings(read_poll_data = function(path) {
    value <- reader(path)
    if (control$change_during_read) {
      control$change_during_read <- FALSE
      replacement <- live_poll_fixture()
      replacement$score <- c(30, 70)
      write_live_poll_fixture(replacement, path)
    }
    value
  })
  shiny::testServer(function(input, output, session) {
    watched <- watch_poll_data(path, session)
  }, {
    original <- watched$data()
    data$score <- c(45, 55)
    write_live_poll_fixture(data, path)
    control$change_during_read <- TRUE
    session$elapse(2000)
    testthat::expect_match(watched$error(), "pendant sa lecture", fixed = TRUE)
    testthat::expect_identical(watched$data(), original)
    session$elapse(2000)
    testthat::expect_null(watched$error())
    testthat::expect_equal(watched$data()$score, c(70, 30))
  })
})
