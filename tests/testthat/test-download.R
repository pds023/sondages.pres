download_fixture <- function(path) {
  utils::write.csv(data.frame(
    sondage_id = "one", institut = "Institut", terrain_debut = "2026-10-01",
    terrain_fin = "2026-10-02", echantillon = 1000, tour = 1,
    configuration = 1, configuration_principale = 1,
    echantillon_configuration = NA, candidat_id = c("a", "b"),
    candidat = c("Alice", "Bob"), parti = c("A", "B"), score = c(40, 60),
    source = "Fixture", wikipedia_revid = "1", url_source = "https://example.org"
  ), path, row.names = FALSE, fileEncoding = "UTF-8")
}

testthat::test_that("a valid download replaces only the cache and preserves download settings", {
  seed <- tempfile(fileext = ".csv")
  download_fixture(seed)
  seed_hash <- unname(tools::md5sum(seed))
  cache <- tempfile("download-test-")
  state <- new_poll_download(seed, cache)
  testthat::expect_identical(state$path, seed)
  testthat::expect_null(state$last_success)
  testthat::expect_null(state$last_attempt)
  old_timeout <- getOption("timeout")
  testthat::local_mocked_bindings(download.file = function(url, destfile, method, mode, quiet) {
    testthat::expect_identical(url,
      "https://www.data.gouv.fr/api/1/datasets/r/6b97e100-6031-42e0-9c93-aeb9dd49ef0d")
    testthat::expect_identical(method, "libcurl")
    testthat::expect_identical(mode, "wb")
    testthat::expect_true(quiet)
    testthat::expect_equal(getOption("timeout"), 30)
    file.copy(seed, destfile)
    0L
  }, .package = "utils")
  now <- as.POSIXct("2026-10-26 10:00:00", tz = "UTC")
  testthat::expect_identical(refresh_poll_download(state, now), state)
  testthat::expect_equal(state$last_success, as.Date("2026-10-26"))
  testthat::expect_identical(state$last_attempt, now)
  testthat::expect_null(state$error)
  testthat::expect_true(file.exists(state$path))
  testthat::expect_identical(normalizePath(dirname(state$path)), normalizePath(cache))
  testthat::expect_equal(nrow(read_poll_data(state$path)), 2)
  testthat::expect_identical(unname(tools::md5sum(seed)), seed_hash)
  testthat::expect_identical(getOption("timeout"), old_timeout)
})

testthat::test_that("downloads run once per Paris calendar day and remove only superseded cache files", {
  seed <- tempfile(fileext = ".csv")
  download_fixture(seed)
  state <- new_poll_download(seed)
  calls <- 0L
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    calls <<- calls + 1L
    file.copy(seed, destfile)
    0L
  }, .package = "utils")
  refresh_poll_download(state, as.POSIXct("2026-07-01 21:59:00", tz = "UTC"))
  first <- state$path
  refresh_poll_download(state, as.POSIXct("2026-07-01 21:59:59", tz = "UTC"))
  testthat::expect_identical(calls, 1L)
  refresh_poll_download(state, as.POSIXct("2026-07-01 22:00:00", tz = "UTC"))
  testthat::expect_identical(calls, 2L)
  testthat::expect_equal(state$last_success, as.Date("2026-07-02"))
  testthat::expect_false(file.exists(first))
  testthat::expect_true(file.exists(seed))
  testthat::expect_true(file.exists(state$path))
})

testthat::test_that("network failures retain valid data and retry after an hour", {
  seed <- tempfile(fileext = ".csv")
  download_fixture(seed)
  state <- new_poll_download(seed)
  calls <- 0L
  fail <- TRUE
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    calls <<- calls + 1L
    if (fail) {
      writeLines("partial", destfile)
      stop("network unavailable")
    }
    file.copy(seed, destfile)
    0L
  }, .package = "utils")
  now <- as.POSIXct("2026-10-26 10:00:00", tz = "UTC")
  refresh_poll_download(state, now)
  testthat::expect_identical(state$path, seed)
  testthat::expect_null(state$last_success)
  testthat::expect_match(state$error, "network unavailable")
  testthat::expect_length(list.files(state$cache_dir), 0L)
  refresh_poll_download(state, now + 3599)
  testthat::expect_identical(calls, 1L)
  fail <- FALSE
  refresh_poll_download(state, now + 3600)
  testthat::expect_identical(calls, 2L)
  testthat::expect_null(state$error)
  good_path <- state$path
  good_date <- state$last_success
  fail <- TRUE
  refresh_poll_download(state, now + 86400)
  testthat::expect_identical(state$path, good_path)
  testthat::expect_identical(state$last_success, good_date)
  testthat::expect_true(file.exists(good_path))
  testthat::expect_equal(nrow(read_poll_data(good_path)), 2)
  testthat::expect_length(list.files(state$cache_dir), 1L)
})

testthat::test_that("invalid CSV and unsuccessful download status never replace the seed", {
  seed <- tempfile(fileext = ".csv")
  download_fixture(seed)
  state <- new_poll_download(seed)
  status <- 0L
  testthat::local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("<html>Service unavailable</html>", destfile)
    status
  }, .package = "utils")
  now <- as.POSIXct("2026-10-26 10:00:00", tz = "UTC")
  refresh_poll_download(state, now)
  testthat::expect_match(state$error, "Colonnes manquantes")
  testthat::expect_identical(state$path, seed)
  testthat::expect_null(state$last_success)
  testthat::expect_length(list.files(state$cache_dir), 0L)
  status <- 1L
  refresh_poll_download(state, now, force = TRUE)
  testthat::expect_match(state$error, "chargement")
  testthat::expect_identical(state$path, seed)
  testthat::expect_true(file.exists(seed))
  testthat::expect_length(list.files(state$cache_dir), 0L)
})
