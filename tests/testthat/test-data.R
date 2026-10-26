poll_fixture <- function() {
  data.frame(
    sondage_id = c("one", "one", "one", "two", "two"),
    candidat_id = c("a", "a", "b", "a", "b"),
    candidat = c("Alice", "Alice", "Bob", "Alice", "Bob"),
    parti = c("A", "A", "B", "A", "B"), tour = 1,
    scenario_id = c("one::1::1", "one::1::2", "one::1::1", "two::1::1", "two::1::1"),
    score = c(20, 40, 80, 50, 50), sample_n = c(1000, 1000, 1000, 2000, 2000),
    terrain_fin = as.Date(c("2026-03-01", "2026-03-01", "2026-03-01", "2026-04-01", "2026-04-01"))
  )
}

testthat::test_that("alternative scenarios never multiply the weight of a study", {
  estimates <- poll_candidate_estimates(poll_fixture())
  alice <- estimates[estimates$candidat_id == "a", ]
  testthat::expect_equal(alice$score, 40)
  testthat::expect_equal(alice$n_polls, 2L)
  testthat::expect_equal(alice$min_score, 20)
  testthat::expect_equal(alice$max_score, 50)
  weighted <- poll_candidate_estimates(poll_fixture(), "sample")
  testthat::expect_equal(weighted$score[weighted$candidat_id == "a"], (30 + 50 * 2) / 3)
})

testthat::test_that("filters are respected and missing candidates are never zero-imputed", {
  x <- poll_fixture()
  x <- x[x$scenario_id != "one::1::1", ]
  estimates <- poll_candidate_estimates(x)
  testthat::expect_equal(estimates$score[estimates$candidat_id == "a"], 45)
  testthat::expect_equal(estimates$score[estimates$candidat_id == "b"], 50)
  testthat::expect_equal(estimates$n_polls[estimates$candidat_id == "b"], 1L)
  x$tour[1] <- 2
  testthat::expect_error(poll_candidate_estimates(x), "un seul tour")
})

testthat::test_that("sample weighting excludes unknown sample sizes", {
  x <- poll_fixture()
  x$sample_n[x$sondage_id == "two"] <- NA_real_
  estimates <- poll_candidate_estimates(x, "sample")
  testthat::expect_equal(estimates$score[estimates$candidat_id == "a"], 30)
  testthat::expect_true(all(estimates$n_polls == 1))
  x$sample_n[] <- NA_real_
  testthat::expect_equal(nrow(poll_candidate_estimates(x, "sample")), 0L)
})

testthat::test_that("trends use calendar windows without future leakage or extrapolation", {
  x <- poll_fixture()
  trend <- poll_trend(x, window = 32)
  alice <- trend[trend$candidat_id == "a", ]
  testthat::expect_equal(alice$score, c(30, 40))
  testthat::expect_equal(alice$date, as.Date(c("2026-03-01", "2026-04-01")))
  shorter <- poll_trend(x, window = 31)
  testthat::expect_equal(shorter$score[shorter$candidat_id == "a"], c(30, 50))
  x <- x[!(x$candidat_id == "b" & x$sondage_id == "two"), ]
  trend <- poll_trend(x)
  testthat::expect_equal(max(trend$date[trend$candidat_id == "b"]), as.Date("2026-03-01"))
  testthat::expect_error(poll_trend(x, window = 0), "strictement positif")
})

testthat::test_that("empty filters return typed zero-row tables", {
  empty <- poll_fixture()[FALSE, ]
  testthat::expect_equal(nrow(poll_candidate_estimates(empty)), 0L)
  trend <- poll_trend(empty)
  testthat::expect_equal(nrow(trend), 0L)
  testthat::expect_s3_class(trend$date, "Date")
  testthat::expect_s3_class(trend$last_date, "Date")
})

testthat::test_that("the provided snapshot loads without losing provenance or identities", {
  paths <- c(file.path("raw-data", "sondages-presidentielle-2027.csv"),
             file.path("..", "..", "raw-data", "sondages-presidentielle-2027.csv"),
             system.file("extdata", "sondages-presidentielle-2027.csv", package = "sondages.pres"))
  paths <- paths[file.exists(paths)]
  testthat::skip_if(!length(paths), "Source snapshot unavailable in this installation")
  x <- read_poll_data(paths[1L])
  original <- utils::read.csv(paths[1L], fileEncoding = "UTF-8-BOM",
                              colClasses = "character", na.strings = c("", "NA"))
  original_key <- with(original, paste(sondage_id, tour, configuration, candidat_id, sep = "::"))
  enriched_key <- paste(x$scenario_id, x$candidat_id, sep = "::")
  testthat::expect_true(validate_poll_data(x)$valid)
  testthat::expect_equal(nrow(x), nrow(original))
  testthat::expect_equal(anyDuplicated(enriched_key), 0L)
  testthat::expect_setequal(enriched_key, original_key)
  original <- original[match(enriched_key, original_key), ]
  for (column in c("candidat_id", "candidat", "source", "wikipedia_revid", "url_source")) {
    testthat::expect_equal(x[[column]], original[[column]], info = column)
  }
  testthat::expect_equal(x$score, as.numeric(gsub(",", ".", original$score, fixed = TRUE)))
  testthat::expect_equal(x$terrain_fin, as.Date(original$terrain_fin))
  testthat::expect_true(all(grepl("^https://", x$url_source)))
  config <- !is.na(x$echantillon_configuration)
  testthat::expect_true(all(x$sample_kind[config] == "configuration"))
  testthat::expect_true(all(x$sample_n[config] == x$echantillon_configuration[config]))
  global <- !config & !is.na(x$echantillon)
  testthat::expect_true(all(x$sample_kind[global] == "global"))
  testthat::expect_equal(x$sample_n[global], x$echantillon[global])
  missing <- !config & is.na(x$echantillon)
  testthat::expect_true(all(x$sample_kind[missing] == "missing"))
  testthat::expect_true(all(is.na(x$sample_n[missing])))
  ids <- split(x$candidat_id, x$scenario_id)
  signatures <- vapply(ids, function(z) paste(sort(z), collapse = "|"), character(1))
  testthat::expect_equal(unname(sub("^[12]::", "", x$scenario_signature)),
                         unname(signatures[x$scenario_id]))
  off_total <- any(abs(tapply(x$score, x$scenario_id, sum) - 100) > 1)
  testthat::expect_equal(any(grepl("scores publiés", attr(x, "data_warnings"))), off_total)
  empty_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(empty_csv))
  utils::write.csv(x[FALSE, ], empty_csv, row.names = FALSE, fileEncoding = "UTF-8")
  testthat::expect_error(read_poll_data(empty_csv), "aucune observation")
  duplicate <- rbind(x, x[1L, ])
  testthat::expect_false(validate_poll_data(duplicate)$valid)
  x$score[1L] <- 101
  testthat::expect_false(validate_poll_data(x)$valid)
})
