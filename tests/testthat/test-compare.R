testthat::test_that("families follow party labels, with candidate overrides", {
  testthat::expect_equal(
    poll_family(c("le-pen", "macron", "macron", "asselineau", "hulot", "dupont-aignan", "philippe"),
                c("FN", "SE", "DVG", "UPR", "SE", "DLR", "HOR")),
    c("extreme-droite", "centre", "centre", "autres", "gauche", "extreme-droite", "centre"))
})

testthat::test_that("window averages count each institute once and respect the window", {
  studies <- data.frame(election = "2022", candidat_id = "a", institut = c("A", "A", "A", "B", "B"),
                        sondage_id = c("a1", "a2", "a3", "b1", "b2"), score = c(40, 40, 40, 20, 90),
                        jours = c(10L, 11L, 12L, 15L, 45L))
  avg <- compare_average(studies, c(0L, 20L, 60L), 30)
  testthat::expect_equal(avg$score[avg$jour == 0], 30)  # (40 + 20) / 2, not (3 × 40 + 20) / 4
  testthat::expect_equal(avg$n_instituts[avg$jour == 0], 2L)
  testthat::expect_equal(avg$n_sondages[avg$jour == 0], 4L)
  testthat::expect_equal(avg$score[avg$jour == 20], 90)
  testthat::expect_false(60L %in% avg$jour)
  testthat::expect_equal(nrow(compare_average(studies, integer(), 30)), 0L)
})

testthat::test_that("family scores add up candidates within each configuration", {
  polls <- data.frame(
    sondage_id = "p", institut = "I", terrain_fin = as.Date("2027-01-01"), tour = 1,
    scenario_id = rep(c("p::1::1", "p::1::2"), each = 3), candidat_id = c("le-pen", "zemmour", "attal", "bardella", "zemmour", "attal"),
    candidat = "x", parti = c("RN", "REC", "RE", "RN", "REC", "RE"), score = c(30, 10, 20, 34, 8, 18), sample_n = 1000
  )
  studies <- compare_studies(compare_data(polls, history = list()))
  testthat::expect_equal(studies$score[studies$candidat_id == "f:extreme-droite"], 41)  # mean of 40 and 42
  testthat::expect_equal(studies$score[studies$candidat_id == "f:centre"], 19)
  testthat::expect_equal(studies$jours[1], as.integer(as.Date("2027-04-18") - as.Date("2027-01-01")))
})

testthat::test_that("historical files reproduce the known final polling errors", {
  history <- history_polls()
  testthat::expect_equal(length(unique(history[["2022"]]$sondage_id)), 409L)
  testthat::expect_equal(length(unique(history[["2017"]]$sondage_id)), 203L)
  # "20 % Juppé" in Fillon's column becomes a Juppé score.
  testthat::expect_true("juppe" %in% history[["2017"]]$candidat_id[history[["2017"]]$terrain_fin > as.Date("2017-02-25")])
  errors <- compare_errors(compare_data(history[["2022"]][0, ]))
  gap <- function(e, t, id) errors$ecart[errors$election == e & errors$tour == t & errors$candidat_id == id]
  testthat::expect_setequal(errors$candidat_id[errors$election == "2022" & errors$tour == 1],
                            election_results$candidat_id[election_results$election == "2022" & election_results$tour == 1])
  testthat::expect_lt(mean(abs(errors$ecart[errors$election == "2017" & errors$tour == 1])), 1)
  testthat::expect_lt(gap("2022", 1, "melenchon"), -4)
  testthat::expect_gt(gap("2022", 1, "pecresse"), 2)
  testthat::expect_lt(gap("2017", 2, "macron"), -3)
  testthat::expect_lt(gap("2022", 2, "macron"), -2)
})

testthat::test_that("the comparison view renders every chart and table", {
  payload <- function(value) jsonlite::fromJSON(value, simplifyVector = FALSE)$x
  shiny::testServer(app_server, {
    session$setInputs(tour = "1", period = range(polls()$terrain_fin), institutes = unique(polls()$institut),
                      candidates = "le-pen", weighting = "equal", view = "history",
                      history_series = "f:extreme-droite", history_window = "60", history_round = "1")
    latest <- max(polls()$terrain_fin[polls()$tour == 1])
    testthat::expect_equal(history_stage(), as.integer(as.Date("2027-04-18") - latest))
    trend <- payload(output$history_trend)$data
    testthat::expect_setequal(vapply(trend, function(x) x$name, ""),
      c(paste("Campagne", c(2017, 2022, 2027)), paste("Résultat", c(2017, 2022))))
    campaign <- Filter(function(x) identical(x$name, "Campagne 2027"), trend)[[1]]
    testthat::expect_equal(max(unlist(campaign$x)), -history_stage())
    snapshot <- history_snapshot()
    testthat::expect_setequal(snapshot$election, c("2017", "2022", "2027"))
    testthat::expect_true(all(snapshot$jour == history_stage()))
    for (id in c("history_snapshot", "history_errors", "history_snapshot_table", "history_error_table")) {
      testthat::expect_type(payload(output[[id]]), "list")
    }
    testthat::expect_match(output$history_kpis$html, paste0("J-", history_stage()), fixed = TRUE)
    testthat::expect_match(output$history_error_note$html, "Mélenchon", fixed = TRUE)
    session$setInputs(history_series = "le-pen", history_round = "2")
    testthat::expect_length(Filter(function(x) startsWith(x$name, "Résultat"), payload(output$history_trend)$data), 2L)
    testthat::expect_length(payload(output$history_errors)$data, 6L)
  })
})
