# Keep the bundled/local snapshot until a complete, valid download is available.
.poll_download <- new.env(parent = emptyenv())

app_poll_source <- function() {
  seed <- app_data_path()
  if (!is.null(getOption("sondages.pres.data")) ||
      identical(getOption("sondages.pres.auto_download"), FALSE)) {
    return(list(path = function() seed, status = function() NULL))
  }
  if (is.null(.poll_download$state)) .poll_download$state <- new_poll_download(seed)
  state <- .poll_download$state
  list(
    path = function() {
      refresh_poll_download(state)
      state$path
    },
    status = function() list(last_success = state$last_success, error = state$error)
  )
}

new_poll_download <- function(seed_path, cache_dir = tempfile("sondages-pres-")) {
  state <- new.env(parent = emptyenv())
  state$path <- seed_path
  state$cache_dir <- cache_dir
  state$last_success <- NULL
  state$last_attempt <- NULL
  state$error <- NULL
  state$current_download <- NULL
  state
}

refresh_poll_download <- function(state, now = Sys.time(), force = FALSE) {
  today <- as.Date(now, tz = "Europe/Paris")
  if (!force) {
    if (identical(state$last_success, today)) return(invisible(state))
    if (!is.null(state$error) && !is.null(state$last_attempt) &&
        as.numeric(difftime(now, state$last_attempt, units = "secs")) < 3600) {
      return(invisible(state))
    }
  }

  state$last_attempt <- now
  candidate <- NULL
  old_options <- options(timeout = 30)
  on.exit(options(old_options), add = TRUE)
  on.exit({
    if (!is.null(candidate) && !identical(candidate, state$path)) unlink(candidate)
  }, add = TRUE)

  tryCatch({
    if (!dir.exists(state$cache_dir) &&
        !dir.create(state$cache_dir, recursive = TRUE, showWarnings = FALSE)) {
      stop("Impossible de cr\u00e9er le cache des sondages.", call. = FALSE)
    }
    candidate <- tempfile("sondages-", tmpdir = state$cache_dir, fileext = ".csv")
    status <- utils::download.file(
      "https://www.data.gouv.fr/api/1/datasets/r/6b97e100-6031-42e0-9c93-aeb9dd49ef0d",
      destfile = candidate, method = "libcurl", mode = "wb", quiet = TRUE
    )
    if (!identical(status, 0L) && !identical(status, 0)) {
      stop("Le t\u00e9l\u00e9chargement des sondages a \u00e9chou\u00e9.", call. = FALSE)
    }
    read_poll_data(candidate)
    previous <- state$current_download
    state$path <- candidate
    state$current_download <- candidate
    state$last_success <- today
    state$error <- NULL
    if (!is.null(previous)) unlink(previous)
  }, error = function(error) {
    state$error <- conditionMessage(error)
  })
  invisible(state)
}
