# Run from the project root: source("dev/generate_manifest.R")
# Re-run after changing app files, embedded data, or R packages.
# The active R version is recorded: use a version supported by Connect Cloud.
local({
  stopifnot(file.exists("app.R"), file.exists("DESCRIPTION"))
  r_minor <- strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L]
  local_library <- paste0("artifacts/r-library-", R.version$major, ".", r_minor)
  if (dir.exists(local_library)) {
    .libPaths(c(normalizePath(local_library), .libPaths()))
  }
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("Install rsconnect first: install.packages('rsconnect')", call. = FALSE)
  }
  repos <- getOption("repos")
  if (is.null(repos) || !length(repos) || any(repos == "@CRAN@")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  # Only runtime files: no local library, history, credentials, or raw sources.
  app_files <- c(
    "app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", ".Rbuildignore",
    list.files(c("R", "inst"), recursive = TRUE, full.names = TRUE)
  )
  rsconnect::writeManifest(
    appDir = ".", appFiles = app_files,
    appPrimaryDoc = "app.R", appMode = "shiny", quarto = FALSE
  )

  manifest <- jsonlite::read_json("manifest.json", simplifyVector = FALSE)
  stopifnot(
    identical(manifest$version, 1L),
    identical(manifest$metadata$appmode, "shiny"),
    !"sondages.pres" %in% names(manifest$packages),
    setequal(app_files, names(manifest$files)),
    all(vapply(manifest$packages, function(x) {
      !tolower(x$Source) %in% c("local", "unknown")
    }, logical(1))),
    all(vapply(manifest$files, function(x) is.character(x$checksum), logical(1)))
  )
  hashes <- tools::md5sum(names(manifest$files))
  recorded <- vapply(manifest$files, function(x) x$checksum, character(1))
  stopifnot(identical(unname(hashes), unname(recorded)))
  message("manifest.json generated and file checksums verified.")
})
