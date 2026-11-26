# Build the 2017 and 2022 polling files used by the "Comparer" view.
# Run from the project root: source("dev/build_historique.R")
# Needs jsonlite and xml2, plus network access for the 2017 Wikipedia page.
# Outputs use the same schema as the 2027 file and are validated by read_poll_data().
local({
  stopifnot(file.exists("DESCRIPTION"), requireNamespace("jsonlite"), requireNamespace("xml2"))
  source("R/fct_data.R", local = TRUE, encoding = "UTF-8")
  columns <- c("sondage_id", "institut", "terrain_debut", "terrain_fin", "echantillon", "tour",
               "configuration", "configuration_principale", "echantillon_configuration",
               "candidat_id", "candidat", "parti", "score", "source", "wikipedia_revid", "url_source")

  slug <- function(x) gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT"))))
  # Same rule as the 2027 file: last name without particle ("le-pen", "villepin").
  candidate_id <- function(name) slug(sub("^(de|d') ", "", sub("^\\S+\\s+", "", name)))
  institute <- function(x) {
    x <- trimws(gsub("[«»\"]", "", x))
    x[grepl("ipsos", x, ignore.case = TRUE)] <- "Ipsos"
    x[grepl("^ifop", x, ignore.case = TRUE)] <- "Ifop"
    x[grepl("kantar|sofres", x, ignore.case = TRUE)] <- "Kantar"
    x[grepl("^harris", x, ignore.case = TRUE)] <- "Harris"
    x[grepl("^opinion ?way", x, ignore.case = TRUE)] <- "OpinionWay"
    x[x == "Atlasintel"] <- "AtlasIntel"
    x
  }
  write_polls <- function(df, path) {
    df <- df[order(df$terrain_fin, df$sondage_id, df$tour, df$configuration, -df$score), columns]
    utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    checked <- read_poll_data(path)
    message(path, " : ", nrow(checked), " lignes, ", length(unique(checked$sondage_id)), " sondages")
    for (w in attr(checked, "data_warnings")) message("  avertissement : ", w)
    shared <- tapply(df$candidat, df$candidat_id, function(x) length(unique(x)) > 1L)
    if (any(shared)) message("  identifiant partagé par plusieurs noms : ", paste(names(which(shared)), collapse = ", "))
  }

  # 2022 ------------------------------------------------------------------------
  # raw-data/presidentielle.json (format NSPPolls) : sondage > tours > hypothèses > candidats.
  parties <- c("Rassemblement national" = "RN", "France insoumise" = "LFI", "Parti socialiste" = "PS",
               "EE-LV" = "EELV", "LRM" = "LREM", "Les Républicains" = "LR", "Reconquête" = "REC",
               "Debout la France" = "DLF", "Parti communiste" = "PCF", "Lutte ouvrière" = "LO",
               "NPA" = "NPA", "Résistons" = "RES", "UPR" = "UPR", "UDI" = "UDI", "Les Patriotes" = "LP",
               "PCD" = "PCD", "Parti animaliste" = "PA", "Solidarité et progrès" = "S&P",
               "Modem" = "MoDem", "Agir" = "Agir", "« div. gauche »" = "DVG", "« ext. gauche »" = "EXG")
  polls <- jsonlite::read_json("raw-data/presidentielle.json")
  rows <- list()
  for (poll in polls) for (round in poll$tours) for (k in seq_along(round$hypotheses)) {
    h <- round$hypotheses[[k]]
    # Three hypotheses hold a single candidate (extraction leftovers): not a scenario.
    if (length(h$candidats) < 2L) next
    party <- vapply(h$candidats, function(c) if (length(c$parti)) c$parti[[1]] else "", "")
    name <- vapply(h$candidats, function(c) if (is.null(c$candidat)) NA_character_ else c$candidat, "")
    rows[[length(rows) + 1L]] <- data.frame(
      sondage_id = poll$id, institut = institute(poll$nom_institut),
      terrain_debut = poll$debut_enquete, terrain_fin = poll$fin_enquete,
      echantillon = poll$echantillon, tour = if (round$tour == "Premier tour") 1L else 2L,
      configuration = k, configuration_principale = NA,
      echantillon_configuration = if (isTRUE(h$sous_echantillon > 0)) h$sous_echantillon else NA,
      candidat = name, parti = unname(parties[party]),
      score = vapply(h$candidats, function(c) as.numeric(c$intentions), 0),
      source = "nsppolls", wikipedia_revid = NA, url_source = if (is.null(poll$lien)) NA else poll$lien
    )
  }
  y22 <- do.call(rbind, rows)
  # 33 unnamed candidacies (an undecided LR, ecologist or far-left nominee) keep their score.
  unnamed <- is.na(y22$candidat)
  y22$candidat[unnamed] <- paste0("Candidat non désigné (", y22$parti[unnamed], ")")
  y22$candidat <- sub("^Eric ", "Éric ", y22$candidat)
  y22$candidat_id <- ifelse(unnamed, paste0("non-designe-", slug(y22$parti)), candidate_id(y22$candidat))
  # One label per candidate: the most frequent known one (a few rows are empty or wrong).
  usual <- tapply(y22$parti, y22$candidat_id, function(p) c(names(which.max(table(p))), "DIV")[1L])
  usual[c("montebourg", "taubira")] <- "DVG"
  y22$parti <- unname(usual[y22$candidat_id])
  write_polls(y22, "inst/extdata/sondages-presidentielle-2022.csv")

  # 2017 ------------------------------------------------------------------------
  # Pinned revision: re-running the script always yields the same file.
  revid <- 241553082
  page <- tempfile(fileext = ".json")
  on.exit(unlink(page), add = TRUE)
  utils::download.file(
    paste0("https://fr.wikipedia.org/w/api.php?action=parse&format=json&formatversion=2",
           "&prop=text%7Crevid&oldid=", revid),
    page, mode = "wb", quiet = TRUE, headers = c("User-Agent" = "sondages.pres (dev/build_historique.R)")
  )
  html <- jsonlite::read_json(page)$parse$text
  doc <- xml2::read_html(gsub("<br\\s*/?>", " ", html))
  xml2::xml_remove(xml2::xml_find_all(doc, "//sup | //style"))
  squish <- function(x) trimws(gsub("[[:space:]  ]+", " ", x))

  # Expand rowspan/colspan into a plain text grid, keeping each cell's external link.
  table_grid <- function(table) {
    rows <- xml2::xml_find_all(table, "./tr | ./tbody/tr")
    text <- link <- matrix(NA_character_, length(rows), 80L)
    for (r in seq_along(rows)) {
      col <- 1L
      for (cell in xml2::xml_find_all(rows[[r]], "./th | ./td")) {
        while (!is.na(text[r, col])) col <- col + 1L
        span <- function(a) max(1L, suppressWarnings(as.integer(gsub("\\D", "", xml2::xml_attr(cell, a, default = "1")))), na.rm = TRUE)
        rr <- r:min(length(rows), r + span("rowspan") - 1L)
        cc <- col:(col + span("colspan") - 1L)
        text[rr, cc] <- squish(xml2::xml_text(cell))
        link[rr, cc] <- xml2::xml_attr(xml2::xml_find_first(cell, ".//a[contains(@class, 'external')]"), "href")
        col <- max(cc) + 1L
      }
    }
    used <- colSums(!is.na(text)) > 0
    list(text = text[, used, drop = FALSE], link = link[, used, drop = FALSE])
  }
  month_of <- function(word) {
    w <- iconv(word, "UTF-8", "ASCII//TRANSLIT")
    if (startsWith(w, "juin")) return(6L)
    if (startsWith(w, "juil")) return(7L)
    unname(c(jan = 1L, fev = 2L, mar = 3L, avr = 4L, mai = 5L, aou = 8L, sep = 9L,
             oct = 10L, nov = 11L, dec = 12L)[substr(w, 1L, 3L)])
  }
  # "du 17 au 19", "28 fév. - 2 mars", "14 et 15 décembre 2015", "le 20"...
  parse_dates <- function(text, year, month) {
    tokens <- regmatches(tolower(text), gregexpr("[0-9]+|[[:alpha:]]+", tolower(text)))[[1]]
    numbers <- suppressWarnings(as.integer(tokens))
    days <- numbers[!is.na(numbers) & numbers <= 31]
    years <- numbers[!is.na(numbers) & numbers > 1900]
    months <- unlist(lapply(tokens[is.na(numbers)], month_of))
    months <- months[!is.na(months)]
    if (!length(days)) return(as.Date(c(NA, NA)))
    end_month <- if (length(months)) months[length(months)] else month
    end_year <- if (length(years)) years[length(years)] else year
    start_month <- if (length(months) > 1L) months[1L] else end_month
    start_year <- if (length(years) > 1L) years[1L] else end_year - (start_month > end_month)
    as.Date(sprintf("%d-%02d-%02d", c(start_year, end_year), c(start_month, end_month), days[c(1L, length(days))]),
            optional = TRUE)
  }
  number <- function(x) {
    x <- sub(",", ".", gsub("[[:space:] %]", "", x), fixed = TRUE)
    # "<0,5 %" is published as a bound: keep the middle of the interval.
    ifelse(startsWith(x, "<"), suppressWarnings(as.numeric(substring(x, 2L))) / 2, suppressWarnings(as.numeric(x)))
  }
  not_candidate <- "^(Sondeur|Date|Échantillon|Abstention.*|Indécis.*|Autres candidats|Non exprimé|)$"

  rows <- list()
  tables <- xml2::xml_find_all(doc, "//table[contains(@class, 'wikitable')]")
  for (table in tables) {
    part <- xml2::xml_text(xml2::xml_find_first(table, "preceding::h2[1]"))
    heading <- xml2::xml_text(xml2::xml_find_first(table, "preceding::*[self::h2 or self::h3 or self::h4][1]"))
    tour <- unname(c("Sondages concernant le premier tour" = 1L, "Sondages concernant le second tour" = 2L)[part])
    if (is.na(tour) || heading == "Reports de voix") next
    caption <- xml2::xml_find_first(table, "./caption")
    stamp <- xml2::xml_attr(xml2::xml_find_first(caption, ".//time"), "datetime")
    year <- as.integer(regmatches(xml2::xml_text(caption), regexpr("[0-9]{4}", xml2::xml_text(caption))))
    year <- if (length(year)) year else 2017L
    month <- if (!is.na(stamp)) as.integer(substr(stamp, 6L, 7L)) else NA_integer_
    g <- table_grid(table)
    head <- which(g$text[, 1L] == "Sondeur")
    labels <- apply(g$text[head, , drop = FALSE], 2L, function(x) paste(unique(x[!is.na(x) & nzchar(x)]), collapse = " "))
    candidates <- which(!grepl(not_candidate, labels))
    for (r in setdiff(seq_len(nrow(g$text)), head)) {
      cells <- g$text[r, ]
      if (length(unique(cells)) < 4L || cells[1L] %in% c("Résultats", "1 tour")) next
      dates <- parse_dates(cells[2L], year, month)
      if (anyNA(dates)) { message("Date ignorée : ", cells[2L]); next }
      year <- as.integer(format(dates[2L], "%Y"))  # undated rows follow the previous one
      # "20 % Juppé" in Fillon's column: the score of a replacement candidate.
      substitute <- sub("^[^[:alpha:]]*", "", cells[candidates])
      score <- number(sub("[[:alpha:]].*$", "", cells[candidates]))
      keep <- !is.na(score)
      if (sum(keep) < 2L) next
      label <- labels[candidates][keep]
      rows[[length(rows) + 1L]] <- data.frame(
        institut = institute(cells[1L]), terrain_debut = dates[1L], terrain_fin = dates[2L],
        echantillon = number(sub("-.*", "", cells[3L])), tour = tour, row = length(rows),
        candidat = ifelse(nzchar(substitute[keep]), substitute[keep], sub("\\s*\\(.*$", "", label)),
        parti = ifelse(grepl("\\(", label), sub("^.*\\((.*)\\)$", "\\1", label), NA),
        score = score[keep], url_source = g$link[r, 1L]
      )
    }
  }
  y17 <- do.call(rbind, rows)
  short <- !grepl(" ", y17$candidat)
  full <- unique(y17$candidat[!short])
  y17$candidat[short] <- vapply(y17$candidat[short], function(n) full[endsWith(full, paste0(" ", n))][1L], "")
  stopifnot(!anyNA(y17$candidat))
  y17$candidat_id <- candidate_id(y17$candidat)
  y17$parti[y17$parti == "SP"] <- "S&P"
  # Second-round tables name no party: reuse the latest first-round label.
  y17 <- y17[order(y17$terrain_fin), ]
  latest <- tapply(y17$parti, y17$candidat_id, function(p) utils::tail(c("SE", p[!is.na(p)]), 1L))
  y17$parti[is.na(y17$parti)] <- latest[y17$candidat_id[is.na(y17$parti)]]
  # A poll is an institute, a fieldwork period and a sample; each table row is one configuration.
  key <- paste(y17$institut, y17$terrain_debut, y17$terrain_fin, y17$echantillon)
  y17$sondage_id <- paste(slug(y17$institut), y17$terrain_fin, sep = "-")
  clash <- tapply(key, y17$sondage_id, function(k) length(unique(k)) > 1L)
  extra <- y17$sondage_id %in% names(which(clash))
  y17$sondage_id[extra] <- paste(y17$sondage_id[extra], ifelse(is.na(y17$echantillon[extra]), "na", y17$echantillon[extra]), sep = "-")
  y17$configuration <- stats::ave(y17$row, y17$sondage_id, y17$tour, FUN = function(x) match(x, unique(x)))
  y17$configuration_principale <- NA
  y17$echantillon_configuration <- NA
  y17$source <- "wikipedia"
  y17$wikipedia_revid <- revid
  y17$terrain_debut <- format(y17$terrain_debut)
  y17$terrain_fin <- format(y17$terrain_fin)
  write_polls(y17, "inst/extdata/sondages-presidentielle-2017.csv")
})
