# Storage layout under cfg$data_dir:
#   speeches.jsonl  one JSON record per collected speech (append-only)
#   visited.csv     every article URL already processed, with outcome, so
#                   re-runs only fetch new pages
#   raw/            gzipped HTML of fetched speech pages (if cfg$cache_html)
#   runs/           per-run summary CSVs

store_init <- function(cfg) {
  dir.create(file.path(cfg$data_dir, "raw"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(cfg$data_dir, "runs"), recursive = TRUE, showWarnings = FALSE)
  invisible(cfg$data_dir)
}

visited_urls <- function(cfg) {
  f <- file.path(cfg$data_dir, "visited.csv")
  if (!file.exists(f)) return(data.frame(source_id = character(), url = character(), outcome = character()))
  utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")
}

mark_visited <- function(cfg, source_id, url, outcome) {
  f <- file.path(cfg$data_dir, "visited.csv")
  row <- data.frame(source_id = source_id, url = url, outcome = outcome,
                    visited_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  utils::write.table(row, f, sep = ",", row.names = FALSE, col.names = !file.exists(f),
                     append = file.exists(f), qmethod = "double", fileEncoding = "UTF-8")
}

save_speech <- function(cfg, record) {
  line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", na = "null")
  con <- file(file.path(cfg$data_dir, "speeches.jsonl"), open = "a", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(line, con, useBytes = TRUE)
}

save_raw_html <- function(cfg, url, html) {
  if (!isTRUE(cfg$cache_html) || is.na(html)) return(invisible())
  con <- gzfile(file.path(cfg$data_dir, "raw", paste0(ns_hash(url), ".html.gz")), "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(html, con, useBytes = TRUE)
}

#' Read the collected corpus into a data.frame.
read_speeches <- function(cfg = ns_config()) {
  f <- file.path(cfg$data_dir, "speeches.jsonl")
  if (!file.exists(f)) return(data.frame())
  lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
  recs <- lapply(lines[nzchar(lines)], jsonlite::fromJSON, simplifyVector = FALSE)
  cols <- unique(unlist(lapply(recs, names)))
  df <- as.data.frame(lapply(setNames(cols, cols), function(k) {
    v <- lapply(recs, `[[`, k)
    if (k == "pdf_links") I(lapply(v, unlist)) else vapply(v, function(x) if (is.null(x)) NA_character_ else paste(unlist(x), collapse = " "), character(1))
  }), stringsAsFactors = FALSE)
  df$n_words <- as.integer(df$n_words)
  df
}

#' Write the corpus to CSV (text included) or RDS.
export_speeches <- function(path, cfg = ns_config()) {
  df <- read_speeches(cfg)
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    saveRDS(df, path)
  } else {
    df$pdf_links <- vapply(df$pdf_links, paste, character(1), collapse = " ")
    utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(df)
}
