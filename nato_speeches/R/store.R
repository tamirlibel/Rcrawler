# Storage layout under cfg$data_dir:
#   speeches.jsonl  one JSON record per collected speech (append-only)
#   visited.csv     every article URL already processed, with outcome, so
#                   re-runs only fetch new pages
#   raw/            gzipped HTML (or the PDF) of saved speeches, if cfg$cache_html
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

save_raw <- function(cfg, url, res) {
  if (!isTRUE(cfg$cache_html)) return(invisible())
  stem <- file.path(cfg$data_dir, "raw", ns_hash(url))
  if (!is.null(res$raw)) {
    writeBin(res$raw, paste0(stem, ".pdf"))
  } else if (!is.na(res$body)) {
    con <- gzfile(paste0(stem, ".html.gz"), "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(res$body, con, useBytes = TRUE)
  }
}

#' Read or export the collected corpus
#'
#' `read_speeches()` loads every collected speech into a data.frame with one
#' row per speech and these columns:
#' `id` (hash of the URL), `source_id`, `iso3`, `country`, `role`, `office`,
#' `url`, `title`, `date` (`YYYY-MM-DD`, may be `NA`), `date_source` (where the
#' date came from: `meta`, `jsonld`, `time`, `xpath`, `listing`, `text` or
#' `pdf_created`; `text` and `pdf_created` are the least reliable), `language`,
#' `format` (`html` or `pdf`), `text`, `n_words`, `text_hash` (for finding the
#' same text published twice), `pdf_links` (list column of PDFs linked from
#' the page) and `retrieved_at`.
#'
#' `export_speeches()` writes the same table to CSV (UTF-8; `pdf_links`
#' space-separated) or, if `path` ends in `.rds`, to RDS.
#'
#' @param cfg settings from [ns_config()]; only `data_dir` is used.
#' @param path output file, `.csv` or `.rds`.
#' @return a data.frame (invisibly for `export_speeches()`).
#' @export
#' @examples
#' \dontrun{
#' speeches <- read_speeches(ns_config(data_dir = "natospeech_data"))
#' table(speeches$country, speeches$role)
#' export_speeches("nato_speeches.csv", ns_config(data_dir = "natospeech_data"))
#' }
read_speeches <- function(cfg = ns_config()) {
  f <- file.path(cfg$data_dir, "speeches.jsonl")
  if (!file.exists(f)) return(data.frame())
  lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
  recs <- lapply(lines[nzchar(lines)], jsonlite::fromJSON, simplifyVector = FALSE)
  cols <- unique(unlist(lapply(recs, names)))
  df <- as.data.frame(lapply(stats::setNames(cols, cols), function(k) {
    v <- lapply(recs, `[[`, k)
    if (k == "pdf_links") I(lapply(v, function(x) as.character(unlist(x)))) else
      vapply(v, function(x) if (is.null(x)) NA_character_ else paste(unlist(x), collapse = " "), character(1))
  }), stringsAsFactors = FALSE)
  df$n_words <- as.integer(df$n_words)
  df
}

#' @rdname read_speeches
#' @export
export_speeches <- function(path, cfg = ns_config()) {
  df <- read_speeches(cfg)
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    saveRDS(df, path)
  } else {
    if (nrow(df)) df$pdf_links <- vapply(df$pdf_links, paste, character(1), collapse = " ")
    utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(df)
}
