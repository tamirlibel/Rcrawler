#' Collect speeches
#'
#' For each selected source: reads its listing pages, keeps links that look
#' like speeches, fetches pages not processed in earlier runs, extracts the
#' text, and appends each speech to `speeches.jsonl` in `cfg$data_dir`.
#' Re-running only fetches new pages, so it can be scheduled (e.g. daily).
#'
#' Every processed URL is logged in `visited.csv` with its outcome
#' (`saved`, `not_speech`, `wrong_speaker`, `too_old`). Failed fetches are not
#' logged, so they are retried on the next run.
#'
#' @param countries ISO3 codes, e.g. `c("FRA", "DEU")`. `NULL` for all 32
#'   members.
#' @param roles any of `"hos"` (head of state), `"mod"` (minister of defence),
#'   `"mfa"` (minister of foreign affairs), `"chod"` (chief of defence).
#'   `NULL` for all.
#' @param since keep only speeches dated on or after this date
#'   (`"YYYY-MM-DD"`). Speeches whose date cannot be found are kept, with
#'   `date = NA`.
#' @param max_pages maximum listing pages per source; overrides the
#'   registry's `max_pages` when smaller.
#' @param max_articles maximum new pages fetched per source in this run.
#' @param include_js also run sources flagged `js_required`. Only useful with
#'   a JavaScript-capable fetcher such as [chromote_fetcher()].
#' @param ids source ids, e.g. `"GBR_mod"`, as an alternative to
#'   `countries`/`roles`.
#' @param cfg settings from [ns_config()].
#' @param sources registry from [load_sources()].
#' @return a data.frame with one row per source: links `discovered`, `new`
#'   links, speeches `saved`, and links rejected as `not_speech`,
#'   `wrong_speaker`, `too_old` or `failed`, plus any `error`. It is also
#'   written to `cfg$data_dir/runs/`.
#' @export
#' @examples
#' \dontrun{
#' cfg <- ns_config(data_dir = "natospeech_data", contact = "me@university.edu")
#' collect_speeches(countries = c("FRA", "DEU"), roles = c("hos", "mfa"),
#'                  since = "2025-01-01", cfg = cfg)
#' speeches <- read_speeches(cfg)
#' }
collect_speeches <- function(countries = NULL, roles = NULL, since = NULL,
                             max_pages = NULL, max_articles = Inf,
                             include_js = FALSE, ids = NULL,
                             cfg = ns_config(), sources = load_sources()) {
  store_init(cfg)
  sel <- select_sources(sources, countries, roles, ids)
  if (!include_js && any(sel$js_required)) {
    ns_log("skipping JavaScript-rendered sources: ", paste(sel$source_id[sel$js_required], collapse = ", "),
           " (pass include_js = TRUE with a JS-capable fetcher)", level = "WARN")
    sel <- sel[!sel$js_required, , drop = FALSE]
  }
  summary <- do.call(rbind, lapply(seq_len(nrow(sel)), function(i) {
    src <- as.list(sel[i, ])
    tryCatch(collect_source(src, cfg, since, max_pages, max_articles),
             error = function(e) {
               ns_log(src$source_id, ": failed - ", conditionMessage(e), level = "ERROR")
               data.frame(source_id = src$source_id, discovered = NA_integer_, new = NA_integer_,
                          saved = 0L, not_speech = NA_integer_, wrong_speaker = NA_integer_,
                          too_old = NA_integer_, failed = NA_integer_, error = conditionMessage(e))
             })
  }))
  if (!is.null(summary) && nrow(summary)) {
    f <- file.path(cfg$data_dir, "runs", paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    utils::write.csv(summary, f, row.names = FALSE)
    ns_log("run summary written to ", f)
  }
  invisible(summary)
}

collect_source <- function(src, cfg, since = NULL, max_pages = NULL, max_articles = Inf) {
  ns_log(src$source_id, ": discovering from ", src$listing_url)
  links <- discover_links(src, cfg, max_pages)
  seen <- visited_urls(cfg)
  seen <- seen$url[seen$source_id == src$source_id]
  todo <- links[!links$url %in% seen, , drop = FALSE]

  # Cheap pre-filter on link text / URL before fetching anything.
  pre_ok <- vapply(seq_len(nrow(todo)), function(j) is_speech(src, NA, todo$url[j], todo$link_text[j]), logical(1))
  for (u in todo$url[!pre_ok]) mark_visited(cfg, src$source_id, u, "not_speech")
  todo <- todo[pre_ok, , drop = FALSE]
  # Skip pages the listing already dates before `since`, without fetching them.
  listed_old <- if (is.null(since)) rep(FALSE, nrow(todo)) else
    vapply(todo$listed_date, function(d) { d <- parse_date_string(d); !is.na(d) && as.Date(d) < as.Date(since) }, logical(1))
  for (u in todo$url[listed_old]) mark_visited(cfg, src$source_id, u, "too_old")
  todo <- utils::head(todo[!listed_old, , drop = FALSE], max_articles)

  counts <- c(saved = 0L, wrong_speaker = 0L, too_old = 0L, failed = 0L)
  for (j in seq_len(nrow(todo))) {
    outcome <- process_article(src, todo[j, ], cfg, since)
    counts[outcome] <- counts[outcome] + 1L
    # Only record final outcomes: a too-old page stays too old, but a failed
    # fetch should be retried next run.
    if (outcome != "failed") mark_visited(cfg, src$source_id, todo$url[j], outcome)
  }
  ns_log(src$source_id, ": ", nrow(links), " links, ", nrow(todo), " fetched, ", counts[["saved"]], " saved")
  data.frame(source_id = src$source_id, discovered = nrow(links), new = sum(!links$url %in% seen),
             saved = counts[["saved"]], not_speech = sum(!pre_ok), wrong_speaker = counts[["wrong_speaker"]],
             too_old = counts[["too_old"]] + sum(listed_old), failed = counts[["failed"]], error = NA_character_)
}

process_article <- function(src, link, cfg, since = NULL) {
  res <- polite_get(link$url, cfg)
  if (is.null(res) || isTRUE(res$blocked) || !identical(as.integer(res$status), 200L)) return("failed")
  is_pdf <- !is.null(res$raw) || grepl("pdf", res$content_type %||% "", ignore.case = TRUE)
  art <- tryCatch(
    if (is_pdf) {
      if (!isTRUE(cfg$pdf) || is.null(res$raw)) NULL else extract_pdf(res$raw, link$link_text, src$language)
    } else if (!is.na(res$body)) {
      extract_article(res$body, res$url %||% link$url, src)
    },
    error = function(e) {
      ns_log(src$source_id, ": extraction failed for ", link$url, " - ", conditionMessage(e), level = "WARN")
      NULL
    })
  if (is.null(art) || is.na(art$text) || nchar(art$text) < 200) return("failed")
  if (!is.na(link$listed_date) && (is.na(art$date) || art$date_source %in% c("text", "pdf_created"))) {
    listed <- parse_date_string(link$listed_date, art$language)
    if (!is.na(listed)) { art$date <- listed; art$date_source <- "listing" }
  }
  if (!speaker_matches(src, art$title, art$text)) return("wrong_speaker")
  if (!is.null(since) && !is.na(art$date) && as.Date(art$date) < as.Date(since)) return("too_old")

  save_raw(cfg, link$url, res)
  save_speech(cfg, list(
    id = ns_hash(link$url), source_id = src$source_id, iso3 = src$iso3, country = src$country,
    role = src$role, office = src$office, url = link$url, title = art$title, date = art$date,
    date_source = art$date_source, language = art$language, format = if (is_pdf) "pdf" else "html",
    text = art$text, n_words = length(strsplit(art$text, "\\s+")[[1]]), text_hash = ns_hash(art$text),
    pdf_links = I(art$pdf_links), retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")))
  "saved"
}

#' Check that registry sources work
#'
#' Fetches the first listing page of each selected source and reports whether
#' it loads and yields speech-like links. Run it before a first collection and
#' whenever a source stops producing speeches (sites get redesigned).
#'
#' Verdicts: `ok`; `no_speech_links` (links found, none look like speeches:
#' check `link_regex`/`filter`); `no_links` (page loads but no article links:
#' wrong URL, or `js_required`); `http_<code>`; `unreachable`;
#' `robots_disallowed`.
#'
#' @inheritParams collect_speeches
#' @return a data.frame with one row per source: `source_id`, `listing_url`,
#'   `final_url` (after redirects), `http_status`, `links`,
#'   `speech_like_links`, `js_required` and `verdict`.
#' @export
#' @examples
#' \dontrun{
#' report <- check_sources(cfg = ns_config(contact = "me@university.edu"))
#' table(report$verdict)
#' report[report$verdict != "ok", ]
#' }
check_sources <- function(countries = NULL, roles = NULL, ids = NULL,
                          cfg = ns_config(), sources = load_sources()) {
  sel <- select_sources(sources, countries, roles, ids)
  out <- do.call(rbind, lapply(seq_len(nrow(sel)), function(i) {
    src <- as.list(sel[i, ])
    res <- tryCatch(polite_get(src$listing_url, cfg), error = function(e) NULL)
    n <- NA_integer_; n_speech <- NA_integer_
    if (!is.null(res) && identical(as.integer(res$status), 200L) && !is.na(res$body)) {
      links <- tryCatch(parse_listing(src, res, src$listing_url, cfg), error = function(e) NULL)
      if (!is.null(links)) {
        n <- nrow(links)
        n_speech <- sum(vapply(seq_len(n), function(j) is_speech(src, NA, links$url[j], links$link_text[j]), logical(1)))
      }
    }
    verdict <- if (is.null(res)) "unreachable" else if (isTRUE(res$blocked)) "robots_disallowed" else
      if (!identical(as.integer(res$status), 200L)) paste0("http_", res$status) else
      if (is.na(n) || n == 0) "no_links" else if (n_speech == 0) "no_speech_links" else "ok"
    ns_log(sprintf("%-9s %-18s links=%s speech_like=%s", src$source_id, verdict, n, n_speech))
    data.frame(source_id = src$source_id, listing_url = src$listing_url, final_url = res$url %||% NA_character_,
               http_status = res$status %||% NA_integer_, links = n, speech_like_links = n_speech,
               js_required = src$js_required, verdict = verdict)
  }))
  invisible(out)
}
