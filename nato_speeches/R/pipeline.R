#' Collect speeches for the selected countries and roles.
#'
#' @param countries ISO3 codes (NULL = all 32 members).
#' @param roles any of "hos", "mod", "mfa", "chod" (NULL = all).
#' @param since keep only speeches dated on/after this date ("YYYY-MM-DD").
#'   Undated speeches are kept and can be filtered later (`date` is NA).
#' @param max_pages cap on listing pages per source (overrides the registry).
#' @param max_articles cap on new article pages fetched per source.
#' @param include_js also try sources flagged `js_required` with the configured
#'   fetcher (only useful if `cfg$fetcher` renders JavaScript).
#' @return a per-source summary data.frame (also written to data/runs/).
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
  if (is.null(res) || isTRUE(res$blocked) || !identical(as.integer(res$status), 200L) || is.na(res$body)) {
    return("failed")
  }
  art <- tryCatch(extract_article(res$body, res$url %||% link$url, src), error = function(e) NULL)
  if (is.null(art) || is.na(art$text) || nchar(art$text) < 200) return("failed")
  if (is.na(art$date) && !is.na(link$listed_date)) {
    art$date <- parse_date_string(link$listed_date, art$language)
    art$date_source <- if (is.na(art$date)) NA_character_ else "listing"
  }
  if (!speaker_matches(src, art$title, art$text)) return("wrong_speaker")
  if (!is.null(since) && !is.na(art$date) && as.Date(art$date) < as.Date(since)) return("too_old")

  save_raw_html(cfg, link$url, res$body)
  save_speech(cfg, list(
    id = ns_hash(link$url), source_id = src$source_id, iso3 = src$iso3, country = src$country,
    role = src$role, office = src$office, url = link$url, title = art$title, date = art$date,
    date_source = art$date_source, language = art$language, text = art$text,
    n_words = length(strsplit(art$text, "\\s+")[[1]]), text_hash = ns_hash(art$text),
    pdf_links = I(art$pdf_links), retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")))
  "saved"
}

#' Health check for the registry: fetch page 1 of each listing and report how
#' many candidate links were found. Run this first, and after site redesigns.
check_sources <- function(countries = NULL, roles = NULL, ids = NULL,
                          cfg = ns_config(), sources = load_sources()) {
  sel <- select_sources(sources, countries, roles, ids)
  out <- do.call(rbind, lapply(seq_len(nrow(sel)), function(i) {
    src <- as.list(sel[i, ])
    res <- tryCatch(polite_get(src$listing_url, cfg), error = function(e) NULL)
    n <- NA_integer_; n_speech <- NA_integer_
    if (!is.null(res) && identical(as.integer(res$status), 200L) && !is.na(res$body)) {
      links <- tryCatch(switch(src$type,
        html = parse_listing_html(res$body, res$url %||% src$listing_url, src),
        rss = parse_listing_rss(res$body),
        govuk_api = parse_listing_govuk(res$body)), error = function(e) NULL)
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
