#' Crawler settings. Set `contact` to an email or URL so site operators can
#' reach you; it is added to the User-Agent string.
ns_config <- function(data_dir = ns_path("data"),
                      contact = Sys.getenv("NATOSPEECH_CONTACT", ""),
                      delay = 3, timeout = 30, retries = 3,
                      obey_robots = TRUE, cache_html = TRUE,
                      fetcher = NULL) {
  ua <- paste0("NatoSpeechCollector/0.1 (research; R/Rcrawler",
               if (nzchar(contact)) paste0("; contact: ", contact), ")")
  list(data_dir = data_dir, user_agent = ua, delay = delay, timeout = timeout,
       retries = retries, obey_robots = obey_robots, cache_html = cache_html,
       fetcher = fetcher %||% http_fetch)
}

# Per-session state: last request time per host and parsed robots.txt per host.
.ns_state <- new.env(parent = emptyenv())
.ns_state$last_hit <- list()
.ns_state$robots <- list()

#' Default fetcher: GET with retries/backoff. Returns
#' list(url, status, content_type, body) or NULL on network failure.
http_fetch <- function(url, cfg) {
  for (attempt in seq_len(cfg$retries)) {
    res <- tryCatch(
      httr::GET(url, httr::user_agent(cfg$user_agent), httr::timeout(cfg$timeout),
                httr::add_headers(`Accept-Language` = "en;q=0.8, *;q=0.5")),
      error = function(e) e)
    if (inherits(res, "error")) {
      ns_log("network error (attempt ", attempt, "): ", url, " - ", conditionMessage(res), level = "WARN")
    } else {
      status <- httr::status_code(res)
      if (!(status == 429 || status >= 500)) {
        type <- httr::headers(res)[["content-type"]] %||% ""
        body <- if (grepl("html|xml|json|text", type, ignore.case = TRUE) || !nzchar(type))
          httr::content(res, as = "text", encoding = guess_encoding(res)) else NA_character_
        return(list(url = res$url, status = status, content_type = type, body = body))
      }
      ns_log("HTTP ", status, " (attempt ", attempt, "): ", url, level = "WARN")
    }
    if (attempt < cfg$retries) Sys.sleep(cfg$delay * 2^attempt)
  }
  NULL
}

guess_encoding <- function(res) {
  type <- httr::headers(res)[["content-type"]] %||% ""
  enc <- regmatches(type, regexpr("(?<=charset=)[^;[:space:]]+", type, perl = TRUE))
  if (length(enc) && nzchar(enc)) return(toupper(enc))
  raw <- httr::content(res, as = "raw")
  head_txt <- rawToChar(raw[seq_len(min(length(raw), 4096))][raw[seq_len(min(length(raw), 4096))] != as.raw(0)])
  meta <- regmatches(head_txt, regexpr("(?i)charset=[\"']?[A-Za-z0-9_-]+", head_txt, perl = TRUE))
  if (length(meta)) return(toupper(sub("(?i)charset=[\"']?", "", meta, perl = TRUE)))
  "UTF-8"
}

#' Polite fetch: robots.txt check, per-host throttling, then the configured fetcher.
polite_get <- function(url, cfg) {
  host <- url_host(url)
  if (cfg$obey_robots && !robots_allowed(url, cfg)) {
    ns_log("disallowed by robots.txt: ", url, level = "WARN")
    return(list(url = url, status = NA_integer_, content_type = NA_character_,
                body = NA_character_, blocked = TRUE))
  }
  last <- .ns_state$last_hit[[host]]
  if (!is.null(last)) {
    wait <- cfg$delay - as.numeric(difftime(Sys.time(), last, units = "secs"))
    if (wait > 0) Sys.sleep(wait)
  }
  .ns_state$last_hit[[host]] <- Sys.time()
  cfg$fetcher(url, cfg)
}

# ---- robots.txt ---------------------------------------------------------

robots_allowed <- function(url, cfg) {
  host <- url_host(url)
  if (is.na(host)) return(FALSE)
  rules <- .ns_state$robots[[host]]
  if (is.null(rules)) {
    scheme <- sub("://.*$", "", url)
    robots_url <- paste0(scheme, "://", host, "/robots.txt")
    res <- cfg$fetcher(robots_url, cfg)
    txt <- if (!is.null(res) && identical(as.integer(res$status), 200L) && !is.na(res$body)) res$body else ""
    rules <- parse_robots(txt, agent = "NatoSpeechCollector")
    .ns_state$robots[[host]] <- rules
  }
  robots_path_allowed(rules, url_path(url))
}

#' Parse robots.txt into allow/disallow patterns for `agent` (falling back to `*`).
parse_robots <- function(txt, agent) {
  lines <- sub("#.*$", "", strsplit(txt, "\r?\n")[[1]] %||% character())
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  groups <- list(); current_agents <- character(); rules <- NULL; last_was_agent <- FALSE
  for (ln in lines) {
    key <- tolower(trimws(sub(":.*$", "", ln)))
    val <- trimws(sub("^[^:]*:", "", ln))
    if (key == "user-agent") {
      if (!last_was_agent) { current_agents <- character(); rules <- data.frame(type = character(), path = character()) }
      current_agents <- c(current_agents, tolower(val))
      last_was_agent <- TRUE
    } else if (key %in% c("allow", "disallow") && length(current_agents)) {
      last_was_agent <- FALSE
      for (a in current_agents) {
        groups[[a]] <- rbind(groups[[a]] %||% data.frame(type = character(), path = character()),
                             data.frame(type = key, path = val))
      }
    } else {
      last_was_agent <- FALSE
    }
  }
  specific <- groups[[tolower(agent)]]
  r <- specific %||% groups[["*"]] %||% data.frame(type = character(), path = character())
  r[nzchar(r$path), , drop = FALSE]
}

# Longest matching rule wins; Allow wins ties. Supports `*` and `$`.
robots_path_allowed <- function(rules, path) {
  if (!nrow(rules)) return(TRUE)
  glob_to_rx <- function(p) {
    anchored <- endsWith(p, "$")
    p <- sub("\\$$", "", p)
    parts <- strsplit(p, "*", fixed = TRUE)[[1]]
    if (endsWith(p, "*")) parts <- c(parts, "")
    esc <- gsub("([][{}()+?.^$|\\\\])", "\\\\\\1", parts, perl = TRUE)
    paste0("^", paste(esc, collapse = ".*"), if (anchored) "$")
  }
  hit <- vapply(rules$path, function(p) grepl(glob_to_rx(p), path, perl = TRUE), logical(1))
  if (!any(hit)) return(TRUE)
  cand <- rules[hit, , drop = FALSE]
  len <- nchar(cand$path)
  best <- cand[len == max(len), , drop = FALSE]
  any(best$type == "allow")
}
