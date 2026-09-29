`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (is.atomic(a) && length(a) == 1 && is.na(a))) b else a

ns_blank <- function(x) is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))

ns_log <- function(..., level = "INFO") {
  if (isTRUE(getOption("natospeech.quiet"))) return(invisible())
  message(sprintf("[%s] %-5s %s", format(Sys.time(), "%H:%M:%S"), level, paste0(...)))
}

ns_root <- function() getOption("natospeech.root", default = ".")

ns_path <- function(...) file.path(ns_root(), ...)

ns_hash <- function(x) vapply(x, digest::digest, character(1), algo = "md5", serialize = FALSE, USE.NAMES = FALSE)

ns_squish <- function(x) trimws(gsub("[\\s\\x{00A0}]+", " ", enc2utf8(as.character(x)), perl = TRUE))

url_host <- function(url) {
  h <- sub("^[a-zA-Z][a-zA-Z0-9+.-]*://([^/?#:]+).*$", "\\1", url)
  h[!grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", url)] <- NA_character_
  tolower(h)
}

# Hosts are treated as the same site when they match after dropping "www.".
same_site <- function(url, base_url) {
  strip <- function(h) sub("^www\\.", "", h)
  h <- strip(url_host(url))
  !is.na(h) & h == strip(url_host(base_url))
}

url_path <- function(url) {
  p <- sub("^[a-zA-Z][a-zA-Z0-9+.-]*://[^/?#]+", "", url)
  p <- sub("#.*$", "", p)
  ifelse(nzchar(p), p, "/")
}
