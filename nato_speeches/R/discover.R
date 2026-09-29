NS_ASSET_RX <- "\\.(jpe?g|png|gif|svg|webp|css|js|ico|mp[34]|zip|pdf|docx?|xlsx?|pptx?)([?#].*)?$"
NS_NONARTICLE_RX <- paste0("(?i)(/tag/|/tags/|/category/|/search|/login|/contact|/sitemap|/privacy|",
                           "/cookie|/rss|/feed|/newsletter|mailto:|tel:|javascript:|/print/)")

#' Collect candidate speech URLs from all listing pages of a source.
#' Returns data.frame(url, link_text, listed_date).
discover_links <- function(source, cfg, max_pages = NULL) {
  pages <- min(source$max_pages, max_pages %||% source$max_pages)
  out <- data.frame(url = character(), link_text = character(), listed_date = character())
  for (p in seq_len(max(pages, 1))) {
    page_url <- listing_page_url(source, p)
    res <- polite_get(page_url, cfg)
    if (is.null(res) || isTRUE(res$blocked) || is.na(res$body) || !identical(as.integer(res$status), 200L)) {
      ns_log(source$source_id, ": listing page ", p, " unavailable (status ", res$status %||% "none", ")", level = "WARN")
      break
    }
    links <- switch(source$type,
      html = parse_listing_html(res$body, res$url %||% page_url, source),
      rss = parse_listing_rss(res$body),
      govuk_api = parse_listing_govuk(res$body))
    links <- links[!links$url %in% out$url, , drop = FALSE]
    if (!nrow(links)) break   # pagination exhausted or template wrong
    out <- rbind(out, links)
  }
  out
}

parse_listing_html <- function(html, base_url, source) {
  doc <- xml2::read_html(html)
  xml2::xml_remove(xml2::xml_find_all(doc, "//nav | //header | //footer | //*[@role='navigation' or @role='banner' or @role='contentinfo']"))
  a <- xml2::xml_find_all(doc, "//a[@href]")
  href <- trimws(xml2::xml_attr(a, "href"))
  text <- ns_squish(xml2::xml_text(a))
  url <- sub("#.*$", "", xml2::url_absolute(href, base_url))
  keep <- grepl("^https?://", url) & same_site(url, base_url) &
    !grepl(NS_ASSET_RX, url, ignore.case = TRUE) & !grepl(NS_NONARTICLE_RX, url, perl = TRUE) &
    sub("/$", "", url) != sub("/$", "", base_url)
  if (!ns_blank(source$link_regex)) {
    keep <- keep & grepl(source$link_regex, url, perl = TRUE)
  } else {
    keep <- keep & looks_like_article(url)
  }
  df <- data.frame(url = url[keep], link_text = text[keep], listed_date = NA_character_)
  # Prefer the longest anchor text when the same URL is linked several times.
  df <- df[order(-nchar(df$link_text)), , drop = FALSE]
  df[!duplicated(df$url), , drop = FALSE]
}

# Heuristic for article-like URLs when a source has no link_regex: a slug with
# several words, a numeric id, or a dated path.
looks_like_article <- function(url) {
  path <- url_path(url)
  segs <- strsplit(sub("^/", "", sub("\\?.*$", "", path)), "/")
  last <- vapply(segs, function(s) if (length(s)) s[length(s)] else "", character(1))
  slug <- lengths(regmatches(last, gregexpr("[-_]", last))) >= 3
  numeric_id <- grepl("(^|[-_/=])[0-9]{3,}([-_./]|$)", path) | grepl("[?&](id|nid|p|article)=[0-9]+", url)
  dated <- grepl("/20[0-9]{2}/[01]?[0-9]/", path)
  (slug | numeric_id | dated) & lengths(segs) >= 1
}

parse_listing_rss <- function(xml_txt) {
  doc <- xml2::read_xml(xml_txt)
  xml2::xml_ns_strip(doc)
  items <- xml2::xml_find_all(doc, "//item | //entry")
  link <- vapply(items, function(it) {
    l <- xml2::xml_text(xml2::xml_find_first(it, "./link"))
    if (is.na(l) || !nzchar(trimws(l))) l <- xml2::xml_attr(xml2::xml_find_first(it, "./link"), "href")
    trimws(l)
  }, character(1))
  data.frame(url = link,
             link_text = ns_squish(xml2::xml_text(xml2::xml_find_first(items, "./title"))),
             listed_date = xml2::xml_text(xml2::xml_find_first(items, "./pubDate | ./published | ./updated")))
}

parse_listing_govuk <- function(json_txt) {
  res <- jsonlite::fromJSON(json_txt, simplifyVector = TRUE)$results
  if (is.null(res) || !length(res) || !NROW(res)) return(data.frame(url = character(), link_text = character(), listed_date = character()))
  data.frame(url = paste0("https://www.gov.uk", res$link),
             link_text = res$title %||% NA_character_,
             listed_date = substr(res$public_timestamp %||% NA_character_, 1, 10))
}
