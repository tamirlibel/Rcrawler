options(natospeech.quiet = TRUE)

fixture <- function(name) paste(readLines(test_path("fixtures", name), encoding = "UTF-8", warn = FALSE), collapse = "\n")

src_row <- function(...) {
  s <- list(source_id = "TST_mod", iso3 = "TST", country = "Testland", role = "mod",
            office = "Minister of Defence", language = "en", type = "html",
            listing_url = "https://mod.example.org/news/speeches", page_template = NA,
            max_pages = 1L, link_regex = NA, speaker_regex = NA, filter = "keywords",
            js_required = FALSE, status = "unverified", notes = NA,
            title_xpath = NA, date_xpath = NA, body_xpath = NA)
  modifyList(s, list(...))
}

# A fake website: named list of url -> body (a string, or a raw vector for
# PDFs). Unknown URLs return 404.
mock_fetcher <- function(pages) {
  force(pages)
  function(url, cfg) {
    body <- pages[[url]]
    if (is.null(body)) return(list(url = url, status = 404L, content_type = "text/html", body = ""))
    if (is.raw(body)) return(list(url = url, status = 200L, content_type = "application/pdf", body = NA_character_, raw = body))
    list(url = url, status = 200L, content_type = "text/html", body = body)
  }
}
