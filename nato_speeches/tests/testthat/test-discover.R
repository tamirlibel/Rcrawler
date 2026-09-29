test_that("html listing yields same-site article links, deduplicated", {
  links <- parse_listing_html(fixture("listing.html"), "https://mod.example.org/news/speeches", src_row())
  expect_setequal(links$url, c(
    "https://mod.example.org/news/speeches/minister-speech-at-nato-summit-2025",
    "https://mod.example.org/news/speeches/chief-of-defence-remarks-on-readiness",
    "https://mod.example.org/news/press-releases/new-frigates-ordered-for-the-navy"))
  expect_equal(links$link_text[grepl("summit", links$url)], "Minister's speech at the NATO summit")
})

test_that("link_regex restricts candidates", {
  links <- parse_listing_html(fixture("listing.html"), "https://mod.example.org/news/speeches",
                              src_row(link_regex = "/speeches/"))
  expect_equal(nrow(links), 2)
})

test_that("GOV.UK search API results are parsed", {
  links <- parse_listing_govuk(fixture("govuk.json"))
  expect_equal(nrow(links), 2)
  expect_match(links$url[1], "^https://www.gov.uk/government/speeches/")
  expect_equal(links$listed_date[2], "2024-12-04")
})

test_that("RSS items are parsed", {
  rss <- '<?xml version="1.0"?><rss><channel><item><title>Rede des Bundespräsidenten</title><link>https://x.de/rede-1</link><pubDate>Tue, 03 Jun 2025 10:00:00 +0200</pubDate></item></channel></rss>'
  links <- parse_listing_rss(rss)
  expect_equal(links$url, "https://x.de/rede-1")
  expect_equal(parse_date_string(links$listed_date), "2025-06-03")
})

test_that("a listing without article links yields an empty data.frame", {
  links <- parse_listing_html("<html><body><main><a href='/'>Home</a></main></body></html>",
                              "https://x.org/news", src_row())
  expect_equal(nrow(links), 0)
  expect_named(links, c("url", "link_text", "listed_date"))
})
