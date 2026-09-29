make_site <- function() {
  base <- "https://mod.example.org"
  pages <- list()
  pages[["https://mod.example.org/robots.txt"]] <- "User-agent: *\nDisallow: /news/press-releases/"
  pages[["https://mod.example.org/news/speeches"]] <- fixture("listing.html")
  pages[["https://mod.example.org/news/speeches/minister-speech-at-nato-summit-2025"]] <- fixture("speech.html")
  pages[["https://mod.example.org/news/speeches/chief-of-defence-remarks-on-readiness"]] <-
    sub("Minister's speech at the NATO summit", "Chief of Defence remarks on readiness",
        gsub("Speech by the Minister of Defence", "Remarks by the Chief of Defence", fixture("speech.html")), fixed = FALSE)
  pages
}

test_that("end-to-end collection with a mock site, incremental re-runs", {
  dir <- tempfile("ns_data")
  site <- make_site()
  calls <- character()
  fetch <- mock_fetcher(site)
  counting <- function(url, cfg) { calls <<- c(calls, url); fetch(url, cfg) }
  cfg <- ns_config(data_dir = dir, delay = 0, fetcher = counting)
  .ns_state$robots <- list(); .ns_state$last_hit <- list()

  srcs <- as.data.frame(rbind(
    src_row(source_id = "TST_mod", speaker_regex = "Minister of Defence"),
    src_row(source_id = "TST_chod", role = "chod", office = "Chief of Defence", speaker_regex = "Chief of Defence")))
  srcs[] <- lapply(srcs, unlist)

  s <- collect_speeches(cfg = cfg, sources = srcs)
  expect_equal(s$saved, c(1L, 1L))
  expect_equal(s$not_speech, c(1L, 1L))       # the frigates press release
  expect_equal(s$wrong_speaker, c(1L, 1L))

  df <- read_speeches(cfg)
  expect_equal(nrow(df), 2)
  expect_setequal(df$role, c("mod", "chod"))
  expect_equal(df$date[df$role == "mod"], "2025-06-24")
  expect_equal(unname(df$pdf_links[[1]]), "https://mod.example.org/files/speech-text-full.pdf")
  expect_true(all(file.exists(file.path(dir, "raw", paste0(ns_hash(df$url), ".html.gz")))))
  expect_equal(sum(calls == "https://mod.example.org/robots.txt"), 1)   # robots cached per host

  # Second run: nothing new is fetched beyond the listing pages.
  calls <- character()
  s2 <- collect_speeches(cfg = cfg, sources = srcs)
  expect_equal(s2$saved, c(0L, 0L))
  expect_equal(s2$new, c(0L, 0L))
  expect_setequal(unique(calls), "https://mod.example.org/news/speeches")

  out <- tempfile(fileext = ".csv")
  export_speeches(out, cfg)
  expect_equal(nrow(read.csv(out)), 2)
})

test_that("since filter and robots disallow are respected", {
  dir <- tempfile("ns_data")
  site <- make_site()
  site[["https://mod.example.org/robots.txt"]] <- "User-agent: *\nDisallow: /news/speeches/chief"
  cfg <- ns_config(data_dir = dir, delay = 0, fetcher = mock_fetcher(site))
  .ns_state$robots <- list(); .ns_state$last_hit <- list()
  srcs <- as.data.frame(src_row(source_id = "TST_all")); srcs[] <- lapply(srcs, unlist)
  s <- collect_speeches(cfg = cfg, sources = srcs, since = "2025-07-01")
  expect_equal(s$saved, 0L)
  expect_equal(s$too_old, 1L)
  expect_equal(s$failed, 1L)   # robots-blocked page
  v <- visited_urls(cfg)
  expect_false(any(grepl("chief", v$url)))   # failures are retried next run, not marked visited
})

test_that("check_sources reports verdicts", {
  cfg <- ns_config(data_dir = tempfile(), delay = 0, fetcher = mock_fetcher(make_site()))
  .ns_state$robots <- list(); .ns_state$last_hit <- list()
  srcs <- as.data.frame(rbind(src_row(source_id = "OK_1"),
                              src_row(source_id = "BAD_1", listing_url = "https://mod.example.org/gone")))
  srcs[] <- lapply(srcs, unlist)
  r <- check_sources(cfg = cfg, sources = srcs)
  expect_equal(r$verdict, c("ok", "http_404"))
  expect_equal(r$speech_like_links[1], 2L)
})
