make_pdf <- function() {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 8.5, height = 11, title = "Address by the President")
  graphics::plot.new()
  body <- strwrap(paste(rep("We stand together with our allies to defend every inch of Alliance territory.", 5),
                        collapse = " "), 90)
  lines <- c("Address by the President to the Parliament", "Vilnius, 12 March 2025", body)
  for (i in seq_along(lines)) graphics::text(0, 1 - i * 0.05, lines[i], adj = 0, cex = 0.8)
  grDevices::dev.off()
  readBin(f, "raw", file.size(f))
}

test_that("extract_pdf gets text, date and title", {
  skip_if_not_installed("pdftools")
  a <- extract_pdf(make_pdf(), lang = "en")
  expect_match(a$text, "defend every inch of Alliance territory")
  expect_equal(a$date, "2025-03-12")
  expect_equal(a$date_source, "text")
  expect_equal(a$title, "Address by the President")
  expect_equal(extract_pdf(make_pdf(), title = "From link")$title, "From link")
})

test_that("PDF speeches linked from a listing are collected", {
  skip_if_not_installed("pdftools")
  site <- list(
    "https://president.example.lt/robots.txt" = "",
    "https://president.example.lt/speeches" = paste0(
      '<html><body><main><a href="/files/presidential-address-to-parliament.pdf">Presidential address to parliament</a>',
      '<a href="/files/annual-budget-report.pdf">Annual budget report</a></main></body></html>'),
    "https://president.example.lt/files/presidential-address-to-parliament.pdf" = make_pdf())
  cfg <- ns_config(data_dir = tempfile(), delay = 0, pdf = TRUE, fetcher = mock_fetcher(site))
  reset_crawler_state()
  srcs <- as.data.frame(src_row(source_id = "LTU_hos", listing_url = "https://president.example.lt/speeches"))
  srcs[] <- lapply(srcs, unlist)
  s <- collect_speeches(cfg = cfg, sources = srcs)
  expect_equal(s$saved, 1L)
  expect_equal(s$not_speech, 1L)
  df <- read_speeches(cfg)
  expect_equal(df$format, "pdf")
  expect_equal(df$title, "Presidential address to parliament")
  expect_true(file.exists(file.path(cfg$data_dir, "raw", paste0(df$id, ".pdf"))))

  # With pdf = FALSE, PDF links are ignored entirely.
  cfg2 <- ns_config(data_dir = tempfile(), delay = 0, pdf = FALSE, fetcher = mock_fetcher(site))
  expect_equal(collect_speeches(cfg = cfg2, sources = srcs)$discovered, 0L)
})
