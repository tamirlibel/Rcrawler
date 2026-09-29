test_that("registry covers all 32 NATO members and all four roles", {
  src <- load_sources()
  nato <- c("ALB","BEL","BGR","CAN","HRV","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","ISL","ITA","LVA",
            "LTU","LUX","MNE","NLD","MKD","NOR","POL","PRT","ROU","SVK","SVN","ESP","SWE","TUR","GBR","USA")
  expect_setequal(unique(src$iso3), nato)
  expect_equal(nrow(src), 32 * 4)
  expect_true(all(table(src$iso3, src$role) == 1))
  expect_true(all(grepl("^https?://", src$listing_url[src$type != "none"])))
  expect_setequal(src$source_id[src$type == "none"], c("ISL_mod", "ISL_chod"))
})

test_that("select_sources filters and drops 'none' rows", {
  src <- load_sources()
  expect_equal(nrow(select_sources(src, countries = "isl")), 2)
  expect_setequal(select_sources(src, countries = c("FRA", "DEU"), roles = "hos")$source_id, c("FRA_hos", "DEU_hos"))
})

test_that("listing_page_url builds page and offset URLs", {
  s <- src_row(listing_url = "https://x.org/news", page_template = "?page={page}")
  expect_equal(listing_page_url(s, 1), "https://x.org/news")
  expect_equal(listing_page_url(s, 3), "https://x.org/news?page=3")
  s2 <- src_row(listing_url = "https://x.org/api?filter=a&count=50", page_template = "&start={offset}")
  expect_equal(listing_page_url(s2, 3), "https://x.org/api?filter=a&count=50&start=100")
  s3 <- src_row(listing_url = "https://x.org/remarks/", page_template = "page/{page}/")
  expect_equal(listing_page_url(s3, 2), "https://x.org/remarks/page/2/")
})
