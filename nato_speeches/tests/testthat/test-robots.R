test_that("robots.txt rules: agent groups, longest match, wildcards", {
  txt <- "User-agent: *\nDisallow: /private/\nAllow: /private/speeches/\nDisallow: /*.pdf$\n\nUser-agent: BadBot\nDisallow: /"
  r <- parse_robots(txt, "NatoSpeechCollector")
  expect_true(robots_path_allowed(r, "/news/speech-1"))
  expect_false(robots_path_allowed(r, "/private/x"))
  expect_true(robots_path_allowed(r, "/private/speeches/x"))
  expect_false(robots_path_allowed(r, "/docs/a.pdf"))
  expect_true(robots_path_allowed(r, "/docs/a.pdf?download=1"))
  expect_false(robots_path_allowed(parse_robots(txt, "BadBot"), "/anything"))
  expect_true(robots_path_allowed(parse_robots("", "x"), "/anything"))
})
