test_that("speech keywords work across languages and URL slugs", {
  s <- src_row()
  expect_true(is_speech(s, "Rede des Bundespräsidenten"))
  expect_true(is_speech(s, "Discours du Président de la République"))
  expect_true(is_speech(s, "Przemówienie Prezydenta RP"))
  expect_true(is_speech(s, "Ομιλία του Υπουργού"))
  expect_true(is_speech(s, NA, "https://x.tr/haber/cumhurbaskani-konusma-yapti-123"))
  expect_false(is_speech(s, "New frigates ordered for the navy", "https://x.org/news/new-frigates"))
  expect_false(is_speech(s, "Directed energy weapons", "https://x.org/news/prerede-thing")) # no partial-word hits
  expect_true(is_speech(src_row(filter = "all"), "Anything at all"))
})

test_that("speaker_regex separates office-holders on shared listings", {
  s <- src_row(speaker_regex = "Chief of Defence|CHOD")
  expect_true(speaker_matches(s, "Chief of Defence remarks on readiness", ""))
  expect_false(speaker_matches(s, "Minister's speech", "The Minister said..."))
  expect_true(speaker_matches(src_row(), "anything", ""))
})
