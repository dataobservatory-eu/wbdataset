test_that("resolve_from_session() prioritizes passed param over session", {
  s <- list(language = "fr")
  expect_equal(resolve_from_session("language", "en", s), "en")
  expect_equal(resolve_from_session("language", NULL, s), "fr")
  expect_null(resolve_from_session("language", NULL, list()))
})
