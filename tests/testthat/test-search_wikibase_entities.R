test_that("search_wikibase_entities returns expected results for valid input", {
  results <- search_wikibase_entities(
    search_term = "Estonian National Museum",
    language = "en",
    wikibase_api_url = "https://www.wikidata.org/w/api.php"
  )

  expect_type(results, "list")
  expect_true(length(results) > 0)
  expect_true(any(sapply(results, function(x) x$label == "Estonian National Museum")))
})

test_that("search_wikibase_entities handles no matches gracefully", {
  results <- search_wikibase_entities(
    search_term = "NonExistentEntity12345",
    language = "en",
    wikibase_api_url = "https://www.wikidata.org/w/api.php"
  )

  expect_type(results, "list")
  expect_length(results, 0)
})

test_that("search_wikibase_entities handles invalid API URL gracefully", {
  expect_error(
    search_wikibase_entities(
      search_term = "Estonian National Museum",
      language = "en",
      wikibase_api_url = "https://invalid.wikidata.org/w/api.php"
    ),
    regexp = "Could not resolve host|Failed to connect|Name or service not known",
    info = "Expected a network-related error due to invalid API URL"
  )
})
