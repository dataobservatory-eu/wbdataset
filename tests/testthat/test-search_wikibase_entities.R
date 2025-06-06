test_that("search_wikibase_entities returns expected results for valid input", {
  results <- search_wikibase_entities(
    search_term = "Estonian National Museum",
    language = "en",
    type = "item",
    wikibase_api_url = "https://www.wikidata.org/w/api.php"
  )

  expect_type(results, "list")
  expect_true(length(results) > 0)
  expect_true(any(sapply(results, function(x) x$label == "Estonian National Museum")))
})

test_that("search_wikibase_entities() throws error for invalid type", {
  expect_error(
    search_wikibase_entities(
      search_term = "duration",
      language = "en",
      type = "invalid_type"
    ),
    "Invalid 'type' parameter. Must be either 'item' or 'property'."
  )
})

test_that("search_wikibase_entities handles no matches gracefully", {
  results <- search_wikibase_entities(
    search_term = "NonExistentEntity12345",
    language = "en",
    type = "item",
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
      type = "item",
      wikibase_api_url = "https://invalid.wikidata.org/w/api.php"
    ),
    regexp = "Could not resolve host|Failed to connect|Name or service not known",
    info = "Expected a network-related error due to invalid API URL"
  )
})

test_that("search_wikibase_entities() returns correct entity for 'duration' as item", {
  results <- search_wikibase_entities(
    search_term = "duration",
    language = "en",
    type = "item"
  )
  # Check that the returned entity is the 'duration' item (e.g., Q2199864)
  expect_true(any(sapply(results, function(x) x$id == "Q2199864")))
})

test_that("search_wikibase_entities() returns correct entity for 'duration' as property", {
  results <- search_wikibase_entities(
    search_term = "duration",
    language = "en",
    type = "property"
  )
  # Check that the returned entity is the 'duration' property (e.g., P2047)
  expect_true(any(sapply(results, function(x) x$id == "P2047")))
})

library(httptest2)

test_that("search_wikibase_entities returns results on success", {
  # Use with_mock_api to simulate a successful HTTP response
    skip_if_not_installed("httptest2")

  with_mock_api({
    result <- search_wikibase_entities(
      search_term = "instance of",
      language = "en",
      type = "property"
    )

    expect_type(result, "list")
    expect_true(length(result) > 0)
    expect_true("id" %in% names(result[[1]]))
    expect_true("label" %in% names(result[[1]]))
  })
})
