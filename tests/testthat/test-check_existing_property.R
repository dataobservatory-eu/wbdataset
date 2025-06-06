test_that("check_existing_property() works", {
  skip_on_cran()
  skip_if_offline()  # skip if no internet

  test <- check_existing_property(search_term = "instance of",
                                  language = "en")
  expect_equal(
    as.character(test$rowid),
    "wd:P31"
  )
  expect_equal(
    test$datatype,
    "wikibase-item"
  )
})


test_that("check_existing_property() handles invalid inputs appropriately", {
  # Test for non-character search_term
  expect_error(
    check_existing_property(search_term = 123, language = "en"),
    "Invalid input in check_existing_property(): 'search_term' must be a non-empty character string.",
    fixed = TRUE
  )

  # Test for empty search_term
  expect_error(
    check_existing_property(search_term = "", language = "en"),
    "Invalid input in check_existing_property(): 'search_term' must be a non-empty character string.",
    fixed = TRUE
  )

  # Test for non-character language
  expect_error(
    check_existing_property(search_term = "Estonian National Museum", language = 123),
    "Invalid input in check_existing_property(): 'language' must be a non-empty character string.",
    fixed = TRUE
  )

  # Test for empty language
  expect_error(
    check_existing_property(search_term = "Estonian National Museum",
                            language = ""),
    "Invalid input in check_existing_property(): 'language' must be a non-empty character string.",
    fixed = TRUE
  )

  # Test for invalid wikibase_api_url
  expect_error(
    check_existing_property(search_term = "Estonian National Museum",
                            language = "en",
                            wikibase_api_url = "invalid_url"),
    "Invalid input in check_existing_property(): 'wikibase_api_url' must be a valid URL string.",
    fixed = TRUE
  )
})


test_that("check_existing_property() returns expected structure for known term", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Fake search result as returned by search_wikibase_entities()
  fake_result <- list(
    list(
      id = "P31",
      label = "instance of",
      description = "describes what kind of thing an item is",
      datatype = "wikibase-item"
    )
  )

  # Use the first result directly in ambiguity resolution
  fake_ambiguity_resolver <- function(search_results, ...) search_results[[1]]

  # Stub both dependencies
  stub(check_existing_property, "search_wikibase_entities", function(...) fake_result)
  stub(check_existing_property, "handle_search_term_ambiguity", fake_ambiguity_resolver)

  result <- check_existing_property(
    search_term = "instance of",
    language = "en"
  )

  expect_s3_class(result, "dataset_df")
  expect_equal(as.character(result$rowid), "wd:P31")
  expect_equal(as.character(result$label), "instance of")
  expect_equal(result$datatype, "wikibase-item")
  expect_false(result$success)
})

