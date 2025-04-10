test_that("check_existing_item() works", {
  test <- check_existing_item(
    search_term = "Estonian National Museum",
    language = "en"
  )
  expect_true(inherits(test, "data.frame"))
  expect_equal(
    as.character(test$rowid),
    "wbi:Q1370397"
  )
})

test_that("check_existing_item() handles invalid inputs appropriately", {
  # Test for non-character search_term
  expect_error(
    check_existing_item(search_term = 123, language = "en"),
    "Invalid input in check_existing_item(): 'search_term' must be a non-empty character string.",
    fixed = TRUE
  )

  # Test for empty search_term
  expect_error(
    check_existing_item(search_term = "", language = "en"),
    "Invalid input in check_existing_item(): 'search_term' must be a non-empty character string.",
    fixed = TRUE
  )

  # Test for non-character language
  expect_error(
    check_existing_item(search_term = "Estonian National Museum", language = 123),
    "Invalid input in check_existing_item(): 'language' must be a non-empty character string.",
    fixed = TRUE
  )

  # Test for empty language
  expect_error(
    check_existing_item(search_term = "Estonian National Museum", language = ""),
    "Invalid input in check_existing_item(): 'language' must be a non-empty character string.",
    fixed = TRUE
  )

  # Test for invalid wikibase_api_url
  expect_error(
    check_existing_item(search_term = "Estonian National Museum", language = "en", wikibase_api_url = "invalid_url"),
    "Invalid input in check_existing_item(): 'wikibase_api_url' must be a valid URL string.",
    fixed = TRUE
  )
})
