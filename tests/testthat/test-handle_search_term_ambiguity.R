test_that("handle_search_term_ambiguity returns correct match", {
  search_results <- list(
    list(label = "Orange", match = list(language = "en"), id = "Q1"),
    list(label = "Orange", match = list(language = "fr"), id = "Q2")
  )

  result <- handle_search_term_ambiguity(search_results, "Orange", "en")
  expect_equal(result$id, "Q1")
})

test_that("handle_search_term_ambiguity returns NULL when no match", {
  search_results <- list(
    list(label = "Apple", match = list(language = "en"), id = "Q3")
  )

  result <- handle_search_term_ambiguity(search_results, "Orange", "en")
  expect_null(result)
})

test_that("handle_search_term_ambiguity returns first match when prefer_first is TRUE", {
  search_results <- list(
    list(label = "Orange", match = list(language = "en"), id = "Q1"),
    list(label = "Orange", match = list(language = "en"), id = "Q2")
  )

  result <- handle_search_term_ambiguity(search_results,
    "Orange", "en",
    strategy = "return_first"
  )
  expect_equal(result$id, "Q1")
})

test_that("handle_search_term_ambiguity returns NULL when multiple matches and prefer_first is FALSE", {
  search_results <- list(
    list(label = "Orange", match = list(language = "en"), id = "Q1"),
    list(label = "Orange", match = list(language = "en"), id = "Q2")
  )

  result <- handle_search_term_ambiguity(search_results, "Orange",
    "en",
    strategy = "return_null"
  )
  expect_null(result)
})
