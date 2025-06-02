test_that("Valid Wikibase datatypes return TRUE", {
  expect_true(is_valid_wikibase_datatype("wikibase-item"))
  expect_true(is_valid_wikibase_datatype("string"))
  expect_true(is_valid_wikibase_datatype("external-id"))
  expect_true(is_valid_wikibase_datatype("wikibase-form"))
})

test_that("Invalid Wikibase datatypes return FALSE", {
  expect_false(is_valid_wikibase_datatype("invalid-type"))
  expect_false(is_valid_wikibase_datatype("item"))  # common mistake
  expect_false(is_valid_wikibase_datatype("WIKIBASE-ITEM"))  # case-sensitive
  expect_false(is_valid_wikibase_datatype("monolingual"))  # incomplete
})

test_that("Non-character inputs are coerced correctly", {
  expect_false(is_valid_wikibase_datatype(123))   # not a string
  expect_false(is_valid_wikibase_datatype(NA))    # missing value
  expect_false(is_valid_wikibase_datatype(NULL))  # NULL value
})

test_that("Vectorized input works as expected", {
  input <- c("wikibase-item", "string", "invalid", "url")
  expected <- c(TRUE, TRUE, FALSE, TRUE)
  expect_equal(is_valid_wikibase_datatype(input), expected)
})

