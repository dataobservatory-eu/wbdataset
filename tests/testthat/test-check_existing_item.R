test_that("check_existing_item() works", {
  test <- check_existing_item(
    search_term = "Estonian National Museum",
    language = "en"
  )
  expect_equal(
    as.character(test$rowid),
    "wbi:Q1370397"
  )
})
