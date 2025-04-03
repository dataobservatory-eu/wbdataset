test_that("check_existing_property() works", {
  test <- check_existing_property(search_term="instance of", language = "en")
  expect_equal(as.character(test$rowid),
               "wd:P31")
  expect_equal(test$datatype,
               "wikibase-item")
})



