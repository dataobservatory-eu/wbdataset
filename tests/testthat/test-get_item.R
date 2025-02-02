test_df <- get_item(qid = "Q42", language = c("en", "nl"))

test_that("get_item() works with single QID", {
  expect_equal(as.character(test_df$language), c("en", "nl"))
  expect_equal(as.character(test_df$label), c("Douglas Adams", "Douglas Adams"))
  expect_equal(as.character(test_df$qid), c("Q42", "Q42"))
})

test_df2 <- get_item(qid = c("Q228", "Q347"), language = c("en", "nl"))

test_that("get_item() works with multiple QIDs", {
  expect_equal(as.character(test_df2$language), c("en", "nl", "en", "nl"))
  expect_equal(as.character(test_df2$label), c("Andorra", "Andorra", "Liechtenstein", "Liechtenstein"))
  expect_equal(as.character(test_df2$qid), c("Q228", "Q228", "Q347", "Q347"))
  expect_equal(attr(test_df2$qid, "label"), "QID on https://www.wikidata.org/w/api.php")
  expect_equal(attr(test_df2$language, "label"), "Language of label and description")
  expect_equal(attr(test_df2$qid, "namespace"), "https://www.wikidata.org/w/api.php")
})


test_that("get_item() gives error", {
  expect_error(get_item(qid = "ZZZ", language = "en"))
})
