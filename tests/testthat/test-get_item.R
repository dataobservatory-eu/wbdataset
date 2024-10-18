
test_df <- get_item(qid="Q42", language=c("en", "nl"))

test_that("get_item() works with single QID", {
  expect_equal(test_df$language, c("en", "nl"))
  expect_equal(test_df$label, c("Douglas Adams", "Douglas Adams"))
  expect_equal(test_df$qid, c("Q42", "Q42"))
})

test_df2 <- get_item(qid=c("Q228", "Q347"), language=c("en", "nl"))

test_that("get_item() works with multiple QIDs", {
  expect_equal(test_df2$language, c("en", "nl", "en", "nl"))
  expect_equal(test_df2$label, c("Andorra", "Andorra", "Liechtenstein", "Liechtenstein"))
  expect_equal(test_df2$qid, c("Q228", "Q228", "Q347", "Q347"))
})

test_df2
dataset::as_dataset(test_df2,
                    author=person("Jane Doe"),
                    title = "Small countries")
