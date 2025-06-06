test_that("get_wikidata_item() works with single qid_on_wikidata", {
  skip_on_cran()
  skip_if_offline()

  test_df <- get_wikidata_item(qid_on_wikidata = "Q42",
                               language = c("en", "nl"))
  expect_equal(as.character(test_df$language), c("en", "nl"))
  expect_equal(as.character(test_df$label), c("Douglas Adams",
                                              "Douglas Adams"))
  expect_equal(as.character(test_df$qid_on_wikidata), c("Q42", "Q42"))
})

test_that("get_wikidata_item() throws an error for invalid QID", {
  expect_error(
    get_wikidata_item("invalid_qid", language="en"),
    "do not appear to look like QIDs"
  )
})

test_that("get_wikidata_item() throws an error for bad language input", {
  expect_error(
    get_wikidata_item("Q42", language = 123),
    "must be a non-empty character vector"
  )
})


test_that("get_wikidata_item() works with multiple qid_on_wikidata", {
  skip_on_cran()
  skip_if_offline()

  test_df2 <- get_wikidata_item(
    qid_on_wikidata = c("Q228", "Q347"),
    language = c("en", "nl")
  )
  expect_equal(as.character(test_df2$language),
               c("en", "nl", "en", "nl"))
  expect_equal(as.character(test_df2$label),
               c("Andorra", "Andorra", "Liechtenstein", "Liechtenstein"))
  expect_equal(as.character(test_df2$qid_on_wikidata),
               c("Q228", "Q228", "Q347", "Q347"))
  expect_equal(attr(test_df2$qid_on_wikidata, "label"),
               "qid_on_wikidata on https://www.wikidata.org/w/api.php")
  expect_equal(attr(test_df2$language, "label"),
               "Language of label and description")
  expect_equal(attr(test_df2$qid_on_wikidata, "namespace"),
               "https://www.wikidata.org/w/api.php")
})

test_that("get_wikidata_item() uses fallback when label is missing [mocked]", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Simulated API response that has English label but not "zz"
  fake_entity <- list(
    entities = list(
      Q42 = list(
        id = "Q42",
        labels = list(en = list(language = "en", value = "Douglas Adams")),
        descriptions = list(en = list(language = "en", value = "British author")),
        claims = list()
      )
    )
  )

  stub(get_single_item, "httr::POST", function(...) structure(list(), class = "response"))
  stub(get_single_item, "httr::content", function(...) fake_entity)
  stub(get_single_item, "is_response_success", function(...) TRUE)

  result <- get_wikidata_item("Q42",
                              language = "zz",
                              fallback_language = "en")
  expect_true(as.character(result$label)[1] == "Douglas Adams")
})


test_that("get_wikidata_item() gives error", {
  expect_error(get_wikidata_item(qid_on_wikidata = "ZZZ",
                                 language = "en"))
})
