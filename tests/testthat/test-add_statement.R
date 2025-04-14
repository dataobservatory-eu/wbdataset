test_that("add_statement() fails for invalid wikibase_type", {
  expect_error(
    add_statement(
      qid = "Q1", pid = "P1", o = "some_value",
      wikibase_type = "banana",
      csrf = "dummy"
    )
  )
})

test_that("add_statement() fails gracefully on bad wikibase_type input", {
  expect_error(
    add_statement(
      qid = "Q1", pid = "P1", o = "oops",
      wikibase_type = NULL,
      csrf = "dummy"
    ),
    "must be a single character string"
  )
  expect_error(
    add_statement(
      qid = "Q1", pid = "P1", o = "oops",
      wikibase_type = "banana",
      csrf = "dummy"
    )
  )
})
