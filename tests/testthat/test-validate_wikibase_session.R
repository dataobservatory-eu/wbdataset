test_that("validate_wikibase_session() passes on NULL (allowed)", {
  expect_silent(validate_wikibase_session(NULL))
})

test_that("validate_wikibase_session() fails on non-list input", {
  expect_error(
    validate_wikibase_session("not a list"),
    "must be a named list"
  )
})

test_that("validate_wikibase_session() fails with unknown fields", {
  bad_session <- list(language = "en", foo = "bar")
  expect_error(
    validate_wikibase_session(bad_session),
    "Invalid field\\(s\\) in wikibase_session: foo"
  )
})

test_that("validate_wikibase_session() fails on bad language value", {
  expect_error(
    validate_wikibase_session(list(language = c("en", "fr"))),
    "language must be a single character string"
  )

  expect_error(
    validate_wikibase_session(list(pid_equivalence_property = 12)),
    "pid_equivalence_property must be a single character string"
  )

  expect_error(
    validate_wikibase_session(list(qid_equivalence_property = 12)),
    "qid_equivalence_property must be a single character string"
  )

  expect_error(
    validate_wikibase_session(list(language = 1)),
    "language must be a single character string"
  )
})

test_that("validate_wikibase_session() fails on bad data_curator", {
  expect_error(
    validate_wikibase_session(list(data_curator = "Jane")),
    "must be created using person\\(\\)"
  )
})

test_that("validate_wikibase_session() fails on bad URL", {
  expect_error(
    validate_wikibase_session(list(wikibase_api_url = "not-a-url")),
    "must be a valid URL"
  )
})

test_that("validate_wikibase_session() passes with a valid session", {
  good_session <- list(
    language = "en",
    data_curator = utils::person("Jane", "Doe"),
    wikibase_api_url = "https://demo.wikibase.org/w/api.php"
  )
  expect_silent(validate_wikibase_session(good_session))
})
