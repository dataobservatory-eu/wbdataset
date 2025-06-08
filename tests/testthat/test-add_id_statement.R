test_that("add_id_statement() throws error", {
  expect_error(
    add_id_statement(
      qid = "Q4441",
      pid = "P417",
      o = "12345",
      wikibase_type = "time",  # invalid type
      wikibase_api_url = "https://reprexbase.eu/fu/api.php",
      csrf = "dummy"
    ),
    regexp = "Unsupported datatype"
  )
})

test_that("add_id_statement returns data frame on success", {
  skip_if_not_installed("mockery")
  library(mockery)
  # Create a fake response object (POST will return this)
  fake_response <- list(success = 1, claim = list(
    id = "Q4441$ABC123",
    mainsnak = list(datavalue = list(value = "mocked-id"))
  ))

  # Mock functions
  mock_post <- mock("fake_response_object")
  mock_content <- mock(fake_response)
  mock_csrf <- mock("+FAKE-CSRF+TOKEN+")

  # Use mockery to stub functions inside add_id_statement()
  stub(add_id_statement, "httr::POST", mock_post)
  stub(add_id_statement, "httr::content", mock_content)
  stub(add_id_statement, "get_csrf_token", mock_csrf)

  # Call the function
  result <- add_id_statement(
    qid = "Q4441",
    pid = "P417",
    o = "mocked-id",
    wikibase_api_url = "https://reprexbase.eu/fu/api.php",
    csrf = "dummy"
  )

  # Assertions
  expect_s3_class(result, "data.frame")
  expect_equal(result$s, "Q4441")
  expect_equal(result$p, "P417")
  expect_equal(result$o, "mocked-id")

  # Verify mocks were called as expected
  expect_called(mock_post, 1)
  expect_called(mock_content, 1)
  expect_called(mock_csrf, 1)
})
