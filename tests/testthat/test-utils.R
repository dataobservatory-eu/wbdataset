test_that("is_valid_csrf_token() detects typical valid tokens", {
  expect_true(is_valid_csrf_token("+|rNkcBBnS9-UoPRhhkpdrA0C0RMkYY5OwM5ziYDeRQU="))
  expect_true(is_valid_csrf_token("+|abcDEF123-_=+/=="))
})

test_that("is_valid_csrf_token() rejects invalid or malformed tokens", {
  expect_false(is_valid_csrf_token(NULL))
  expect_false(is_valid_csrf_token(123))
  expect_false(is_valid_csrf_token("short"))
})
