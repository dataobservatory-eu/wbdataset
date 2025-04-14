test_that("is_valid_csrf() detects typical valid tokens", {
  expect_true(is_valid_csrf("+|rNkcBBnS9-UoPRhhkpdrA0C0RMkYY5OwM5ziYDeRQU="))
  expect_true(is_valid_csrf("+|abcDEF123-_=+/=="))
})

test_that("is_valid_csrf() rejects invalid or malformed tokens", {
  expect_false(is_valid_csrf(NULL))
  expect_false(is_valid_csrf(123))
  expect_false(is_valid_csrf("short"))
  expect_false(is_valid_csrf("not-prefixed"))
})
