test_that("create_item() validates inputs", {
  expect_error(create_item(label=123, description="test"),
               regexp = "\\'label\\' must be a non-empty character string")
})

