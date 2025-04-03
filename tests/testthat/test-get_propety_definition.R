test_that("get_property_definition() works", {
  test <- get_property_definition(pid = "P2047",
                                  return_type = "data.frame")
  expect_equal(nrow(test), 3)
  expect_equal(test$datatype, rep("quantity", 3))
  expect_true(inherits(test, "data.frame"))
  expect_message(get_property_definition(pid = "P3",
                                        language = "en",
                                        return_type = "data.frame"))
  expect_true(get_property_definition(pid = "P3",
                                      language = "en",
                                      return_type = "data.frame")$success)
})



