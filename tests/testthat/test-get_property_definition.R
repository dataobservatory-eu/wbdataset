

test_that("get_property_definition_assertions work", {
  expect_error(get_property_definition(pid="hello"))
})

test_that("get_property_definition_assertions work", {
  expect_true(inherits(get_property_definition(pid="P2047"), "json"))
})




get_property_definition(pid="P2047", return_type = "data.frame")
