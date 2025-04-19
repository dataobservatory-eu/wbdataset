test_that("create_property() validates inputs", {
  expect_error(create_property(label="test", description="test",
                           datatype = "chicken"),
               regexp = "is not a valid Wikibase property type")
})
