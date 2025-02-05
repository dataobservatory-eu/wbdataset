test_that("property_identity_datastring_create() makes correct assertions", {
  expect_error(property_identity_datastring_create(
    labels_list = list(en = "duration"),
    descriptions_list = list(en = "lenght of the process in time"),
    datatype = list("quantity")
  ))
})
