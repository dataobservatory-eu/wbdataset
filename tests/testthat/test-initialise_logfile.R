test_that("initialise_logfile()", {
  expect_s3_class(initialise_logfile(), "dataset_df")
  expect_true(grepl(pattern="Wikibase Data Model Log", dataset_title(initialise_logfile())))
  expect_equal(nrow(initialise_logfile()), 0)
  expect_s3_class(initialise_logfile()$rowid, "haven_labelled_defined")
})

