test_time <- Sys.time()
test_that("action_timestamp_create() works", {
  expect_equal(
    substr(action_timestamp_create(), 1, 8),
    substr(as.character(test_time), 1, 8)
  )
  expect_false(grepl(":", action_timestamp_create()))
})
