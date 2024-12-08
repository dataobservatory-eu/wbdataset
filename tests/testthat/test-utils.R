test_that("is_df_not_empty() works", {
  expect_true(is_df_not_empty (df=iris))
  expect_false(is_df_not_empty (df=data.frame()))
  expect_false(is_df_not_empty (df=1))
})

