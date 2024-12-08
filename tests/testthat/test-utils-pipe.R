test_that("%>% works", {
  expect_equal(mtcars %>% summary(), summary(mtcars))
})
