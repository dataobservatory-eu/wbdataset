test_that("left_join_column() works", {
  skip("Waiting on API support for this property type.")
  data("wikidata_countries_df")
  ds <- wikidata_countries_df
  property <- "P297"
  add_one_col <- left_join_column(
    ds = wikidata_countries_df,
    property = "P297"
  )
  expect_equal(nrow(add_one_col), nrow(wikidata_countries_df))
  expect_equal(add_one_col$P297, c(NA_character_, "LI", NA_character_, "XK"))
})

test_that("left_join_column() works", {
  skip("Waiting on API support for this property type.")
  data("wikidata_countries_df")
  ds <- wikidata_countries_df
  add_one_col <- left_join_column(
    ds = wikidata_countries_df,
    property = "P297"
  )
  add_2nd_col <- left_join_column(
    ds = add_one_col,
    property = "P1566",
    label = "Geonames ID",
    namespace = "https://www.geonames.org/",
    silent = TRUE
  )
  expect_equal(names(add_2nd_col), c("qid", "label", "description", "language", "rowid", "P297", "P1566"))
  expect_equal(attr(add_2nd_col$P1566, "namespace"), "https://www.geonames.org/")
})
