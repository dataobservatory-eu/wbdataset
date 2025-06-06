test_that("left_join_column() works with wikidata_countries_df [MOCKED]", {
  skip_if_not_installed("mockery")
  library(mockery)
  data("wikidata_countries_df")

  ds <- data.frame(qid = as.character(wikidata_countries_df$qid))

  fake_get_claim <- function(qid, property, ...) {
    switch(qid,
           "Q347" = data.frame(qid = "Q347", P297 = "LI"),
           "Q1246" = data.frame(qid = "Q1246", P297 = "XK"),
           "Q756617" = NULL,
           "Q3908" = NULL,
           NULL
    )
  }

  stub(left_join_column, "get_claim", fake_get_claim)

  result <- left_join_column(ds, property = "P297")
  expect_equal(result$P297, c(NA, "LI", NA, "XK"))
})


