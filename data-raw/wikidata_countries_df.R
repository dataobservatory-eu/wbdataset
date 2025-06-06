## code to prepare the `wikidata_countries_df` dataset

wikidata_countries <- c(
  "http://www.wikidata.org/entity/Q756617",
  "http://www.wikidata.org/entity/Q347",
  "http://www.wikidata.org/entity/Q3908",
  "http://www.wikidata.org/entity/Q1246"
)

wikidata_countries_df <- get_item(
  qid = wikidata_countries,
  language = "en",
  title = "European countries",
  creator = person("Daniel", "Antal")
)

usethis::use_data(wikidata_countries_df, overwrite = TRUE)
