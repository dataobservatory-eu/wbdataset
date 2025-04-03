test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
jekyll_anna <- "Reprexba jekyll@MesterannaBot"
jekyll_daniel <- "Reprexba jekyll@AntaldanielBot"
agya_wikibase_api_url <- "https://reprexbase.eu/jekyll/api.php" # temporary
my_csrf <- get_csrf(
  jekyll_daniel,
  password = key_get(
    service = agya_wikibase_api_url,
    username = jekyll_daniel,
  ),
  agya_wikibase_api_url
)

agya_wikibase_session <- list(
  csrf = my_csrf,
  wikibase_api_url = agya_wikibase_api_url,
  language = c("en", "sk", "hu", "de", "fr", "es", "ro"),
  data_curator = person("Daniel", "Antal"),
  pid_equivalence_property = "P2",
  qid_equivalence_property = "P35",
  log_path = here("not_included")
)

digital_record_class <- create_item(
  label="digital record",
  description="a subclass of the record, a record in the collection of the Andrássy Gyula Foundation.",
  language = "en",
  classification_property = "P137", #subclass of
  classification_id = "Q584", #record
  wikibase_api_url = agya_wikibase_api_url,
  csrf = agya_wikibase_session$csrf,
  wikibase_session = agya_wikibase_session)
