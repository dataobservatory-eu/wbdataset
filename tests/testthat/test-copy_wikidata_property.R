test_that("copy_wikidata_property() fails when CSRF token is missing or invalid", {
  expect_error(copy_wikidata_property(
    pid_on_wikidata = "P31",
    csrf = NULL,
    wikibase_api_url = "https://reprexbase.eu/jekyll/api.php",
    data_curator = person("Jane", "Doe")
  ), " csrf must be created with get_csrf()")
  })


test_that("copy_wikidata_property() fails when data curator is not a person", {
  expect_error(copy_wikidata_property(
    pid_on_wikidata = "P31",
    wikibase_api_url = "https://reprexbase.eu/jekyll/api.php",
    csrf = NULL,
    data_curator = 123
  ), regexp = "must be a person")
})

# See further tests with validate_create_entity_args() and
# validate_copy_entity_args()
