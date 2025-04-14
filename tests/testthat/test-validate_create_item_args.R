test_that("validate_create_entity_args() catches missing label", {
  expect_error(
    validate_create_entity_args(
      label = "", description = "desc", language = "en",
      wikibase_api_url = "https://example.com/api.php",
      equivalence_property = NA_character_,
      equivalence_id = NA_character_,
      classification_property = NA_character_,
      classification_id = NA_character_,
      csrf = "fake", data_curator = person("Test", "User"),
      validated_action = "create_item()"
    ),
    "label"
  )
})

test_that("validate_create_entity_args() catches wrong URL", {
  expect_error(
    validate_create_entity_args(
      label = "Estonia", description = "desc",
      language = "en",
      wikibase_api_url = "ftp://example.com/api.php",
      equivalence_property = NA_character_,
      equivalence_id = NA_character_,
      classification_property = NA_character_,
      classification_id = NA_character_,
      validated_action = "create_property()",
      csrf = "fake", data_curator = person("Test", "User")
    ),
    "must be a valid URL."
  )
})

test_that("validate_create_entity_args() catches woring equivalence_property", {
  expect_error(
    validate_create_entity_args(
      label = "Estonia", description = "desc",
      language = "en",
      wikibase_api_url = "https://example.com/api.php",
      equivalence_property = 1234,
      equivalence_id = NA_character_,
      classification_property = NA_character_,
      classification_id = NA_character_,
      validated_action = "create_property()",
      csrf = "fake", data_curator = person("Test", "User")
    ),
    "'equivalence_property' must be a non-empty character string."
  )
})

test_that("validate_create_entity_args() dual parameter error with classification", {
  expect_error(
    validate_create_entity_args(
      label = "Estonia", description = "desc",
      language = "en",
      wikibase_api_url = "https://example.com/api.php",
      equivalence_property = NA_character_,
      equivalence_id = NA_character_,
      classification_property = "P12",
      classification_id = NA_character_,
      validated_action = "create_property()",
      csrf = "fake", data_curator = person("Test", "User")
    ),
    "classification_property provided without classification_id"
  )
})
