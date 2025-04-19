test_that("validate_copy_entity_args(..., language, ...)", {
  expect_error(validate_copy_entity_args(language=NA_character_,
                                         validated_action = "test"),
               regexp = "'language' must be a non-empty character vector")
})

test_that("validate_create_entity_args(label, ...)", {
  expect_error(validate_create_entity_args(language="en",
                                         label = 123,
                                         validated_action = "test"),
               regexp = "must be a non-empty character string")
})

test_that("validate_create_entity_args(..., description)", {
  expect_error(validate_create_entity_args(language="en",
                                           label = "test",
                                           wikibase_api_url = "https://reprexbase.eu/jekyll/api.php",
                                           equivalence_property = "P11",
                                           equivalence_id =  "Q12",
                                           classification_property = "P4",
                                           classification_id = "Q89",
                                           description = 12,
                                           validated_action = "test"),
               regexp = " must be a character string or NULL")
})


test_that("validate_copy_entity_args(..., csrf)", {
  expect_error(validate_copy_entity_args(language="en",
                                         wikibase_api_url = "https://reprexbase.eu/jekyll/api.php",
                                         equivalence_property = "P11",
                                         equivalence_id =  "Q12",
                                         classification_property = "P4",
                                         classification_id = "Q89",
                                         data_curator =  person("Unknown"),
                                         validated_action = "test",
                                         csrf=NULL),
               regexp = "csrf must be created with get_csrf")
})

test_that("validate_create_entity_args(..., csrf)", {
  expect_error(validate_create_entity_args(language="en",
                                           label = "test",
                                           description="test",
                                           wikibase_api_url = "https://reprexbase.eu/jekyll/api.php",
                                           equivalence_property = "P11",
                                           equivalence_id =  "Q12",
                                           classification_property = "P4",
                                           classification_id = "Q89",
                                           data_curator =  person("Unknown"),
                                           validated_action = "test",
                                           csrf=NULL),
               regexp = "csrf must be created with get_csrf")
})


test_that("validate_copy_entity_args(..., validated_action)", {
  expect_error(validate_copy_entity_args(language="en",
                                         "https://reprexbase.eu/jekyll/api.php",
                                         equivalence_property = "P11",
                                         equivalence_id =  "Q12",
                                         classification_property = "P4",
                                         classification_id = "Q89",
                                         data_curator =  person("Unknown"),
                                         validated_action = NULL),
               regexp = "'validated_action' must be a non-empty character string")
})
