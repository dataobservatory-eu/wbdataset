#' @keywords internal
validate_create_entity_args <- function(label,
                                        description,
                                        language,
                                        wikibase_api_url,
                                        equivalence_property,
                                        equivalence_id,
                                        classification_property,
                                        classification_id,
                                        csrf,
                                        data_curator,
                                        validated_action) {
  if (!is.character(label) || length(label) != 1 || nchar(label) == 0) {
    stop(validated_action, ": 'label' must be a non-empty character string.")
  }

  if (!is.character(language) || length(language) != 1 || nchar(language) == 0) {
    stop(validated_action, ": 'language' must be a non-empty character string.")
  }

  if (!is.character(wikibase_api_url) || length(wikibase_api_url) != 1 ||
    !grepl("^https?://", wikibase_api_url)) {
    stop(validated_action, ": 'wikibase_api_url' must be a valid URL.")
  }

  if (!is.character(equivalence_property) || length(equivalence_property) != 1) {
    stop(validated_action, ": 'equivalence_property' must be a non-empty character string.")
  }

  if (!is.character(classification_property) || length(classification_property) != 1) {
    stop(validated_action, ": 'classification_property' must be a non-empty character string.")
  }


  if (!is.null(description) && (!is.character(description) || length(description) != 1)) {
    stop(validated_action, ": 'description' must be a character string or NULL.")
  }

  if (!is.null(data_curator) && !inherits(data_curator, "person")) {
    stop(validated_action, ": 'data_curator' must be a `person()` object.")
  }

  if (!is.na(equivalence_id) && is.na(equivalence_property)) {
    stop(validated_action, ": Cannot provide an 'equivalence_id' without an 'equivalence_property'.")
  }

  # Optionally: validate classification logic
  if (!is.na(classification_property) && is.na(classification_id)) {
    stop(validated_action, ": classification_property provided without classification_id.")
  }

  if (!is.na(classification_id) && is.na(classification_property)) {
    stop(validated_action, ": classification_id provided without classification_property.")
  }

  invisible(TRUE)
}
