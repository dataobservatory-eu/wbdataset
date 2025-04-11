#' @title Check for Existing Items in Wikibase
#'
#' @description This function searches for existing items in a specified
#'   Wikibase instance that match a given label in a specified language. It
#'   helps prevent duplicate item creation by identifying existing matches.
#'
#' @details The function interfaces with the Wikibase API's `wbsearchentities`
#'   action to perform a search based on the provided label (`search_term`) and
#'   language. It returns information about matching items, facilitating the
#'   management of data consistency within the Wikibase. See:
#' \href{https://www.wikidata.org/w/api.php?action=help&modules=wbsearchentities}{MediaWiki
#' action=wbsearchentities}.
#'
#' @param search_term A character string representing the label to search for in
#'   the Wikibase. For example, `"Estonian National Museum"`.
#' @param language A character string specifying the language code of the label,
#'   adhering to BCP 47 standards (e.g., `"en"` for English). Defaults to
#'   `"en"`. For more details, see \href{https://tools.ietf.org/html/bcp47}{BCP
#'   47}.
#' @param action A character string indicating the action being performed.
#'   Defaults to `"create_item"`.
#' @param log_file_name A character string specifying the name of the log file.
#'   Defaults to `NA_character_`.
#' @param data_curator An object of class `person` representing the data
#'   curator. Defaults to `person("Unknown", "Person")`.
#' @param ambiguity_handling A character string indicating how to handle
#'   ambiguous results: "return_null" or "return_first". Defaults to
#'   "return_null".
#' @return A `dataset_df` object containing information about the matching
#'   item(s), including action performed, item ID, label, description, language,
#'   and other metadata. Returns `NULL` if no matching items are found.
#' @inheritParams create_item
#' @importFrom glue glue
#' @importFrom dataset dataset_df defined
#' @return A data.frame or NULL.
#' @examples
#' check_existing_item(
#'   search_term = "Estonian National Museum",
#'   language = "en",
#'   wikibase_api_url = "https://www.wikidata.org/w/api.php",
#'   csrf = NULL
#' )
#' @export

check_existing_item <- function(search_term,
                                language = "en",
                                equivalence_property = NA_character_,
                                equivalence_id = NA_character_,
                                classification_property = NA_character_,
                                classification_id = NA_character_,
                                action = "create_item",
                                log_file_name = NA_character_,
                                data_curator = person("Unknown", "Person"),
                                wikibase_api_url = "https://www.wikidata.org/w/api.php",
                                csrf = NULL,
                                ambiguity_handling = "return_null") {
  # Validate inputs
  if (!is.character(search_term) || length(search_term) != 1 || nchar(search_term) == 0) {
    stop("Invalid input: 'search_term' must be a non-empty character string.")
  }
  if (!is.character(language) || length(language) != 1 || nchar(language) == 0) {
    stop("Invalid input: 'language' must be a non-empty character string.")
  }
  if (!is.character(wikibase_api_url) || length(wikibase_api_url) != 1 || !grepl("^https?://", wikibase_api_url)) {
    stop("Invalid input: 'wikibase_api_url' must be a valid URL string.")
  }

  # Search for entities
  search_results <- search_wikibase_entities(
    search_term = search_term,
    language = language,
    type = "item",
    wikibase_api_url = wikibase_api_url,
    csrf = csrf
  )

  # Handle ambiguity
  resolved_item <- handle_search_term_ambiguity(
    search_results = search_results,
    search_term = search_term,
    language = language,
    strategy = ambiguity_handling,
    csrf = csrf
  )

  if (is.null(resolved_item)) {
    return(NULL)
  }

  # Prepare metadata
  action_timestamp <- action_timestamp_create()
  action_time <- Sys.time()
  comment_text <- glue::glue("An item with the label '{search_term}' already exists in this Wikibase.")

  # Construct return data frame
  return_dataframe <- data.frame(
    action = action,
    id_on_target = resolved_item$id,
    label = resolved_item$label,
    description = ifelse(is.null(resolved_item$description), "", resolved_item$description),
    language = language,
    datatype = "wikibase-item",
    wikibase_api_url = wikibase_api_url,
    equivalence_property = equivalence_property,
    equivalence_id = equivalence_id,
    classification_property = classification_property,
    classification_id = classification_id,
    success = FALSE,
    comment = as.character(comment_text),
    time = action_timestamp,
    logfile = ifelse(is.null(log_file_name), "", log_file_name)
  )

  description_text <- paste0(
    "Failed item creation on Wikibase at ",
    wikibase_api_url, " with action: ", action, " at ",
    substr(as.character(action_time), 1, 19)
  )

  # Create dataset_df object
  return_ds <- dataset_df(
    action = return_dataframe$action,
    id_on_target = defined(
      return_dataframe$id_on_target,
      label = paste0("ID on ", wikibase_api_url),
      namespace = wikibase_api_url
    ),
    label = defined(
      return_dataframe$label,
      label = "Label of entity"
    ),
    description = defined(
      return_dataframe$description,
      label = "Description of entity"
    ),
    language = defined(
      return_dataframe$language,
      label = "Language of label and description"
    ),
    datatype = return_dataframe$datatype,
    wikibase_api_url = wikibase_api_url,
    equivalence_property = defined(
      return_dataframe$equivalence_property,
      label = paste0("Equivalence property on ", wikibase_api_url),
      namespace = wikibase_api_url
    ),
    equivalence_id = defined(
      return_dataframe$equivalence_id,
      label = "Equivalent entity in a different graph"
    ),
    classification_property = defined(
      return_dataframe$classification_property,
      label = "A property relationship to a class or superclass",
      namespace = wikibase_api_url
    ),
    classification_id = defined(
      return_dataframe$classification_id,
      label = "Superclass or class on the target instance",
      namespace = wikibase_api_url
    ),
    success = return_dataframe$success,
    comment = return_dataframe$comment,
    time = return_dataframe$time,
    logfile = return_dataframe$logfile,
    dataset_bibentry = dublincore(
      title = paste0(
        "Wikibase Create Item Log (",
        strftime(action_time, "%Y-%m-%d %H:%M:%OS0"), ")"
      ),
      description_text,
      creator = data_curator,
      dataset_date = Sys.Date()
    )
  )

  prefix <- ifelse(wikibase_api_url == "https://www.wikidata.org/w/api.php",
    "wbi:",
    "wd:"
  )

  return_ds$rowid <- defined(
    paste0(
      prefix,
      as.character(return_ds$id_on_target)
    ),
    namespace = wikibase_api_url
  )

  return_ds
}
