#' Check for existing items in Wikibase
#'
#' @description
#' Search a Wikibase instance for an existing item matching a specified
#' label and language. This function supports duplicate prevention in
#' item creation workflows by identifying and returning existing items.
#'
#' @details
#' `check_existing_item()` searches for items using the MediaWiki
#' `wbsearchentities` API action. The search is performed using
#' `search_term` and `language` on the specified Wikibase instance.
#'
#' Search results are passed to [handle_search_term_ambiguity()] to
#' resolve cases in which multiple items match the search term.
#' The `ambiguity_handling` argument determines how these results
#' are handled:
#' \describe{
#'   \item{`"return_null"`}{Return `NULL` when an ambiguous match
#'     cannot be resolved. This is the default.}
#'   \item{`"return_first"`}{Select the first matching item when
#'     multiple candidates are found.}
#' }
#'
#' If no item is resolved, the function returns `NULL`.
#' Otherwise, it returns a structured record describing the existing
#' item and the attempted operation.
#'
#' The returned `success` field is `FALSE`, indicating that no new
#' item was created because an existing item was identified. It does
#' not indicate that the search itself failed.
#'
#' The equivalence and classification parameters provide contextual
#' metadata for the returned record. This function does not create
#' new items or add statements to existing items.
#'
#' See the
#' \href{https://www.wikidata.org/w/api.php?action=help&modules=wbsearchentities}{
#' MediaWiki API documentation for wbsearchentities}.
#'
#' @param search_term Character string. Label or search expression
#'   used to find an existing Wikibase item, for example
#'   `"Estonian National Museum"`.
#' @param language Character string. Language code used for the
#'   search. Defaults to `"en"`. Must follow
#'   \href{https://www.rfc-editor.org/rfc/bcp/bcp47.txt}{
#'   BCP 47 language-tag conventions}.
#' @param equivalence_property Character string. Optional property
#'   identifier (PID) representing an equivalence relationship
#'   between the item and an entity defined in another system.
#'   Defaults to `NA_character_`.
#' @param equivalence_id Character string. Identifier of the
#'   equivalent entity in another system, such as Wikidata.
#'   Defaults to `NA_character_`.
#' @param classification_property Character string. Property
#'   identifier (PID) used for a classification relationship,
#'   such as instance of or subclass of.
#'   Defaults to `NA_character_`.
#' @param classification_id Character string. QID of the class
#'   or superclass associated with the item.
#'   Defaults to `NA_character_`.
#' @param action Character string. Name of the operation for which
#'   the existence check is performed. Defaults to `"create_item"`.
#'   The value is retained in the returned metadata.
#' @param log_file_name Character string. Name or path of the log
#'   file associated with the operation. Defaults to `NA_character_`.
#'   This function records the value but does not write a log file.
#' @param data_curator A [utils::person()] object identifying the
#'   curator responsible for the operation. Defaults to
#'   `person("Unknown", "Person")`.
#' @param wikibase_api_url Character string. Full URL of the target
#'   MediaWiki API endpoint. Defaults to
#'   `https://www.wikidata.org/w/api.php`.
#' @param csrf Optional authenticated session object returned by
#'   [get_csrf()]. Defaults to `NULL` for public API searches.
#' @param ambiguity_handling Character string. Strategy for resolving
#'   multiple search results: `"return_null"` or `"return_first"`.
#'   Defaults to `"return_null"`.
#'
#' @return
#' A [dataset::dataset_df()] describing the existing item, or `NULL`
#' if no item is resolved. The returned dataset contains:
#' \describe{
#'   \item{`rowid`}{Row identifier associated with the existing item.}
#'   \item{`action`}{Name of the attempted operation.}
#'   \item{`id_on_target`}{QID of the existing Wikibase item.}
#'   \item{`label`}{Label of the existing item.}
#'   \item{`description`}{Description of the existing item, or an
#'     empty string if unavailable.}
#'   \item{`language`}{Language code used for the search.}
#'   \item{`datatype`}{Entity datatype, `"wikibase-item"`.}
#'   \item{`wikibase_api_url`}{API endpoint of the Wikibase instance.}
#'   \item{`equivalence_property`}{PID of the equivalence property.}
#'   \item{`equivalence_id`}{Identifier of the equivalent entity.}
#'   \item{`classification_property`}{PID of the classification property.}
#'   \item{`classification_id`}{QID of the associated class.}
#'   \item{`success`}{`FALSE`, because no new item was created.}
#'   \item{`comment`}{Message indicating that an existing item was found.}
#'   \item{`time`}{Timestamp of the operation.}
#'   \item{`logfile`}{Name of the associated log file.}
#' }
#'
#' @seealso
#' [create_item()], [copy_wikidata_item()],
#' [search_wikibase_entities()]
#'
#' @importFrom glue glue
#' @importFrom dataset dataset_df defined
#' @examples
#' check_existing_item(
#'   search_term = "Estonian National Museum",
#'   language = "en",
#'   wikibase_api_url = "https://www.wikidata.org/w/api.php",
#'   csrf = NULL
#' )
#' @export

check_existing_item <- function(
  search_term,
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
  ambiguity_handling = "return_null"
) {
  # Validate inputs
  if (!is.character(search_term) ||
    length(search_term) != 1 ||
    nchar(search_term) == 0) {
    stop("Invalid input: 'search_term' must be a non-empty character string.")
  }
  if (!is.character(language) ||
    length(language) != 1 ||
    nchar(language) == 0) {
    stop("Invalid input: 'language' must be a non-empty character string.")
  }
  if (!is.character(wikibase_api_url) ||
    length(wikibase_api_url) != 1 ||
    !grepl("^https?://", wikibase_api_url)) {
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
