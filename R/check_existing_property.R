#' @title Check if a label already has a property.
#' @description Avoid failed writing attempts by checking if a label already
#'   matches a property.
#' @details A wrapper around
#'   \href{https://www.wikidata.org/w/api.php?action=help&modules=wbsearchentities}{MediaWiki
#'   action=wbsearchentities}.
#' @param search_term A character string specifying the label to search for. For
#'   example, "instance of".
#' @param action A name for the action performed by a function for the
#' log file, defaults to \code{"create_property"}.
#' @param language A character string specifying the language code of the label,
#'   adhering to BCP 47 standards (e.g., `"en"` for English). Defaults to
#'   `"en"`.
#' @param classification_property The instance of, or subclass of, or superclass
#'   of property. Defaults to \code{NA_character} when not used.
#' @param classification_id The QID of the class. Defaults to
#'   \code{NA_character} when not used.
#' @param equivalence_property An optional PID of a property already defined in
#'   the same Wikibase instance that records the equivalence of this new
#'   item with a property defined elsewhere, for example, on Wikidata or
#'   CIDOC-CRM. Defaults to \code{NA_character_}; if left missing, no
#'   equivalence relations is will be claimed.
#' @param equivalence_id The identifier that uniquely identifies this item among
#' another system's definitions. Defaults to
#'   \code{NA_character_}; if left missing, no equivalence relations is will be
#'   claimed.
#' @param wikibase_api_url The full URL of the Wikibase API. Defaults to
#'   `'https://www.wikidata.org/w/api.php'`, which is Wikidata's API endpoint.
#' @param csrf The CSRF token of your session, obtained with
#'   \code{\link{get_csrf}}. Not needed if
#'   \code{wikibase_api_url="https://www.wikidata.org/w/api.php"}. Defaults to
#'   \code{NULL}.
#' @param ambiguity_handling A character string specifying how to handle cases
#'   where multiple properties match the search term. Options are
#'   \code{"return_first"} (default) to return the first match, or
#'   \code{"return_null"} to return \code{NULL} when multiple matches are found.
#' @param log_file_name An explicitly stated full path to a possible CSV log
#'   file, defaults to \code{NULL}. If the value is \code{NULL}, no log file
#'   will be created.
#' @param data_curator The name of the data curator who runs the function and
#'   creates the log file, created with \link[utils]{person}.
#'   It is either given as a parameter or resolved from
#'   \code{wikibase_session}. If no curator
#' @return A data frame containing details of the matching property, or
#'   \code{NULL} if no match is found or if multiple matches are found and
#'   \code{ambiguity_handling} is set to \code{"return_null"}.
#' @examples
#' # No CSRF needed for Wikidata
#' check_existing_property(
#'   search_term = "instance of",
#'   language = "en",
#'   wikibase_api_url = "https://www.wikidata.org/w/api.php"
#' )
#' @export

check_existing_property <- function(
    search_term,
    language,
    equivalence_property = NA_character_,
    equivalence_id = NA_character_,
    classification_property = NA_character_,
    classification_id = NA_character_,
    action = "create_property",
    log_file_name = NA_character_,
    data_curator = person("Unknown", "Person"),
    wikibase_api_url = "https://www.wikidata.org/w/api.php",
    ambiguity_handling = "return_first",
    csrf = NULL) {
  action_timestamp <- action_timestamp_create()
  action_time <- Sys.time()

  # Match the ambiguity handling strategy
  ambiguity_handling <- match.arg(ambiguity_handling)

  # Input validations
  if (!is.character(search_term) || length(search_term) != 1 || nchar(search_term) == 0) {
    stop("Invalid input in check_existing_property(): 'search_term' must be a non-empty character string.")
  }
  if (!is.character(language) || length(language) != 1 || nchar(language) == 0) {
    stop("Invalid input in check_existing_property(): 'language' must be a non-empty character string.")
  }
  if (!is.character(wikibase_api_url) || length(wikibase_api_url) != 1 || !grepl("^https?://", wikibase_api_url)) {
    stop("Invalid input in check_existing_property(): 'wikibase_api_url' must be a valid URL string.")
  }

  action_timestamp <- action_timestamp_create()
  action_time <- Sys.time()

  # Perform the search
  search_results <- search_wikibase_entities(
    search_term = search_term,
    language = language,
    wikibase_api_url = wikibase_api_url,
    type = "property",
    csrf = csrf
  )

  # Handle ambiguity
  resolved_property <- handle_search_term_ambiguity(
    search_results = search_results,
    search_term = search_term,
    language = language,
    strategy = ambiguity_handling
  )

  if (is.null(resolved_property)) {
    return(NULL)
  }

  # Extract matching property data
  matching_property_data <- resolved_property

  comment_text <- glue::glue(
    "A property with the label ",
    search_term, " already exists in this Wikibase."
  )

  return_dataframe <- data.frame(
    action = action,
    id_on_target = matching_property_data$id,
    label = matching_property_data$label,
    description = ifelse(is.null(matching_property_data$description),
      "", matching_property_data$description
    ),
    language = language,
    datatype = matching_property_data$datatype,
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
    "Failed property creation on Wikibase to ",
    wikibase_api_url, " with wbdataset:", action, "() at ",
    substr(as.character(action_time), 1, 19)
  )

  return_dataframe <- data.frame(
    action = action,
    id_on_target = matching_property_data$id,
    label = matching_property_data$label,
    description = ifelse(is.null(matching_property_data$description),
      "", matching_property_data$description
    ),
    language = language,
    datatype = matching_property_data$datatype,
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
    "Failed property creation on Wikibase to ",
    wikibase_api_url, " with wbdataset:", action, "() at ",
    substr(as.character(action_time), 1, 19)
  )

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
    "wd:", "wbi:"
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
