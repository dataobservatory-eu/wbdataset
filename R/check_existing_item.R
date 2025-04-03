#' @title Check if a label already has an item.
#' @description Avoid failed writing attempts by checking if a label already
#' matches an item.
#' @details A wrapper around
#' \href{https://www.wikidata.org/w/api.php?action=help&modules=wbsearchentities}{MediaWiki
#' action=wbsearchentities}.
#' @param action Defaults to \code{"create_item"}.
#' @param search_term A label in the given language, for example, "Estonia".
#' @param language A single language code that indicates the language of the
#'   label and description, using BCP 47-compliant language tags (e.g., "en" for
#'   English, "fr" for French). Defaults to \code{"en"} for English.
#' @param wikibase_api_url The full URL of the Wikibase API, which is the
#'   address that the \code{wbdataset} R client sends requests to when
#'   interacting with the knowledge base. In this case it defaults to
#'   \code{'https://www.wikidata.org/w/api.php'}, Wikidata itself, where no
#'   CSRF is needed.
#' @param csrf The CSRF token of your session, received with
#'   \code{\link{get_csrf}}, not needed if
#'   \code{wikibase_api_url="https://www.wikidata.org/w/api.php"}. Defaults
#'   to \code{NULL}.
#' @inheritParams create_item
#' @return A data.frame or NULL.
#' @examples
#' check_existing_item(
#'     search_term="Estonian National Museum",
#'     language = "en",
#'     wikibase_api_url="https://www.wikidata.org/w/api.php",
#'     csrf=NULL)
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
                                csrf = NULL) {

  action_timestamp <- action_timestamp_create()
  action_time <- Sys.time()

  get_search <- httr::POST(
    wikibase_api_url,
    body = list(
      action = "wbsearchentities",
      search = search_term,
      language = language,
      formatversion = 2,
      format = "json",
      type = "item",
      strictlanguage = "true"
    ),
    encode = "form",
    handle = csrf
  )

  search_response <- httr::content(get_search, as = "parsed", type = "application/json")

  if (!is.null(search_response$error)) {
    stop(paste(search_response$error$code, ": ", search_response$error$info))
  }


  if (search_response$success == 1) {
    if (length(search_response$search) == 0) {
      # No match was found
      return(NULL)
    }
  }

  is_display_match <- function(this_display) {
    this_display$label$value == search_term && this_display$label$language == language
  }

  matching_items <- vapply(1:length(search_response$search),
                           function(x) search_response$search[[x]]$id,
                           character(1))


  exact_match <- vapply(
    1:length(search_response$search),
    function(x) is_display_match(search_response$search[[x]]$display),
    logical(1)
  )

  if (sum(exact_match)>1) {
    stop("Multiple items [", paste(matching_items, collapse=", "),  "] are matching '", search_term, "' in language='", language, "'." )
  }

  if (! any(exact_match)) { return(NULL) }
  if ( is.null(search_response$search[[1]])) { return(NULL) }
  if ( ! is.list(search_response$search[[1]])) { return(NULL) }
  if ( is.null(search_response$search[[which(exact_match)]])) { return(NULL)}

  matching_items[exact_match]

  matching_item_data <- search_response$search[[which(exact_match)]]
  datatype <- "wikibase-item"
  comment_text <- glue::glue("An item with the label ", search_term, " already exists in this Wikibase.")

  return_dataframe <- data.frame(
    action = action,
    id_on_target = matching_item_data$id,
    label = matching_item_data$label,
    description = matching_item_data$description,
    language = language,
    datatype = datatype,
    wikibase_api_url = wikibase_api_url,
    equivalence_property = equivalence_property,
    equivalence_id = equivalence_id,
    classification_property = classification_property,
    classification_id = classification_id,
    success = FALSE,
    comment = comment_text,
    time = action_timestamp,
    logfile = ifelse(is.null(log_file_name), "", log_file_name)
  )

  description_text <- paste0(
    "Failed item creation on Wikibase to ",
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

  prefix <- ifelse(wikibase_api_url=="https://www.wikidata.org/w/api.php", "wbi:", "wd:")
  return_ds$rowid <- defined(paste0(prefix, as.character(return_ds$id_on_target)),
    namespace = wikibase_api_url
  )

  return_ds
}
