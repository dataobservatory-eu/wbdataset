#' @title Check if a label already has an property.
#' @description Avoid failed writing attempts by checking if a label already
#' matches an item.
#' @details A wrapper around
#' \href{https://www.wikidata.org/w/api.php?action=help&modules=wbsearchentities}{MediaWiki
#' action=wbsearchentities}.
#' @param action Defaults to \code{"create_property"}.
#' @param classification_property The instance of, or subclass of, or superclass
#'   of property. Defaults to \code{NA_character} when not used.
#' @param classification_id The QID of the class. Defaults to
#'   \code{NA_character} when not used.
#' @param wikibase_api_url The full URL of the Wikibase API, which is the
#'   address that the \code{wbdataset} R client sends requests to when
#'   interacting with the knowledge base. In this case it defaults to
#'   \code{'https://www.wikidata.org/w/api.php'}, Wikidata itself, where no
#'   CSRF is needed.
#' @param csrf The CSRF token of your session, received with
#'   \code{\link{get_csrf}}, not needed if
#'   \code{wikibase_api_url="https://www.wikidata.org/w/api.php"}. Defaults
#'   to \code{NULL}.
#' @param search_term A label in the given language, for example, "Estonia".
#' @inheritParams create_property
#' @return A data.frame or NULL.
#' @examples
#' # No CSRF needed for Wikidata, but you will need it for Wikibase Suit
#' check_existing_property(
#'     search_term="instance of",
#'     language = "en",
#'     wikibase_api_url="https://www.wikidata.org/w/api.php")
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
    wikibase_api_url="https://www.wikidata.org/w/api.php",
    csrf=NULL) {

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
      type = "property",
      strictlanguage = "true"
    ),
    encode = "form",
    handle = csrf
  )

  search_response <- httr::content(get_search,
                                   as = "parsed",
                                   type = "application/json")

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

  matching_props <- vapply(1:length(search_response$search), function(x) search_response$search[[x]]$id, character(1))

  exact_match <- vapply(
    1:length(search_response$search), function(x) is_display_match(search_response$search[[x]]$display),
    logical(1)
  )

  if (sum(exact_match)>1) {
    stop("Multiple items [", paste(matching_props, collapse=", "),  "] are matching '", search_term, "' in language='", language, "'." )
  }

  if (! any(exact_match)) { return(NULL) }
  if ( is.null(search_response$search[[1]])) { return(NULL) }
  if ( ! is.list(search_response$search[[1]])) { return(NULL) }
  if ( is.null(search_response$search[[which(exact_match)]])) { return(NULL)}

  matching_props[exact_match]

  matching_property_data <- search_response$search[[which(exact_match)]]

  if (action %in% c("create_property", "copy_property")) {
    datatype <- get_property_definition(matching_property_data$id, "en",
                                        wikibase_api_url = wikibase_api_url,
                                        return_type =  "data.frame",
                                        csrf= csrf)$datatype
    comment_text <- glue::glue("A property with the label ", search_term, " already exists in this Wikibase.")
  }

  return_dataframe <- data.frame(
    action = action,
    id_on_target = matching_property_data$id,
    label = matching_property_data$label,
    description = matching_property_data$description,
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

  prefix <- ifelse(wikibase_api_url=="https://www.wikidata.org/w/api.php",
                   "wd:", "wbi:")

  return_ds$rowid <- defined(paste0(prefix, as.character(return_ds$id_on_target)),
    namespace = wikibase_api_url
  )

  return_ds
}
