#' @title Create a property
#' @description Creates a property entity on a Wikibase instance with a single
#' language labelling and description. New labels and descriptions can be added
#' in further language later. This is a wrapper for
#' \href{https://www.wikidata.org/w/api.php?action=help&modules=wbeditentity}{MediaWiki
#' action=wbeditentity}
#' @details Optionally, the function adds an equivalent property to this newly
#' created property.
#' @param label A language-specific, human-readable name for the Wikibase entity
#'   (item, property, or Lexeme). Used when setting or retrieving the entity’s
#'   label in the specified language, for example, \code{"duration"}.
#' @param description A short, language-specific textual summary of the Wikibase
#'   entity (such as an item, property, or Lexeme). Descriptions help
#'   disambiguate entities with similar labels and are not required to be unique
#'   but are typically distinct within the same language. For example, \code{"Length in
#'   time."}.
#' @param language A single language code that indicates the language of the
#'   label and description, using BCP 47-compliant language tags (e.g., "en" for
#'   English, "fr" for French).
#' @param datatype A single character string defining the datatype of this
#'   property, for example, \code{"string"}, \code{"wikibase-item"},
#'   \code{"quantity"}, \code{"commonsMedia"}, \code{"url"}, \code{"external-id"},
#'   \code{"globe-coordinate"}, \code{"time"}, \code{"monolingualtext"}.
#' @param equivalence_property An optional PID of a property already defined in
#'   the same Wikibase instance that records the equivalence of this new
#'   property with a property defined elsewhere, for example, on Wikidata or
#'   CIDOC-CRM. Defaults to \code{NA_character_}; if left missing, no
#'   equivalence relations is will be claimed.
#' @param equivalence_id An optional PID or other identifier in Wikibase that
#'   records the equivalence of the property. An external ID. Defaults to
#'   \code{NA_character_}; if left missing, no equivalence relations is will be
#'   claimed.
#' @param wikibase_api_url The full URL of the Wikibase API, which is the
#'   address that the \code{wbdataset} R client sends requests to when
#'   interacting with the knowledge base. For example,
#'   \code{'https://reprexbase.eu/demowiki/api.php'}. The URL must end with
#'   api.php.
#' @param wikibase_api_url The full URL of the Wikibase API, which is the
#'   address that the \code{wbdataset} R client sends requests to when
#'   interacting with the knowledge base. For example,
#'   \code{'https://reprexbase.eu/jekyll/api.php'}. The URL must end with
#'   api.php. It is either given as a parameter or resolved from
#'   \code{wikibase_session}.
#' @param data_curator The name of the data curator who runs the function and
#'   creates the log file, created with \link[utils]{person}.
#'   It is either given as a parameter or resolved from
#'   \code{wikibase_session}. If no curator is given, then filled with
#'   \code{person("Unknown", "Curator")}.
#' @param log_file_name An explicitly stated full path to a possible CSV log
#'   file, defaults to \code{NULL}. If the value is \code{NULL}, no log file
#'   will be created.
#' @param csrf The CSRF token of your session, received with
#'   \code{\link{get_csrf}}. It is either given as a parameter or resolved
#'   from \code{wikibase_session}.
#' @param wikibase_session An optional named list of default values to reuse
#'   across multiple function calls. If any of the main parameters (such as
#'   \code{language}, \code{data_curator}, \code{log_file_name},
#'   \code{equivalence_propeert}, \code{classification_property}
#'   \code{wikibase_api_url}, or \code{csrf}) are missing from the function
#'   call, their values will be taken from this list if available. This is
#'   useful in interactive workflows or scripts where the same context is
#'   reused.
#' @export
#' @importFrom utils write.table
#' @return Returns a \code{\link[dataset]{dataset_df}} object.
#' The columns are:\cr
#' \describe{
#'  \item{\code{action}}{\code{create_property}}
#'  \item{\code{id_on_target}}{The new Property Identifier (PID) on the targeted Wikibase.}
#'  \item{\code{label}}{The property label}
#'  \item{\code{description}}{The description label}
#'  \item{\code{language}}{The language code of the label.}
#'  \item{\code{datatype}}{The datatype of the property, for example, `string`}
#'  \item{\code{wikibase_api_url}}{The MediaWiki API URL where the new property is created.}
#'  \item{\code{equivalence_property}}{The PID that connects an equivalence ID to the property.}
#'  \item{\code{equivalence_id}}{The ID of an equivalent property defined elsewhere.}
#'  \item{\code{classification_property}}{Not applicable for properties.}
#'  \item{\code{classification_id}}{Not applicable for properties.}
#'  \item{\code{success}}{TRUE if successfully created, FALSE if there was an error.}
#'  \item{\code{comment}}{A summary of the error messages(s), if success is FALSE.}
#'  \item{\code{time}}{The time when the action started.}
#'  \item{\code{logfile}}{The name of the CSV logfile.}
#' }
#' @examples
#' \dontrun{
#' # Only works with authentication
#' your_csrf <- get_csrf(
#'    username  = your_username,
#'    password = your_password,
#'    wikibase_api_url = "https://reprexbase.eu/demowiki/api.php"
#'    )
#'
#' create_property(
#'    label = "duration"
#'    language = "en"
#'    description = "Length of the process in time"
#'    datatype = "quantity",
#'    csrf = your_csrf)
#' }
#'

create_property <- function(label,
                            description,
                            language,
                            datatype,
                            equivalence_property = NA_character_,
                            equivalence_id = NA_character_,
                            wikibase_api_url = NULL,
                            data_curator = NULL,
                            log_file_name = NULL,
                            csrf = NULL,
                            wikibase_session = NULL) {

  language <- resolve_from_session("language", language, wikibase_session)
  data_curator <- resolve_from_session("data_curator", data_curator, wikibase_session)
  log_file_name <- resolve_from_session("log_file_name", log_file_name, wikibase_session)
  wikibase_api_url <- resolve_from_session("wikibase_api_url", wikibase_api_url, wikibase_session)
  equivalence_property <- resolve_from_session("wikibase_api_url", equivalence_property, wikibase_session)
  csrf <- resolve_from_session("csrf", csrf, wikibase_session)

  if(!is_valid_wikibase_datatype(datatype)) {
    stop ("create_property(..., datatype): ", datatype, " is not a valid Wikibase property type.")
  }

  if (is.null(data_curator)) {
    data_curator <- person("Unknown", "Curator", role = "dtm")
  }

  validate_create_entity_args(
    label = label,
    description = description,
    language = language,
    wikibase_api_url = wikibase_api_url,
    equivalence_property = equivalence_property,
    equivalence_id = equivalence_id,
    classification_property = NA_character_, # not used in this function
    classification_id = NA_character_, # not used in this function
    csrf = csrf,
    data_curator = data_curator,
    validated_action = "create_property()"
  )

  # Save the time of running the code
  action_time <- Sys.time()
  action_timestamp <- action_timestamp_create()

  if (is.null(log_file_name)) {
    log_file_name <- ""
  }

  if ( !is.na(equivalence_id) ) {
    # If there is an equivalence ID, for example, a PID on Wikidata, than the
    # equivalence property that connects this ID to the newly created property
    # must be given, too.
    assertthat::assert_that(
      ! is.na(equivalence_property),
      msg = "create_property() cannot add equivalence_id statement without equivalence_property.")
  }

  existing_property <- check_existing_property(
    action="create_property",
    search_term = label,
    language=language,
    equivalence_property = equivalence_property,
    equivalence_id = equivalence_id,
    classification_property = NA_character_,
    classification_id = NA_character_,
    data_curator = data_curator,
    log_file_name =  log_file_name,
    wikibase_api_url = wikibase_api_url,
    csrf =  csrf )

  if (!is.null(existing_property)) {
    # return existing item
    return(existing_property)
  }

  default_labels <- list ( language = language,
                        value = label)
  labels_list <- list ( default_labels )
  names(labels_list) <- language

  default_descriptions <- list ( language = language,
                              value = description)
  descriptions_list <- list ( default_descriptions )
  names(descriptions_list) <- language


  datastring <- property_identity_datastring_create(
    labels_list = labels_list,
    descriptions_list = descriptions_list,
    datatype = datatype
  )

  teststring = '{"labels":{"en-gb":{"language":"en-gb","value":"Propertylabel"}},"descriptions":{"en-gb":{"language":"en-gb","value":"Propertydescription"}},"datatype":"string"}'

  # Getting the user's CSRF token for writing.
  # See get_csrf, get_csrf_token.
  csrf_token <- get_csrf_token(csrf)

  if(!is_valid_csrf_token(csrf_token)) {
    stop("create_item(, csrf): csfr does not seem to be valid.")
  }

  # Posting the new property ----------------------------------------------
  new_property <- httr::POST(
    wikibase_api_url,
    body = list(
      action = "wbeditentity",
      new    = "property",
      data   = datastring,
      token  = csrf_token,
      format = "json"
    ),
    encode = "form",
    handle = csrf
  )

  # See if the created POST via wbeditentity was successful
  created_property_response <- httr::content(new_property,
                                             as = "parsed",
                                             type = "application/json")
  created_property_response

  if (is_response_success(created_property_response)) {
    # Successfully created the property
    message("Successfully created item ", created_property_response$entity$id, " (", created_property_response$entity$labels$en$value, ")")

    if (!is.na(equivalence_property)) {
    # Add the optional equivalence statement
      wikidata_pid_df <- add_id_statement(
        qid = created_property_response$entity$id,
        pid = equivalence_property,
        o = equivalence_id,
        wikibase_api_url = wikibase_api_url,
        csrf = csrf
      )
    }

    new_description_text <- ""
    new_descriptions <- created_property_response$entity$descriptions

    if ( length(new_descriptions)==0 ) {
      new_description_text <- ""
    } else {
      new_description_text <- created_property_response$entity$descriptions[[1]]$language
    }

    return_dataframe <- data.frame(
      action = "create_property",
      id_on_target = created_property_response$entity$id,
      label = label,
      description =  new_description_text,
      language =  created_property_response$entity$labels[[1]]$language,
      datatype = created_property_response$entity$datatype,
      wikibase_api_url = wikibase_api_url,
      equivalence_property =  equivalence_property,
      equivalence_id = equivalence_id,
      classification_property = NA_character_,
      classification_id = NA_character_,
      success = TRUE,
      comment = NA_character_,
      time = action_timestamp,
      logfile = log_file_name
    )
    return_dataframe
  } else if (
    "wikibase-validator-label-conflict" %in% unlist(created_property_response$error$messages)
    ) {
    # wikibase-validator-label-conflict: the property already exists, and this
    # information should be returned to the user.
    message_strings <- unlist(created_property_response$error$messages)

    message_strings <- message_strings[which(grepl("Property:", message_strings))]
    pattern <- "\\[\\[Property:*(.*?)\\|"
    result <- regmatches(message_strings, regexec(pattern, message_strings))
    old_pid <- result[[1]][2]

    message("Problem with recording a new property with label='", label, "'.
            A property already exists with this label: (", old_pid, ")")

    return_dataframe <- data.frame(
      action = "create_property",
      id_on_target = old_pid,
      label = label,
      description = "<description match not inspected>",
      language =  language,
      datatype = datatype,
      wikibase_api_url = wikibase_api_url,
      equivalence_property =  equivalence_property,
      equivalence_id = equivalence_id,
      classification_property = NA_character_,
      classification_id = NA_character_,
      success = FALSE,
      comment = "wikibase-validator-label-conflict, the label-language pair already exists.",
      time = action_timestamp,
      logfile = log_file_name
    )
  } else {
    # Return an empty data.frame if there was some error, with trying to log
    # the error itself.

    error_comments <- paste(
      unlist(
        lapply(created_property_response$error$messages, function(x) x$name)
        ), collapse="|")

    return_dataframe <- data.frame(
      action = "create_property",
      id_on_target = NA_character_,
      label = label,
      description = description,
      language =  language,
      datatype = datatype,
      wikibase_api_url = wikibase_api_url,
      equivalence_property =  equivalence_property,
      equivalence_id = equivalence_id,
      classification_property = NA_character_,
      classification_id = NA_character_,
      success = FALSE,
      comment = error_comments,
      time = action_timestamp,
      logfile = log_file_name
    )
  }

  description_text <- paste0(
    "Attempted and successful property creation on Wikibase to ",
    wikibase_api_url, " with wbdataset:create_property() at ",
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
        "Wikibase Create Property Log (",
        strftime(action_time, "%Y-%m-%d %H:%M:%OS0"), ")"
      ),
      creator = data_curator,
      dataset_date = Sys.Date()
    )
  )

  return_ds$rowid <- defined(paste0("wbi:", as.character(return_ds$id_on_target)),
                             namespace = wikibase_api_url
  )

  if(!is.null(log_file_name) && nchar(log_file_name)>0 ) {
    write.table(
      return_dataframe,
      file = log_file_name,
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(log_file_name),
      na = "NA",
      append = TRUE,
      quote = TRUE
    )
  }

  return_ds
}
