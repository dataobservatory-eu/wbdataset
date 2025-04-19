#' @title Copy a Wikidata item(s)
#' @description This code will copy (an) item label(s) and description(s) from
#'   Wikidata to a new instance. It should work between instances, but the
#'   authentication to copy from a password protected instance is not yet coded.
#'   The function is more specific than create_item, because this one creates
#'   items that exist in a similarly structured Wikibase instance, such as
#'   Wikidata.
#' @details This function is slightly different from create_item. That function
#'   creates a new item that may not have an equivalent item on another Wikibase
#'   instance, but it may well have an equivalent item in another database. \cr
#'   In this function, we use \code{qid_equivalence_property} for equivalence.
#'   In the more general create function we use \code{equivalence_item}, because
#'   we may use different identifiers. Similarly, the \code{qid_on_wikidata}
#'   replaces the more general \code{equivalence_id}, because we must use QID
#'   for identification in an other Wikibase instance.
#' @param qid_on_wikidata The QID of the item to be copied to your Wikibase. A
#'   single valid QID or a vector of several QIDs.
#' @param qid_equivalence_property The QID in Wikibase that records the
#'   equivalent Wikidata QID as an external ID.
#' @param language Defaults to \code{c("en", "nl", "hu")}. A character string of
#'   the languages in which the users wants to receive the labels and
#'   descriptions of the property. The vector of languages must use \href{https://en.wikipedia.org/wiki/IETF_language_tag}{BCP
#'   47}-compliant language tags (e.g., "en" for English, "nl" for Dutch and "hu"
#'   for Hungarian.)
#' @param classification_property The instance of, or subclass of, or superclass
#'   of property. Defaults to \code{NA_character} when not used.
#' @param classification_id The QID of the class. Defaults to
#'   \code{NA_character} when not used.
#' @param wikibase_api_url The full URL of the Wikibase API, which is the
#'   address that the \code{wbdataset} R client sends requests to when
#'   interacting with the knowledge base. For example,
#'   \code{'https://reprexbase.eu/demowiki/api.php'}. The URL must end with
#'   api.php.
#' @param data_curator The name of the data curator who runs the function and
#'   creates the log file, created with \link[utils]{person}.
#' @param log_file_name An explicitly stated full path to a possible CSV log
#'   file, defaults to \code{NULL}. If the value is \code{NULL}, no log file
#'   will be created.
#' @param csrf The CSRF token of your session, received with
#'   \code{\link{get_csrf}}.
#' @param wikibase_session An optional named list of default values to reuse
#'   across multiple function calls. If any of the main parameters (such as
#'   \code{language}, \code{data_curator}, \code{log_file_name},
#'   \code{equivalence_propeert}, \code{classification_property}
#'   \code{wikibase_api_url}, or \code{csrf}) are missing from the function
#'   call, their values will be taken from this list if available. This is
#'   useful in interactive workflows or scripts where the same context is
#'   reused.
#' @importFrom assertthat assert_that
#' @importFrom utils person
#' @return Returns a \code{\link[dataset]{dataset_df}} object.
#' The columns are:\cr
#' \describe{
#'  \item{\code{rowid}}{A row identifier. }
#'  \item{\code{action}}{\code{copy_wikidata_item}}
#'  \item{\code{id_on_target}}{The new item identifier (QID) on the targeted Wikibase.}
#'  \item{\code{label}}{The property label}
#'  \item{\code{description}}{The description label}
#'  \item{\code{language}}{The language code of the label.}
#'  \item{\code{datatype}}{The datatype of the property, for example, `string`}
#'  \item{\code{wikibase_api_url}}{The MediaWiki API URL where the new property is created.}
#'  \item{\code{equivalence_property}}{The PID that connects an equivalence ID to the property.}
#'  \item{\code{equivalence_id}}{The ID of an equivalent property defined elsewhere.}
#'  \item{\code{classification_property}}{The PID that connects the item to a superclass, or class.}
#'  \item{\code{classification_id}}{The QID of a class, subclass or superclass.}
#'  \item{\code{success}}{TRUE if successfully created, FALSE if there was an error.}
#'  \item{\code{comment}}{A summary of the error messages(s), if success is FALSE.}
#'  \item{\code{time}}{The time when the action started.}
#'  \item{\code{logfile}}{The name of the CSV logfile.}
#' }
#' The number of rows corresponds to the length of the qid_on_wikidata vector.
#' @export

copy_wikidata_item <- function(
    qid_on_wikidata = "Q4",
    qid_equivalence_property = "P35",
    language = c("en", "nl", "hu"),
    classification_property = NA_character_,
    classification_id = NA_character_,
    wikibase_api_url = "https://reprexbase.eu/jekyll/api.php",
    data_curator = NULL,
    log_file_name = NULL,
    csrf,
    wikibase_session = NULL) {

  language <- resolve_from_session("language", language, wikibase_session)
  data_curator <- resolve_from_session("data_curator", data_curator, wikibase_session)
  log_file_name <- resolve_from_session("log_file_name", log_file_name, wikibase_session)
  wikibase_api_url <- resolve_from_session("wikibase_api_url", wikibase_api_url, wikibase_session)
  equivalence_property <- resolve_from_session("wikibase_api_url", equivalence_property, wikibase_session)
  classification_property <- resolve_from_session("wikibase_api_url", classification_property, wikibase_session)
  csrf <- resolve_from_session("csrf", csrf, wikibase_session)

  validate_copy_entity_args(
    language = language,
    wikibase_api_url = wikibase_api_url,
    classification_property = classification_property,
    classification_id = classification_id,
    equivalence_property = equivalence_property,
    equivalence_id = equivalence_id,
    csrf = csrf,
    data_curator = data_curator,
    validated_action = "copy_wikidata_property()"
  )

  if (is.null(qid_equivalence_property)) qid_equivalence_property <- NA_character_

  if (length(qid_on_wikidata) > 1) {
    return_log_file <- copy_wikidata_items(
      qid_on_wikidata = qid_on_wikidata,
      qid_equivalence_property = qid_equivalence_property,
      classification_property = classification_property,
      classification_id = classification_id,
      language = language,
      wikibase_api_url = wikibase_api_url,
      data_curator = data_curator,
      csrf = csrf,
      wikibase_session = wikibase_session
    )

    return_log_file$rowid <- defined(
      return_log_file$id_on_target,
      label = "Wikibase QID",
      namespace = return_log_file$wikibase_api_url[1]
    )

    return(return_log_file)
  }

  # Timestamping ---------------------------------------------------------------------
  action_time <- Sys.time()
  # Save the time of running the code
  action_timestamp <- action_timestamp_create()
  if (is.null(log_file_name)) {
    log_file_name <- ""
  }


  # Assert that qid_on_wikidata looks like a QID
  qid_on_wikidata <- as.character(qid_on_wikidata)
  assertthat::assert_that(is_qid(qid_on_wikidata),
    msg = "qid_on_wikidata must start with Q followed by digits."
  )

  # Getting the data ---------------------------------------------------------

  claim_body <- list(
    action = "wbgetentities",
    ids = qid_on_wikidata,
    format = "json"
  )

  # purrr:safely is used for extended exception handling
  # it saves various error exceptions when using httr::POST
  safely_post <- purrr::safely(httr::POST, NULL)

  get_claim <- safely_post(
    "https://www.wikidata.org/w/api.php",
    body = claim_body,
    encode = "form"
  )

  if (!is.null(get_claim$error)) {
    # Some error
    message(get_claim$error)
  } else {
    response <- httr::content(get_claim$result,
      as = "parsed", type = "application/json"
    )
  }

  if (!is_response_success(response)) {
    # Exception: retrieval of the item was not successful, even though we did not
    # get an explicit error before.

    message("Could not access ", qid_on_wikidata)

    error_comments <- paste(
      unlist(
        lapply(response$error$messages, function(x) x$name)
      ),
      collapse = "|"
    )

    message(error_comments)

    return_dataframe <- data.frame(
      action = "copy_item",
      id_on_target = NA_character_,
      label = "<not retrieved>",
      description = "<not retrieved>",
      language = "<not retrieved>",
      datatype = "<not retrieved>",
      wikibase_api_url = wikibase_api_url,
      equivalence_property = qid_equivalence_property,
      equivalence_id = qid_on_wikidata,
      classification_property = classification_property,
      classification_id = classification_id,
      success = FALSE,
      comment = error_comments,
      time = action_timestamp,
      logfile = log_file_name
    )

    return(return_dataframe)
  }

  # We must determine which labels, descriptions, aliases actually exists
  # If the user wants to copy non-existing descriptions, we will replace them
  # with an empty string.

  labels_present <- language[which(language %in% names(response$entities[[1]]$labels))]
  labels_missing <- language[which(!language %in% names(response$entities[[1]]$labels))]

  descriptions_present <- language[which(language %in% names(response$entities[[1]]$descriptions))]
  descriptions_missing <- language[which(!language %in% names(response$entities[[1]]$descriptions))]

  aliases_present <- language[which(language %in% names(response$entities[[1]]$aliases))]
  aliases_missing <- language[which(!language %in% names(response$entities[[1]]$aliases))]
  labels_missing

  ## Set a default later, this is now hard coded to English but could be a parameter.

  if ("en" %in% names(response$entities[[1]]$labels)) {
    default_label <- response$entities[[1]]$labels$en$value
  } else {
    default_label <- response$entities[[1]]$sitelinks$enwiki$title
  }

  existing_item <- check_existing_item(
    action = "copy_item",
    search_term = default_label,
    language = "en",
    equivalence_property = qid_equivalence_property,
    equivalence_id = qid_on_wikidata,
    classification_property = classification_property,
    classification_id = classification_id,
    data_curator = data_curator,
    log_file_name = log_file_name,
    wikibase_api_url = wikibase_api_url,
    csrf = csrf
  )

  if (!is.null(existing_item)) {
    # return existing item
    return(existing_item)
  }

  message("Default label for ", qid_on_wikidata, ": ", default_label)
  labels_missing_list <- list()

  # The missing labels (i.e., translations that are missing for a label)
  # are replaced with the missing label, so we always have a label.

  for  (l in labels_missing) {
    labels_missing_list <- c(labels_missing_list, tmp = list(list(
      language = l,
      value = default_label
    )))
    names(labels_missing_list)[which(names(labels_missing_list) == "tmp")] <- l
  }

  labels_list <- c(response$entities[[1]]$labels[labels_present], labels_missing_list)

  # The missing description  (i.e., translations that are missing for a descriptions)
  # are replaced with an empty string.

  descriptions_missing_list <- list()

  for  (d in descriptions_missing) {
    descriptions_missing_list <- c(descriptions_missing_list,
      tmp = list(list(language = d, value = ""))
    )
    names(descriptions_missing_list)[which(names(descriptions_missing_list) == "tmp")] <- d
  }

  descriptions_list <- c(response$entities[[1]]$descriptions[descriptions_present], descriptions_missing_list)

  datastring <- item_identity_datastring_create(
    labels_list = labels_list,
    descriptions_list = descriptions_list
  )

  datastring <- jsonlite::toJSON(
    list(
      labels = labels_list,
      descriptions = descriptions_list,
      datatype = response$entities[[1]]$datatype
    ),
    auto_unbox = T
  )

  ## Getting the user's CSRF token for writing.
  ## See get_csrf, get_csrf_token.
  csrf_token <- get_csrf_token(csrf)


  if (!is_valid_csrf_token(csrf_token)) {
    stop(validated_action, ": the csrf appears to be invalid.")
  }

  # Posting the new item  ---------------------------------------------------
  new_item <- httr::POST(
    wikibase_api_url,
    body = list(
      action = "wbeditentity",
      new    = "item",
      data   = datastring,
      token  = csrf_token,
      format = "json"
    ),
    encode = "form",
    handle = csrf
  )

  # See if the created POST via wbeditentity was successful
  created_item_response <- httr::content(
    new_item,
    as = "parsed", type = "application/json"
  )

  # Creating the log file and the returned log data.frame ----------------------

  successful_post <- is_response_success(created_item_response)

  if (successful_post) {
    # Successfully created the item, try to add the equivalence statement
    # before returning log data.

    message(
      "Successfully created item ",
      created_item_response$entity$id, " (",
      created_item_response$entity$labels$en$value, ")"
    )

    if (is_pid(qid_equivalence_property)) {
      # If there is a meaningful equivalence property
      # add the equivalence statement to the target Wikibase instance.

      wikidata_qid_df <- add_id_statement(
        qid = created_item_response$entity$id,
        pid = qid_equivalence_property,
        o = qid_on_wikidata,
        wikibase_api_url = wikibase_api_url,
        csrf = csrf
      )
    }

    # Unwrap the newly created label from the response for checking...
    created_item_label <- created_item_response$entity$labels[1]
    # ... and the description, too.
    created_item_description <- created_item_response$entity$descriptions[1]

    return_dataframe <- data.frame(
      action = "copy_item",
      id_on_target = created_item_response$entity$id,
      label = created_item_label[[1]]$value,
      description = created_item_description[[1]]$value,
      language = created_item_label[[1]]$language,
      datatype = "wikibase-item",
      wikibase_api_url = wikibase_api_url,
      equivalence_property = qid_equivalence_property,
      equivalence_id = qid_on_wikidata,
      classification_property = classification_property,
      classification_id = classification_id,
      success = TRUE,
      comment = "",
      time = action_timestamp,
      logfile = log_file_name
    )
  } else if (
    # Case when we have clear message about a label conflict
    any(c(
      ("wikibase-validator-label-conflict" %in% unlist(created_item_response$error$messages)),
      ("wikibase-validator-label-with-description-conflict" %in% unlist(created_item_response$error$messages))
    ))
  ) {
    # Unwrap error message and send it to terminal
    message_strings <- unlist(created_item_response$error$messages)
    message(message_strings)

    # Get the old, conflicting QID out of the error message
    message_strings <- message_strings[which(grepl("Item:", message_strings))]
    pattern <- "\\[\\[Item:*(.*?)\\|"
    regmatchresult <- regmatches(message_strings, regexec(pattern, message_strings))
    old_qid <- regmatchresult[[1]][2]

    # Unwrap the error messages
    error_messages <- lapply(
      created_item_response$error$messages,
      function(x) unlist(x$parameters)
    )

    # Try to find the English error message
    error_language <- unlist(
      lapply(error_messages, function(x) ifelse(length(x) >= 2, x[2], NA_character_))
    )

    # Defaults for return data
    existing_label <- "<not retrieved>"
    existing_description <- "<not retrieved>"
    language <- "<not retrieved>"

    if ( # we have English-language error message
      any(error_language == "en")
    ) {
      # The error message contains the already existing (conflicting) label
      existing_label <- error_messages[[which(error_language == "en")]][1]
      language <- "en"
    } else if (any(!is.na(error_language))) {
      # The error message contains the already existing (conflicting) label
      # but not in English, select the first language that is available,
      # if there are any messages that can be read in a human language.
      nr_language <- which(!is.na(error_language))[1]
      existing_label <- error_messages[[nr_language]][1]
      language <- error_messages[[nr_language]][2]
    }

    return_dataframe <- data.frame(
      action = "copy_item",
      id_on_target = old_qid,
      label = existing_label,
      description = existing_description,
      language = language,
      datatype = "wikibase-item",
      wikibase_api_url = wikibase_api_url,
      equivalence_property = qid_equivalence_property,
      equivalence_id = qid_on_wikidata,
      classification_property = classification_property,
      classification_id = classification_id,
      success = FALSE,
      comment = "Wikibase validator label conflict: label-language pair already exists.",
      time = action_timestamp,
      logfile = log_file_name
    )
  } else {
    # Return an emptier data.frame if there was some error

    # Print out the error message verbatim to terminal
    message(created_item_response$error)

    # Wrap the main error types into the logfile and return data
    error_comments <- paste(
      unlist(
        lapply(created_item_response$error$messages, function(x) x$name)
      ),
      collapse = "|"
    )

    return_dataframe <- data.frame(
      action = "copy_item",
      id_on_target = NA_character_,
      label = "<not retrieved>",
      description = "<not retrieved>",
      language = "<not retrieved>",
      datatype = "wikibase-item",
      wikibase_api_url = wikibase_api_url,
      equivalence_property = qid_equivalence_property,
      equivalence_id = qid_on_wikidata,
      classification_property = classification_property,
      classification_id = classification_id,
      success = FALSE,
      comment = error_comments,
      time = action_timestamp,
      logfile = log_file_name
    )
  }


  description_text <- paste0(
    "Attempted and successful copying from Wikidata to ",
    wikibase_api_url, " with wbdataset:copy_wikidata_item() at ",
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
        "Wikibase Copy Item Log (",
        strftime(action_time, "%Y-%m-%d %H:%M:%OS0"), ")"
      ),
      creator = data_curator,
      dataset_date = Sys.Date()
    )
  )

  return_ds$rowid <- defined(paste0("wbi:", as.character(return_ds$id_on_target)),
    namespace = wikibase_api_url
  )

  if (!is.null(log_file_name) && nchar(log_file_name) > 0) {
    write_csv(return_dataframe,
      file = log_file_name,
      na = "NA",
      append = TRUE
    )
  }
  return_ds
}

#' @rdname  copy_wikidata_item
#' @keywords internal
copy_wikidata_items <- function(qid_on_wikidata,
                                qid_equivalence_property,
                                classification_property,
                                classification_id,
                                language,
                                wikibase_api_url,
                                data_curator,
                                csrf,
                                wikibase_session) {
  # Ensure that QIDs are used in the loop ----------------------------
  is_qid <- vapply(qid_on_wikidata, is_qid, logical(1))
  not_qid <- paste(names(which(!is_qid)), collapse = "|")

  assertthat::assert_that(
    not_qid == "",
    msg = paste0(
      "Error copy_wikidata_items(): ", not_qid,
      " does not appear to be a QID."
    )
  )

  if (!is.null(wikibase_session)) {
    # For repeated queries you can add your variables directly or in a list

    if (!is.null(wikibase_session$qid_equivalence_property)) {
      qid_equivalence_property <- wikibase_session$qid_equivalence_property
    }

    if (!is.null(wikibase_session$language)) {
      language <- wikibase_session$language
    }
    if (!is.null(wikibase_session$data_curator)) {
      data_curator <- wikibase_session$data_curator
    }

    if (!is.null(wikibase_session$wikibase_api_url)) {
      wikibase_api_url <- wikibase_session$wikibase_api_url
    }

    if (!is.null(wikibase_session$csrf)) {
      csrf <- wikibase_session$csrf
    }
  }

  returned_list <- lapply(
    qid_on_wikidata, function(x) {
      copy_wikidata_item(
        qid_on_wikidata = x,
        qid_equivalence_property = qid_equivalence_property,
        classification_property = classification_property,
        classification_id = classification_property,
        language = language,
        wikibase_api_url = wikibase_api_url,
        data_curator = data_curator,
        csrf = csrf
      )
    }
  )

  do.call(rbind, returned_list)
}
