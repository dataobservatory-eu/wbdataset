#' Copy Wikidata items to a Wikibase instance
#'
#' @description
#' Copy the labels and descriptions of one or more Wikidata items to
#' a target Wikibase instance, creating corresponding local items.
#' Optionally record their Wikidata identifiers and classification
#' relationships.
#'
#' @details
#' `copy_wikidata_item()` retrieves item metadata from Wikidata and
#' creates corresponding items in the target Wikibase instance.
#' The source items are identified by their Wikidata QIDs.
#'
#' Unlike [create_item()], which creates items from locally supplied
#' metadata, this function uses existing Wikidata items as the source
#' of labels, descriptions, and identifiers.
#'
#' The function supports individual QIDs and vectors of QIDs. When
#' multiple QIDs are supplied, the items are processed individually.
#'
#' For each source item, the function:
#' \enumerate{
#'   \item Retrieves labels and descriptions from Wikidata.
#'   \item Checks whether an equivalent item already exists in the
#'     target Wikibase instance.
#'   \item Creates a new item if no existing equivalent is found.
#'   \item Optionally adds a statement recording the Wikidata QID.
#'   \item Optionally adds a classification statement.
#'   \item Returns a structured record of the operation.
#' }
#'
#' The `qid_equivalence_property` identifies the property on the
#' target Wikibase instance used to record the source Wikidata QID
#' as an external identifier. This differs from the more general
#' equivalence identifiers supported by [create_item()].
#'
#' Labels are requested in the languages specified by `language`.
#' When a requested label is unavailable, the function substitutes
#' the source item's English label, or its English Wikipedia sitelink
#' title if no English label exists. Missing descriptions are
#' represented by empty strings.
#'
#' The function retrieves source metadata from the public Wikidata
#' API. Authentication is required for writing to the target Wikibase
#' instance. Reading from password-protected source instances is
#' not currently supported.
#'
#' A CSV log can optionally be written to record the results of
#' individual operations. Existing log files are appended to.
#'
#' @param qid_on_wikidata Character vector. One or more Wikidata item
#'   identifiers (QIDs), such as `"Q42"`.
#' @param qid_equivalence_property Character string. Property identifier
#'   (PID) on the target Wikibase instance used to record the
#'   corresponding Wikidata QID as an external identifier.
#'   May be `NULL` if no equivalence statement is required.
#' @param language Character vector of requested languages for labels
#'   and descriptions. Defaults to `"en"`. Languages must be expressed
#'   using \href{https://www.rfc-editor.org/rfc/bcp/bcp47.txt}{
#'   BCP 47 language tags}, such as `"en"`, `"nl"`, or `"hu"`.
#' @param classification_property Character string. Property identifier
#'   (PID) used to express a classification relationship, such as
#'   instance of or subclass of. Defaults to `NA_character_`.
#' @param classification_id Character string. QID of the class or
#'   superclass to which the new item should be linked.
#'   Defaults to `NA_character_`.
#' @param wikibase_api_url Character string. Full URL of the target
#'   MediaWiki API endpoint, ending in `api.php`. May be supplied
#'   directly or through `wikibase_session`.
#' @param data_curator A [utils::person()] object identifying the
#'   curator responsible for the operation. May be supplied directly
#'   or through `wikibase_session`. If omitted, defaults to
#'   `person("Unknown", "Curator", role = "dtm")`.
#' @param log_file_name Character string or `NULL`. Path to a CSV log
#'   file. When specified, operation results are appended to the file.
#'   Defaults to `NULL`, which disables file logging.
#' @param csrf Authenticated HTTP response returned by [get_csrf()].
#'   Used to authorize edits on the target Wikibase instance.
#'   May be supplied directly or through `wikibase_session`.
#' @param wikibase_session Optional named list of reusable session
#'   parameters, including language preferences, authentication,
#'   target API URL, curator information, and property identifiers.
#'   Values are resolved using the package's session-resolution
#'   mechanism.
#'
#' @return
#' A [dataset::dataset_df()] containing the results of the copying
#' operation. Its columns include:
#' \describe{
#'   \item{`rowid`}{Row identifier associated with the target item.}
#'   \item{`action`}{Operation identifier, `"copy_item"`.}
#'   \item{`id_on_target`}{QID of the item on the target Wikibase.}
#'   \item{`label`}{Label of the target item.}
#'   \item{`description`}{Description of the target item.}
#'   \item{`language`}{Language code of the returned label.}
#'   \item{`datatype`}{Entity datatype, typically `"wikibase-item"`.}
#'   \item{`wikibase_api_url`}{API endpoint of the target Wikibase.}
#'   \item{`equivalence_property`}{PID used to record equivalence.}
#'   \item{`equivalence_id`}{Wikidata QID of the source item.}
#'   \item{`classification_property`}{PID of the classification relation.}
#'   \item{`classification_id`}{QID of the class or superclass.}
#'   \item{`success`}{Logical success indicator.}
#'   \item{`comment`}{Error or status information.}
#'   \item{`time`}{Timestamp of the operation.}
#'   \item{`logfile`}{Name of the CSV log file, if specified.}
#' }
#'
#' For a vector of source QIDs, the function returns the combined
#' results of the individual copying operations.
#'
#' @seealso
#' [create_item()], [get_csrf()], [add_id_statement()],
#' [add_item_statement()]
#'
#' @importFrom assertthat assert_that
#' @importFrom utils person
#' @export

copy_wikidata_item <- function(
  qid_on_wikidata,
  qid_equivalence_property,
  language = "en",
  classification_property = NA_character_,
  classification_id = NA_character_,
  wikibase_api_url = NULL,
  data_curator = NULL,
  log_file_name = NULL,
  csrf = NULL,
  wikibase_session = NULL
) {
  if (is.null(qid_equivalence_property)) qid_equivalence_property <- NA_character_
  validated_action <- "copy_wikidata_item()"

  language <- resolve_from_session("language", language, wikibase_session)
  data_curator <- resolve_from_session("data_curator", data_curator, wikibase_session)
  log_file_name <- resolve_from_session("log_file_name", log_file_name, wikibase_session)
  wikibase_api_url <- resolve_from_session("wikibase_api_url", wikibase_api_url, wikibase_session)
  classification_property <- resolve_from_session("wikibase_api_url", classification_property, wikibase_session)
  csrf <- resolve_from_session("csrf", csrf, wikibase_session)
  qid_equivalence_property <- resolve_from_session("qid_equivalence_property", qid_equivalence_property, wikibase_session)

  if (is.null(data_curator)) {
    data_curator <- person("Unknown", "Curator", role = "dtm")
  }

  validate_copy_entity_args(
    language = language,
    wikibase_api_url = wikibase_api_url,
    classification_property = classification_property,
    classification_id = classification_id,
    equivalence_property = NA_character_,
    equivalence_id = NA_character_,
    csrf = csrf,
    data_curator = data_curator,
    validated_action = validated_action
  )

  # Case when multiple items need to be copied ------------
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

  # Assert that qid_on_wikidata looks like a QID ----------------------
  qid_on_wikidata <- as.character(qid_on_wikidata)
  assert_that(is_qid(qid_on_wikidata),
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

  # The data is always received from Wikidata -------------------
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

    if (is_pid(classification_property) && is_qid(classification_id)) {
      ## If there is a classification property like instance of or
      ## subclass of, then try to add a statement about it.
      add_item_statement(
        qid = created_item_response$entity$id,
        pid = classification_property,
        o = classification_id,
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
