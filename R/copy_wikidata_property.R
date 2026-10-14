#' Copy Wikidata properties to a Wikibase instance
#'
#' @description
#' Copy one or more Wikidata properties, including their datatypes, labels,
#' and descriptions, to a target Wikibase instance. A single call can request
#' several languages for each property.
#'
#' @details
#' Unlike [create_property()], this function uses Wikidata property IDs to
#' retrieve the source definitions. The source is the public Wikidata API;
#' authentication for private source instances is not implemented.
#'
#' For each requested language, the function copies the available label and
#' description. If a label is unavailable, it uses the English label (or the
#' English Wikipedia sitelink title when no English label exists). Missing
#' descriptions become empty strings.
#'
#' All requested translations are submitted together when a new property is
#' created. If the target property already exists, the function returns it
#' without adding or updating translations.
#'
#' @param pid_on_wikidata One or more Wikidata property IDs, such as `"P31"`
#'   or `c("P31", "P279")`.
#' @param pid_equivalence_property Target Wikibase property ID used to record
#'   the corresponding Wikidata PID as an external identifier.
#' @param language Character vector of BCP 47 language tags identifying the
#'   languages whose labels and descriptions should be copied. Defaults to
#'   `"en"`. Use, for example, a character vector`c("en", "hu", "de")` to copy
#'   three languages in a single call.
#'   Missing labels fall back to the default English label;
#'   missing descriptions are stored as empty strings. This argument does not
#'   add translations to properties that already exist on the target.
#' @param wikibase_api_url Full target Wikibase API URL ending in `api.php`.
#'   Can be supplied through `wikibase_session`.
#' @param classification_property Optional classification PID. Defaults to
#'   `NA_character_`; no classification statement is created by this function.
#' @param classification_id Optional classification QID. Defaults to
#'   `NA_character_`; no classification statement is created by this function.
#' @param equivalence_property Optional equivalence PID, defaulting to
#'   `NA_character_`. Wikidata PID equivalence is recorded using
#'   `pid_equivalence_property` instead.
#' @param equivalence_id Optional external identifier, defaulting to
#'   `NA_character_`. The source is identified by `pid_on_wikidata`.
#' @param data_curator Curator responsible for the operation, represented by
#'   [utils::person()]. If omitted, an unknown curator is used. Can be
#'   supplied through `wikibase_session`.
#' @param log_file_name Optional CSV log path. `NULL` (the default) disables
#'   file logging.
#' @param csrf Authenticated session handle obtained with [get_csrf()]. Can
#'   be supplied through `wikibase_session`.
#' @param wikibase_session Optional named list of shared session defaults,
#'   including language, curator, API URL, equivalence settings, and CSRF
#'   handle. Explicit arguments and session values are resolved by the
#'   existing implementation.
#'
#' @returns
#' A `dataset_df` recording the copy operation. Its columns are:
#'
#' - `rowid`: Row identifier.
#' - `action`: Copy operation name.
#' - `id_on_target`: PID on the target Wikibase.
#' - `label`, `description`, `language`: One reported label, description,
#'   and language for the copied property.
#' - `datatype`: Wikibase property datatype.
#' - `wikibase_api_url`: Target API URL.
#' - `equivalence_property`, `equivalence_id`: Equivalence metadata.
#' - `classification_property`, `classification_id`: Classification fields.
#' - `success`, `comment`: Operation status and diagnostic message.
#' - `time`, `logfile`: Operation timestamp and optional log path.
#'
#' For multiple source PIDs, results are combined across properties. The
#' returned log is **not** expanded to one row per language, even though
#' multiple translations can be created for each new property.
#'
#' @importFrom assertthat assert_that
#' @importFrom utils person write.table
#' @export


copy_wikidata_property <- function(
  pid_on_wikidata,
  pid_equivalence_property,
  language = "en",
  wikibase_api_url = NULL,
  classification_property = NA_character_,
  classification_id = NA_character_,
  equivalence_property = NA_character_,
  equivalence_id = NA_character_,
  data_curator = NULL,
  log_file_name = NULL,
  csrf = NULL,
  wikibase_session = NULL
) {
  # Resolve inputs from explicit parameters or session parameter list ---

  language <- resolve_from_session("language", language, wikibase_session)
  data_curator <- resolve_from_session("data_curator", data_curator, wikibase_session)
  log_file_name <- resolve_from_session("log_file_name", log_file_name, wikibase_session)
  wikibase_api_url <- resolve_from_session("wikibase_api_url", wikibase_api_url, wikibase_session)
  classification_property <- resolve_from_session("classification_property", classification_property, wikibase_session)
  equivalence_property <- resolve_from_session("equivalence_property", equivalence_property, wikibase_session)
  pid_equivalence_property <- resolve_from_session("pid_equivalence_property", pid_equivalence_property, wikibase_session)

  csrf <- resolve_from_session("csrf", csrf, wikibase_session)

  if (is.null(data_curator)) {
    data_curator <- person("Unknown", "Curator", role = "dtm")
  }

  # Validate the parameters before API interaction -----------------

  validate_copy_entity_args(
    language = language,
    wikibase_api_url = wikibase_api_url,
    classification_property = classification_property,
    classification_id = classification_id,
    equivalence_property = equivalence_property,
    equivalence_id = equivalence_id, # not used in this function
    csrf = csrf,
    data_curator = data_curator,
    validated_action = "copy_wikidata_property()"
  )

  property_wikibase_datatype <- "<not retrieved>" # set default value

  # If many PIDs are given, loop the interaction --------

  if (length(pid_on_wikidata) > 1) {
    return_log_file <- copy_wikidata_properties(
      pid_on_wikidata = pid_on_wikidata,
      pid_equivalence_property = pid_equivalence_property,
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

  # Assert that pid_on_wikidata makes sense -------------------------
  pid_on_wikidata <- as.character(pid_on_wikidata)
  assertthat::assert_that(is_pid(pid_on_wikidata),
    msg = "pid_on_wikidata must start with P followed by digits."
  )

  # Create a claim body to check if the property alrady exists
  claim_body <- list(
    action = "wbgetentities",
    ids = pid_on_wikidata,
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
    # there was an error
    message(get_claim$error)
  } else {
    # there was no error
    response <- httr::content(
      get_claim$result,
      as = "parsed",
      type = "application/json"
    )
  }

  # Exception: retrieval of the property was not successful,
  # even though we did not get an explicit error before.
  if (!is_response_success(response)) {
    message("Could not access ", pid_on_wikidata)
    error_comments <- paste(
      unlist(
        lapply(response$error$messages, function(x) x$name)
      ),
      collapse = "|"
    )

    return_dataframe <- data.frame(
      action = "copy_property",
      id_on_target = NA_character_,
      label = "<not retrieved>",
      description = "<not retrieved>",
      language = "<not retrieved>",
      datatype = "<not retrieved>",
      wikibase_api_url = wikibase_api_url,
      equivalence_property = equivalence_property,
      equivalence_id = pid_on_wikidata,
      classification_property = NA_character_,
      classification_id = NA_character_,
      success = FALSE,
      comment = error_comments,
      time = action_timestamp,
      logfile = log_file_name
    )
  }

  # aliases: response$entities[[1]]$aliases

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

  message("Default label for ", pid_on_wikidata, ": ", default_label)


  # Check if such a property exists on the target where you want to
  # copy from Wikidata -------------------------------------------

  existing_property <- check_existing_property(
    action = "copy_property",
    search_term = default_label,
    language = "en",
    equivalence_property = pid_equivalence_property,
    equivalence_id = pid_on_wikidata,
    classification_property = NA_character_,
    classification_id = NA_character_,
    data_curator = data_curator,
    log_file_name = log_file_name,
    wikibase_api_url = wikibase_api_url,
    csrf = csrf
  )

  if (!is.null(existing_property)) {
    # return existing item
    return(existing_property)
  }

  # If the property does not exit, go on  ---------------------

  labels_missing_list <- list()

  # The missing labels (i.e., translations that are missing for a label)
  # are replaced with the missing label, so we always have a label.

  for  (l in labels_missing) {
    # The missing labels will received the default (en) label, because they
    # should not be empty
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

  descriptions_list <- c(
    response$entities[[1]]$descriptions[descriptions_present],
    descriptions_missing_list
  )

  datastring <- property_identity_datastring_create(
    # Internal function that converts the lists to the JSON format
    # required by the wbeditidentity API call.
    labels_list = labels_list,
    descriptions_list = descriptions_list,
    datatype = response$entities[[1]]$datatype
  )

  property_wikibase_datatype <- response$entities[[1]]$datatype
  datastring

  ## Getting the user's CSRF token for writing.
  ## See get_csrf, get_csrf_token.
  csrf_token <- get_csrf_token(csrf)

  assertthat::assert_that(!is.null(csrf_token),
    msg = "You do not have a CSRF token; perhaps your session has expired.
    Try get_csrf() with your credentials."
  )

  assertthat::assert_that(nchar(csrf_token) > 10,
    msg = "Your CSRF token is usually, but not always 42 characters long."
  )

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
    type = "application/json"
  )

  if (is_response_success(created_property_response)) {
    # Successfully created the property, try to add the equivalence statement
    # before returning log data.

    message(
      "Successfully created item ",
      created_property_response$entity$id, " (",
      created_property_response$entity$labels$en$value, ")"
    )

    if (is_pid(pid_equivalence_property)) {
      wikidata_pid_df <- add_id_statement(
        qid = created_property_response$entity$id,
        pid = pid_equivalence_property,
        o = pid_on_wikidata,
        wikibase_api_url = wikibase_api_url,
        csrf = csrf
      )
    }

    # Unwrap the newly created label from the response for checking...
    created_item_label <- created_property_response$entity$labels[1]
    # ... and the description, too.
    created_item_description <- created_property_response$entity$descriptions[1]

    return_dataframe <- data.frame(
      action = "copy_property",
      id_on_target = created_property_response$entity$id,
      label = created_item_label[[1]]$value,
      description = created_item_description[[1]]$value,
      language = created_item_label[[1]]$language,
      datatype = created_property_response$entity$datatype,
      wikibase_api_url = wikibase_api_url,
      equivalence_property = pid_equivalence_property,
      equivalence_id = pid_on_wikidata,
      classification_property = NA_character_,
      classification_id = NA_character_,
      success = TRUE,
      comment = "",
      time = action_timestamp,
      logfile = ifelse(is.null(log_file_name), "", log_file_name)
    )
  } else if ("wikibase-validator-label-conflict" %in% unlist(created_property_response$error$messages)) {
    # Unwrap error message and send it to terminal
    message_strings <- unlist(created_property_response$error$messages)
    message(message_strings)

    # Get the old, conflicting PID out of the error message
    message_strings <- message_strings[which(grepl("Property:", message_strings))]
    pattern <- "\\[\\[Property:*(.*?)\\|"
    regmatchresult <- regmatches(message_strings, regexec(pattern, message_strings))
    old_pid <- regmatchresult[[1]][2]

    # Unwrap the error messages
    error_messages <- lapply(
      created_property_response$error$messages,
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

    # Create a return dataset that describes the conflict
    return_dataframe <- data.frame(
      action = "copy_property",
      id_on_target = old_pid,
      label = existing_label,
      description = existing_description,
      language = language,
      datatype = property_wikibase_datatype,
      wikibase_api_url = wikibase_api_url,
      equivalence_property = pid_equivalence_property,
      equivalence_id = pid_on_wikidata,
      classification_property = NA_character_,
      classification_id = NA_character_,
      success = FALSE,
      comment = "Wikibase validator label conflict: label-language pair already exists.",
      time = action_timestamp,
      logfile = ifelse(is.null(log_file_name), "", log_file_name)
    )
  } else {
    # Return an emptier data.frame if there was some error

    # Print out the error message verbatim to terminal
    message(created_property_response$error)

    # Wrap the main error types into the logfile and return data
    error_comments <- paste(
      unlist(
        lapply(created_property_response$error$messages, function(x) x$name)
      ),
      collapse = "|"
    )

    return_dataframe <- data.frame(
      action = "copy_property",
      id_on_target = NA_character_,
      label = "<not retrieved>",
      description = "<not retrieved>",
      language = "<not retrieved>",
      datatype = property_wikibase_datatype,
      wikibase_api_url = wikibase_api_url,
      equivalence_property = pid_equivalence_property,
      equivalence_id = pid_on_wikidata,
      classification_property = NA_character_,
      classification_id = NA_character_,
      success = FALSE,
      comment = error_comments,
      time = action_timestamp,
      logfile = ifelse(is.null(log_file_name), "", log_file_name)
    )
  }

  description_text <- paste0(
    "Attempted and successful copying from Wikidata to ",
    wikibase_api_url, " with wbdataset:copy_wikidata_property() at ",
    substr(as.character(action_time), 1, 19)
  )

  # Return the results
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
        "Wikibase Copy Property Log (",
        strftime(action_time, "%Y-%m-%d %H:%M:%OS0"), ")"
      ),
      description_text,
      creator = data_curator,
      dataset_date = Sys.Date()
    ),
    identifier = c(wbi = wikibase_api_url)
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
copy_wikidata_properties <- function(
  pid_on_wikidata,
  pid_equivalence_property,
  language,
  wikibase_api_url,
  data_curator,
  csrf,
  wikibase_session = NULL
) {
  # Ensure that PIDs are used in the loop ----------------------------
  is_pid <- vapply(pid_on_wikidata, is_pid, logical(1))
  not_pid <- paste(names(which(!is_pid)), collapse = "|")

  assertthat::assert_that(
    not_pid == "",
    msg = paste0(
      "Error copy_wikidata_properties(): ", not_pid,
      " does not appear to be a PID."
    )
  )

  if (!is.null(wikibase_session)) {
    # For repeated queries you can add your variables directly or in a list

    if (!is.null(wikibase_session$pid_equivalence_property)) {
      pid_equivalence_property <- wikibase_session$pid_equivalence_property
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
    pid_on_wikidata, function(x) {
      copy_wikidata_property(
        pid_on_wikidata = x,
        pid_equivalence_property = pid_equivalence_property,
        language = language,
        wikibase_api_url = wikibase_api_url,
        data_curator = data_curator,
        csrf = csrf
      )
    }
  )

  do.call(rbind, returned_list)
}
