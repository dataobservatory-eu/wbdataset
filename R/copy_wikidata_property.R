#' @title Copy a Wikidata property
#' @description
#' This code will copy a property label and description from Wikidata to a
#' new instance. It should work between instances, but the authentication to
#' copy from a password protected instance is not yet coded. The function is more
#' specific than \code{\link{create_property}}, because this one creates
#' properties that exist in a similarly structured Wikibase instance, such as
#' Wikidata.
#' @details This function is slightly different from \code{\link{create_property}}.
#' That function creates a new property that may not have an equivalent property
#' on another Wikibase instance, but it may well have an equivalent property in
#' another graph system. \cr
#' In this function, we use \code{pid_equivalence_property}
#' for equivalence. In the more general create function we use
#' \code{equivalence_property}, because we may use different identifiers.
#' Similarly, the \code{pid_on_wikidata} replaces the more general
#' \code{equivalence_id}, because we must use PID for identification in an
#' other Wikibase instance.
#' @param pid_on_wikidata The PID of the property on Wikidata to be copied to
#' your Wikibase. (Only works with non-authenticated sources, this should be
#' changed.)
#' @param pid_equivalence_property The PID in Wikibase that records the equivalent Wikidata
#' PID as an external ID.
#' @param language A vector of languages codes, for example, \code{c("en", "et")}.
#' @param wikibase_api_url For example, \code{'https://reprexbase.eu/demowiki/api.php'}.
#' @param csrf The CSRF token of your session, received with \code{\link{get_csrf}}.
#' @export
#' @return
#' Currently returns a data.frame, this should be a dataset.
#' The columns are:
#' \itemize{
#'  \item{"action"}{ create_property}
#'  \item{"id_on_target"}{ The new Property Identifier (PID) on the targeted Wikibase.}
#'  \item{"label"}{ The propery label}
#'  \item{"description"}{ The description label}
#'  \item{"language"}{ The language code of the label.}
#'  \item{"datatype"}{ The datatype of the property, for example, `string`}
#'  \item{"wikibase_api_url"}{ The MediaWiki API URL where the new property is created.}
#'  \item{"equivalence_property"}{ The PID that connects an equivalence ID to the property.}
#'  \item{"equivalence_id"}{ The ID of an equivalent property defined elsewhere.}
#'  \item{"success"}{ TRUE if successfully created, FALSE if there was an error.}
#'  \item{"comment"}{ A summary of the error messages(s), if success is FALSE.}
#'  \item{"time"}{ The time when the action started.}
#'  \item{"logfile"}{ The name of the CSV logfile.}
#' }

copy_wikidata_property <- function(
    pid_on_wikidata,
    pid_equivalence_property = "P2",
    languages = c("en", "hu"),
    wikibase_api_url = "https://reprexbase.eu/jekyll/api.php",
    log_path = tempdir(),
    csrf) {
  # Save the time of running the code
  action_timestamp <- action_timestamp_create()
  log_file_name <- paste0("wbdataset_copy_wikibase_property_", action_timestamp, ".csv")

  # Assert that pid_on_wikidata makes sense
  pid_on_wikidata <- as.character(pid_on_wikidata)
  assertthat::assert_that(is_pid(pid_on_wikidata),
    msg = "pid_on_wikidata must start with P followed by digits."
  )

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
  } else {
    # there was no error
    response <- httr::content(get_claim$result, as = "parsed", type = "application/json")
  }

  if (!is_response_success(response)) {
    # Exception: retrieval of the property was not successful, even though we did not
    # get an explicit error before.
    message("Could not access ", pid_on_wikidata)

    error_comments <- paste(
      unlist(
        lapply(response$error$messages, function(x) x$name)
      ),
      collapse = "|"
    )

    return_dataframe <- data.frame(
      action = "copy_property",
      id_on_target = pid_on_wikidata,
      label = "<not retrieved>",
      description = "<not retrieved>",
      language = "<not retrieved>",
      datatype = "<not retrieved>",
      wikibase_api_url = wikibase_api_url,
      equivalence_property = equivalence_property,
      equivalence_id = equivalence_id,
      success = FALSE,
      comment = error_comments,
      time = action_timestamp,
      logfile = log_file_name
    )

    write.csv(return_dataframe,
      file = file.path(log_path, log_file_name),
      row.names = FALSE,
      na = "NA",
      fileEncoding = "UTF-8"
    )

    return(return_dataframe)
  }

  # aliases: response$entities[[1]]$aliases

  ## We must determine which labels, descriptions, aliases actually exists
  ## If the user wants to copy non-existing descriptions, we will replace them
  ## with an empty string.

  labels_present <- languages[which(languages %in% names(response$entities[[1]]$labels))]
  labels_missing <- languages[which(!languages %in% names(response$entities[[1]]$labels))]

  descriptions_present <- languages[which(languages %in% names(response$entities[[1]]$descriptions))]
  descriptions_missing <- languages[which(!languages %in% names(response$entities[[1]]$descriptions))]

  aliases_present <- languages[which(languages %in% names(response$entities[[1]]$aliases))]
  aliases_missing <- languages[which(!languages %in% names(response$entities[[1]]$aliases))]
  labels_missing

  ## Set a default later, this is now hard coded to English but could be a parameter.

  if ("en" %in% names(response$entities[[1]]$labels)) {
    default_label <- response$entities[[1]]$labels$en$value
  } else {
    default_label <- response$entities[[1]]$sitelinks$enwiki$title
  }

  message("Default label for ", pid_on_wikidata, ": ", default_label)
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

  datastring

  ## Getting the user's CSRF token for writing.
  ## See get_csrf, get_csrf_token.
  csrf_token <- get_csrf_token(csrf)

  assertthat::assert_that(!is.null(csrf_token),
    msg = "You do not have a CSRF token; perhaps your session has expired.
    Try get_csrf() with your credentials."
  )

  assertthat::assert_that(nchar(csrf_token) == 42,
    msg = "Your CSRF token should have 42 characters."
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
    # before returning
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
      success = TRUE,
      comment = "",
      time = action_timestamp,
      logfile = log_file_name
    )

    write.csv(return_dataframe,
      file = file.path(log_path, log_file_name),
      row.names = FALSE,
      na = "NA",
      fileEncoding = "UTF-8"
    )
  } else if ("wikibase-validator-label-conflict" %in% unlist(created_property_response$error$messages)) {
    # Unwrap error message and send it to terminal
    message_strings <- unlist(created_property_response$error$messages)
    message(message_strings)

    # Get the old, conflicting PID out of the error message
    message_strings <- message_strings[which(grepl("Property:", message_strings))]
    pattern <- "\\[\\[Property:*(.*?)\\|"
    result <- regmatches(message_strings, regexec(pattern, message_strings))
    old_pid <- result[[1]][2]

    # Unwrap the error messages
    error_messages <- lapply(
      created_property_response$error$messages,
      function(x) unlist(x$parameters)
    )

    # Try to find the English error message
    error_languages <- unlist(
      lapply(error_messages, function(x) ifelse(length(x) >= 2, x[2], NA_character_))
    )

    # Defaults for return data
    existing_label <- "<not retrieved>"
    existing_description <- "<not retrieved>"
    language <- "<not retrieved>"

    if ( # we have English-language error message
      any(error_languages == "en")
    ) {
      # The error message contains the already existing (conflicting) label
      existing_label <- error_messages[[which(error_languages == "en")]][1]
      language <- "en"
    } else if (any(!is.na(error_languages))) {
      # The error message contains the already existing (conflicting) label
      # but not in English, select the first language that is available,
      # if there are any messages that can be read in a human language.
      nr_language <- which(!is.na(error_languages))[1]
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
      datatype = "<not retrieved>",
      wikibase_api_url = wikibase_api_url,
      equivalence_property = pid_equivalence_property,
      equivalence_id = pid_on_wikidata,
      success = FALSE,
      comment = "wikibase-validator-label-conflict, the label-language pair already exists.",
      time = action_timestamp,
      logfile = log_file_name
    )

    # Save the log file
    write.csv(return_dataframe,
      file = file.path(log_path, log_file_name),
      row.names = FALSE,
      na = "NA",
      fileEncoding = "UTF-8"
    )
  } else { # Return an emptier data.frame if there was some error

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
      datatype = "<not retrieved>",
      wikibase_api_url = wikibase_api_url,
      equivalence_property = pid_equivalence_property,
      equivalence_id = pid_on_wikidata,
      success = FALSE,
      comment = error_comments,
      time = action_timestamp,
      logfile = log_file_name
    )

    # Save the log file
    write.csv(return_dataframe,
      file = file.path(log_path, log_file_name),
      row.names = FALSE,
      na = "NA",
      fileEncoding = "UTF-8"
    )
  }

  # Return the results
  return_dataframe
}
