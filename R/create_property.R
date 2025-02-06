#' @title Create a property
#' @description
#' Creates a property entity on a Wikibase instance with a single language
#' labelling and description. New labels and descriptions can be added
#' in further languages later. This is a wrapper for
#'  \href{https://www.wikidata.org/w/api.php?action=help&modules=wbeditentity}{MediaWiki action=wbeditentity}
#' @details
#' Optionally, the function adds an equivalent property to this newly created
#' property.
#' @param language A single label, for example,  \code{"duration"}.
#' @param description A single description, for example,  \code{"Length in time."}.
#' @param language A single language code, for example, \code{"en"}.
#' @param datatype A single character string defining the datatype of this
#' property, for example, \code{"quantity".}
#' @param equivalence_property An optional PID of a property already defined in
#' the same Wikibase instance that records the equivalence of this new property
#' with a property defined elsewhere, for example, on Wikidata or CIDOC-CRM.
#' Defaults to
#' \code{NA_character_}; if left missing, no equivalence relations is will be
#' claimed.
#' @param equivalence_id An optional PID or other identifier in Wikibase
#' that records the equivalence of the property. An external ID. Defaults to
#' \code{NA_character_}; if left missing, no equivalence relations is will be
#' claimed.
#' @param wikibase_api_url For example, \code{'https://reprexbase.eu/demowiki/api.php'}.
#' @param csrf The CSRF token of your session, received with \code{\link{get_csrf}}.
#' @param log_path A path to save the log file. Defaults to the return value
#' of \code{\link{tempdir()}}.
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
                            wikibase_api_url,
                            log_path = tempdir(),
                            csrf) {

  # Save the time of running the code
  action_time <- action_timestamp_create()
  log_file_name <- paste0("wbdataset_create_property_", action_timestamp, ".csv")

  if ( !is.na(equivalence_id) ) {
    # If there is an equivalence ID, for example, a PID on Wikidata, than the
    # equivalence property that connects this ID to the newly created property
    # must be given, too.
    assertthat::assert_that(
      ! is.na(equivalence_property),
      msg = "create_property() cannot add equivalence_id statement without equivalence_property.")
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

  assertthat::assert_that(!is.null(csrf_token),
                          msg = "You do not have a CSRF token"
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
                                             type = "application/json")
  created_property_response

  if (is_response_success(created_property_response)) {
    # Successfully created the property
    message("Successfully created item ", created_property_response$entity$id, " (", created_property_response$entity$labels$en$value, ")")

    if (is.na(equivalence_property)) {
      # If equivalence must be handled, not rewritten yet
      wikidata_pid_df <- add_id_statement(
        qid = created_property_response$entity$id,
        pid = equivalence_id,
        o = equivalence_property,
        wikibase_api_url = wikibase_api_url,
        csrf = csrf
      )
    }

    return_dataframe <- data.frame(
      action = "create_property",
      id_on_target = created_property_response$entity$id,
      label = label,
      description =  created_property_response$entity$descriptions[[1]]$value,
      language =  created_property_response$entity$descriptions[[1]]$language,
      datatype = created_property_response$entity$datatype,
      wikibase_api_url = wikibase_api_url,
      equivalence_property =  equivalence_property,
      equivalence_id = equivalence_id,
      success = TRUE,
      comment = NA_character_,
      time = action_time,
      logfile = log_file_name
    )

    write.csv(return_dataframe,
              file = file.path(log_path, log_file_name),
              row.names=FALSE,
              na = "NA",
              fileEncoding = "UTF-8")

    return_dataframe
  } else if (
    "wikibase-validator-label-conflict" %in% unlist(created_property_response$error$messages)
    ) {
    # wikibase-validator-label-conflict: the property already exists, and this
    # information should be returned to the user.
    message_strings <- unlist(created_property_response$error$messages)
    message(message_strings)
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
      success = FALSE,
      comment = "wikibase-validator-label-conflict, the label-language pair already exists.",
      time = action_time,
      logfile = log_file_name
    )

    write.csv(return_dataframe,
              file = file.path(log_path, log_file_name),
              row.names=FALSE,
              na = "NA",
              fileEncoding = "UTF-8")
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
      success = FALSE,
      comment = error_comments,
      time = action_time,
      logfile = log_file_name
    )

    write.csv(return_dataframe,
              file = file.path(log_path, log_file_name),
              row.names=FALSE,
              na = "NA",
              fileEncoding = "UTF-8")
  }

  return_dataframe
}
