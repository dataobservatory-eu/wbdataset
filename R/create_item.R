#' @title Create a property
#' @description Creates an item entity on a Wikibase instance with a single
#' language labelling and description. New labels and descriptions can be added
#' in further languages later. This is a wrapper for
#' \href{https://www.wikidata.org/w/api.php?action=help&modules=wbeditentity}{MediaWiki
#' action=wbeditentity}
#' @details Optionally, the function adds an equivalent item to this newly
#' created item.
#' @param language A single label, for example, \code{"human"}.
#' @param description A single description, for example, \code{"A member of the
#' homo sapience species."}.
#' @param language A single language code, for example, \code{"en"}.
#' @param equivalence_property An optional PID of a property already defined in
#'   the same Wikibase instance that records the equivalence of this new
#'   item with a property defined elsewhere, for example, on Wikidata or
#'   CIDOC-CRM. Defaults to \code{NA_character_}; if left missing, no
#'   equivalence relations is will be claimed.
#' @param equivalence_id The identifier that uniquely identifies this item among
#' another system's defintions. Defaults to
#'   \code{NA_character_}; if left missing, no equivalence relations is will be
#'   claimed.
#' @param wikibase_api_url For example,
#'   \code{'https://reprexbase.eu/demowiki/api.php'}.
#' @param data_curator The name of the data curator who runs the function and
#'   creates the log file, created with \link[utils]{person}.
#' @param csrf The CSRF token of your session, received with
#'   \code{\link{get_csrf}}.
#' @param log_path A path to save the log file. Defaults to the return value of
#'   \code{\link{tempdir()}}.
#' @export
#' @return Currently returns a data.frame, this should be a dataset. The columns
#' are:
#' \itemize{
#'  \item{"action"}{ create_property}
#'  \item{"id_on_target"}{ The new Property Identifier (PID) on the targeted Wikibase.}
#'  \item{"label"}{ The propery label}
#'  \item{"description"}{ The description label}
#'  \item{"language"}{ The language code of the label}
#'  \item{"datatype"}{ Defaults to `item`}
#'  \item{"wikibase_api_url"}{ The MediaWiki API URL where the new property is created}
#'  \item{"equivalence_property"}{ The PID that connects an equivalence ID to the property}
#'  \item{"equivalence_id"}{ The ID of an equivalent item defined elsewhere}
#'  \item{"success"}{ TRUE if successfully created, FALSE if there was an error}
#'  \item{"comment"}{ A summary of the error messages(s), if success is FALSE}
#'  \item{"time"}{ The time when the action started}
#'  \item{"logfile"}{ The name of the CSV logfile}
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
#' create_item(
#'    label = "biological object"
#'    language = "en"
#'    description = "This class comprises individual items of a material nature,
#'     which live, have lived or are natural products of or from living organisms.",
#'    equivalence_property = "P37"  # property for equivalent CIDOC definitions
#'    equivalence_id = "E20"  # CIDOC-CRM identifier of this concept
#'    csrf = your_csrf)
#' }
#' @export

create_item <- function(label,
                        description,
                        language,
                        equivalence_property = NA_character_,
                        equivalence_id = NA_character_,
                        wikibase_api_url,
                        data_curator = NULL,
                        log_path = tempdir(),
                        csrf) {

  # Credit the person who curates the data
  if (is.null(data_curator)) data_curator <- person("Jane", "Doe")

  assertthat::assert_that(
    inherits(data_curator, "person"),
    msg='copy_wikidata_item(..., data_curator): data_curator must be a person, like person("Jane, "Doe").')


  # Save the time of running the code
  action_timestamp <- action_timestamp_create()
  log_file_name <- paste0("wbdataset_create_item_", action_timestamp, ".csv")

  if ( !is.na(equivalence_id) ) {
    # If there is an equivalence ID, for example, a QID on Wikidata, than the
    # equivalence property that connects this ID to the newly created property
    # must be given, too.
    assertthat::assert_that(
      ! is.na(equivalence_property),
      msg = "create_item() cannot add equivalence_id statement without equivalence_property.")
  }

  default_labels <- list (language = language, value = label)
  labels_list <- list (default_labels)
  names(labels_list) <- language

  default_descriptions <- list (language = language, value = description)
  descriptions_list <- list ( default_descriptions )
  names(descriptions_list) <- language

  datastring <- item_identity_datastring_create(
    labels_list = labels_list,
    descriptions_list = descriptions_list
  )

  teststring = 'api.php?action=wbeditentity&new=item&data={"labels":{"de":{"language":"de","value":"de-value"},"en":{"language":"en","value":"en-value"}}}'

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
      new    = "item",
      data   = datastring,
      token  = csrf_token,
      format = "json"
    ),
    encode = "form",
    handle = csrf
  )

  # See if the created POST via wbeditentity was successful
  created_item_response <- httr::content(new_property,
                                             as = "parsed",
                                             type = "application/json")
  created_item_response

  if (is_response_success(created_item_response)) {
    # Successfully created the property
    message("Successfully created item ", created_item_response$entity$id,
            " (", created_item_response$entity$labels$en$value, ")")

    if (is.na(equivalence_property)) {
      # If equivalence must be handled, not rewritten yet
      wikidata_qid_df <- add_id_statement(
        qid = created_item_response$entity$id,
        pid = equivalence_property,
        o = equivalence_id,
        wikibase_api_url = wikibase_api_url,
        csrf = csrf
      )
    }

    return_dataframe <- data.frame(
      action = "create_item",
      id_on_target = created_item_response$entity$id,
      label = label,
      description =  created_item_response$entity$descriptions[[1]]$value,
      language =  created_item_response$entity$descriptions[[1]]$language,
      datatype = "item",
      wikibase_api_url = wikibase_api_url,
      equivalence_property =  equivalence_property,
      equivalence_id = equivalence_id,
      success = TRUE,
      comment = NA_character_,
      time = action_timestamp,
      logfile = log_file_name
    )

    write.csv(return_dataframe,
              file = file.path(log_path, log_file_name),
              row.names=FALSE,
              na = "NA",
              fileEncoding = "UTF-8")

    return_dataframe
  } else if (
    # There is a wikibase-validator-...-conflict in the error message
    any(
      c( "wikibase-validator-label-conflict" %in% unlist(created_item_response$error$messages),
         "wikibase-validator-label-with-description-conflict" %in% unlist(created_item_response$error$messages))
    )) {
    # wikibase-validator-label-conflict: the property already exists, and this
    # information should be returned to the user.
    message_strings <- unlist(created_item_response$error$messages)
    message(message_strings)
    message_strings <- message_strings[which(grepl("Item:", message_strings))]
    pattern <- "\\[\\[Item:*(.*?)\\|"
    result <- regmatches(message_strings, regexec(pattern, message_strings))
    old_qid <- result[[1]][2]

    message("Problem with recording a new item with label='", label, "'.
            An item already exists with this label: (", old_qid, ")")

    return_dataframe <- data.frame(
      action = "create_item",
      id_on_target = old_qid,
      label = label,
      description = "<description match not inspected>",
      language =  language,
      datatype = "item",
      wikibase_api_url = wikibase_api_url,
      equivalence_property =  equivalence_property,
      equivalence_id = equivalence_id,
      success = FALSE,
      comment = "wikibase-validator-label-conflict, the label-language pair already exists.",
      time = action_timestamp,
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
        lapply(created_item_response$error$messages, function(x) x$name)
        ), collapse="|")

    return_dataframe <- data.frame(
      action = "create_item",
      id_on_target = NA_character_,
      label = label,
      description = description,
      language =  language,
      datatype = "item",
      wikibase_api_url = wikibase_api_url,
      equivalence_property =  equivalence_property,
      equivalence_id = equivalence_id,
      success = FALSE,
      comment = error_comments,
      time = action_timestamp,
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
