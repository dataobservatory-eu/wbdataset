#' @title Copy a Wikibase item
#' @description
#' This code will copy a property label and description from Wikidata to a
#' new instance. It should work between instances, but the authentication to
#' copy from a password protected instance is not yet coded. The function is more
#' specific than create_item, because this one creates
#' items that exist in a similarly structured Wikibase instance, such as
#' Wikidata.
#' @details This function is slightly different from create_item.
#' That function creates a new item that may not have an equivalent item
#' on another Wikibase instance, but it may well have an equivalent item in
#' another database. \cr
#' In this function, we use \code{qid_equivalence_property}
#' for equivalence. In the more general create function we use
#' \code{equivalence_item}, because we may use different identifiers.
#' Similarly, the \code{qid_on_source} replaces the more general
#' \code{equivalence_id}, because we must use QID for identification in an
#' other Wikibase instance.
#' @param qid_on_wikidata The QID of the item to be copied to your Wikibase.
#' @param qid_equivalence_property The QID in Wikibase that records the equivalent
#' Wikidata QID as an external ID.
#' @param language A vector of languages codes, for example, \code{c("en", "et")}.
#' @param wikibase_api_url For example, \code{'https://reprexbase.eu/demowiki/api.php'}.
#' @param csrf The CSRF token of your session, received with \code{\link{get_csrf}}.
#' @examples
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
#' @export

copy_wikibase_item <- function(
    qid_on_source = "Q4",
    qid_equivalence_property = "P377",
    languages = c("en", "nl", "de", "ru", "hu", "lv", "sk", "et", "fr", "es", "pt", "lt"),
    wikibase_api_url = "https://reprexbase.eu/demowiki/api.php",
    csrf) {

  # Save the time of running the code
  action_timestamp <- action_timestamp_create()
  log_file_name <- paste0("wbdataset_copy_wikibase_item_", action_timestamp, ".csv")

  # Assert that qid_on_wikidata looks like a QID
  qid_on_wikidata <- as.character(qid_on_wikidata)
  assertthat::assert_that(is_qid(qid_on_source),
    msg = "qid_on_source must start with Q followed by digits."
  )

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
    # no error
  } else {
    response <- httr::content(get_claim$result,
      as = "parsed", type = "application/json"
    )
  }

  if (!is_response_success(response)) {
    message("Could not access ", qid_on_wikidata)
    message(response$error$messages[[1]])
    return(data.frame(
      default_label = default_label,
      qid_on_wikidata = qid_on_wikidata,
      pid_on_wikibase = NA_character_,
      success = FALSE
    ))
  }

  labels_present <- languages[which(languages %in% names(response$entities[[1]]$labels))]
  labels_missing <- languages[which(!languages %in% names(response$entities[[1]]$labels))]

  descriptions_present <- languages[which(languages %in% names(response$entities[[1]]$descriptions))]
  descriptions_missing <- languages[which(!languages %in% names(response$entities[[1]]$descriptions))]

  aliases_present <- languages[which(languages %in% names(response$entities[[1]]$aliases))]
  aliases_missing <- languages[which(!languages %in% names(response$entities[[1]]$aliases))]
  labels_missing

  if ("en" %in% names(response$entities[[1]]$labels)) {
    default_label <- response$entities[[1]]$labels$en$value
  } else {
    default_label <- response$entities[[1]]$sitelinks$enwiki$title
  }

  message("Default label for ", qid_on_wikidata, ": ", default_label)
  labels_missing_list <- list()

  for  (l in labels_missing) {
    labels_missing_list <- c(labels_missing_list, tmp = list(list(
      language = l,
      value = default_label
    )))
    names(labels_missing_list)[which(names(labels_missing_list) == "tmp")] <- l
  }

  labels_list <- c(response$entities[[1]]$labels[labels_present], labels_missing_list)

  descriptions_missing_list <- list()
  for  (d in descriptions_missing) {
    descriptions_missing_list <- c(descriptions_missing_list,
      tmp = list(list(language = d, value = ""))
    )
    names(descriptions_missing_list)[which(names(descriptions_missing_list) == "tmp")] <- d
  }

  descriptions_list <- c(response$entities[[1]]$descriptions[descriptions_present], descriptions_missing_list)


  datastring <- jsonlite::toJSON(
    list(
      labels = labels_list,
      descriptions = descriptions_list,
      datatype = response$entities[[1]]$datatype
    ),
    auto_unbox = T
  )

  csrf_token <- get_csrf_token(csrf = csrf)

  new_item <- httr::POST(
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

  created_item_response <- httr::content(new_item, as = "parsed", type = "application/json")


  if (is_response_success(created_item_response)) {
    message("Successfully created item ",
            created_item_response$entity$id, " (",
            created_item_response$entity$labels$en$value, ")")

    if (is_qid(qid_equivalence_property)) {
      wikidata_qid_df <- add_id_statement(
        qid = created_item_response$entity$id,
        pid = qid_equivalence_property,
        o = qid_on_wikidata,
        wikibase_api_url = wikibase_api_url,
        csrf = csrf
      )
    }

    data.frame(
      default_label = default_label,
      qid_on_wikidata = qid_on_wikidata,
      pid_on_wikibase = created_item_response$entity$id,
      success = TRUE
    )
  } else if ("wikibase-validator-label-conflict" %in% unlist(created_item_response$error$messages)) {
    message_strings <- unlist(created_item_response$error$messages)
    message(message_strings)
    message_strings <- message_strings[which(grepl("Property:", message_strings))]
    pattern <- "\\[\\[Property:*(.*?)\\|"
    result <- regmatches(message_strings, regexec(pattern, message_strings))
    old_pid <- result[[1]][2]
    data.frame(
      default_label = default_label,
      qid_on_wikidata = qid_on_wikidata,
      pid_on_wikibase = old_pid,
      success = FALSE
    )
  } else {
    message(created_item_response$error)
    data.frame(
      default_label = default_label,
      qid_on_wikidata = qid_on_wikidata,
      pid_on_wikibase = NA_character_,
      success = FALSE
    )
  }
}
