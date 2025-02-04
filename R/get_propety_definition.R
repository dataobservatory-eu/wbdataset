#' @title Get property defintion
#' @description
#' Receive the label and description of a property on the basis of its PID from
#' a Wikibase instance. It will not add further statements about the property.
#' @details
#' Currently the languages choose a default, "en", for cases where the user-chosen
#' language return empty labels and descriptions. This feature may be elaborated
#' or changed later. The function receives aliases (alternative labels), too,
#' but does not return them; the format of aliases needs to be decided in view
#' of how other functions will use them, as aliases themselves can break the
#' tidiness of the returned data.
#' @param pid The PID of the property in the Wikibase instance (or Wikidata itself).
#' @param languages Defaults to \code{c("en", "nl", "hu")}. A character string of
#' the languages in which the users wants to receive the labels and descriptions
#' of the property.
#' @param wikibase_api_url Defaults to \code{"https://www.wikidata.org/w/api.php"},
#' may be replaced with a similar API address of a Wikibase instance. Private
#' instances may require an authenticated session.
#' @return_type Defaults to \code{"data.frame"} that is suitable for receiving
#' the information in stand-alone use. The \code{"JSON"} passes on a JSON string
#' in the format that you may need it in further Wikibase API calls.
#' @return A data.frame of the \code{PID} with the labels and descriptions of the
#' property in the selected languages. Alternatively, when
#' \code{return_type="JSON"}, the same data prepared for use in a subsequent
#' API call, for example, to copy these contents into a new Wikibase instance.
#' @importFrom assertthat assert_that
#' @importFrom purrr safely
#' @importFrom httr content
#' @importFrom jsonlite toJSON
#' @examples
#' # Receive a data.frame for further use
#' get_property_definition(pid="P2047", return_type = "data.frame")
#' # Receive JSON for copying with wbeditidentiy
#'  get_property_definition(pid="P2047", languages=c("en", "hu"))
#' @export

get_property_definition <- function(
    pid,
    languages = c("en", "nl", "hu"),
    wikibase_api_url = "https://www.wikidata.org/w/api.php",
    return_type = "JSON") {

  ## Ensure that the pid is a character string starting with P followed by
  ## numbers.
  pid <- as.character(pid)
  assertthat::assert_that(is_pid(pid),
    msg = "get_property_label_description(pid) - pid does not look like a PID identifier."
  )

  ## Define a standard return JSON reply for not successful queries.
  error_json <- jsonlite::toJSON(
    list(
      labels = list(en = list(language = "en", value = "")),
      description = list(en = list(language = "en", value = "")),
      datatype = "error"
    ),
    auto_unbox = T
  )

  ## Define a standard return data.frame for not successful queries.
  error_data_frame <- data.frame(
    source_pid = pid,
    language = NA_character_,
    label = NA_character_,
    description = NA_character_,
    success = FALSE
  )

  claim_body <- list(
    action = "wbgetentities",
    ids = pid,
    format = "json"
  )

  ## Handle exceptions with posting the query to the Wikibase API
  safely_post <- purrr::safely(httr::POST, NULL)

  recevied_claim <- safely_post(
    "https://www.wikidata.org/w/api.php",
    body = claim_body,
    encode = "form"
  )

  if (!is.null(recevied_claim$error)) {
    ## Not successful: the Wikibase API error itself sends and error message
    ## Give an error message and send back an empty result.
    message(recevied_claim$error)
    if (return_type == "data.frame") {  # if the user needs a data.frame
      return(error_data_frame)
    }
    if (return_type != "data.frame") { # in any other case send JSON
      return(error_json)
    }
  } else {
    # No error message: parse the received response. The response may
    # not be usable, so further exception handling is needed.
    response <- httr::content(recevied_claim$result,
                              as = "parsed", type = "application/json")
  }

  if (!is_response_success(response)) {  # internal assertion for susccessful response
    # Exception: retrieval of the property was not successful, even though we
    # did not get an explicit error before.
    message("Could not access ", pid)
    message(response$error$messages[[1]]) #print the error message for debugging
    if (return_type == "data.frame") { # if the user needs a data.frame
      return(error_data_frame)
    }
    if (return_type != "data.frame") { # in any other case send JSON
      return(error_json)
    }
  }

  ## We must determine which labels, descriptions, aliases actually exist
  ## in the languages requested by the user.
  labels_present <- languages[which(languages %in% names(response$entities[[1]]$labels))]
  labels_missing <- languages[which(!languages %in% names(response$entities[[1]]$labels))]

  descriptions_present <- languages[which(languages %in% names(response$entities[[1]]$descriptions))]
  descriptions_missing <- languages[which(!languages %in% names(response$entities[[1]]$descriptions))]

  # We do not work with the aliases now, but they may be used later.
  aliases_present <- languages[which(languages %in% names(response$entities[[1]]$aliases))]
  aliases_missing <- languages[which(!languages %in% names(response$entities[[1]]$aliases))]

  ## Set a default later, this is now hard coded to English but could be a parameter.

  if ("en" %in% names(response$entities[[1]]$labels)) {
    default_label <- response$entities[[1]]$labels$en$value
  } else {
    default_label <- response$entities[[1]]$sitelinks$enwiki$title
  }

  # message("Default label for ", pid, ": ", default_label)

  # The missing labels (i.e., translations that are missing for a label)
  # are replaced with the missing label, so we always have a label.
  labels_missing_list <- list()

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

  descriptions_list <- c(response$entities[[1]]$descriptions[descriptions_present], descriptions_missing_list)

  # Returning the requested data which is stored in lists;
  # if needed, in the future it can be returned as a nested list, too.

  if (return_type == "data.frame") { # if the user needs a data.frame

    labels_vector <- unlist(lapply(labels_list, function(x) unlist(x$value)))
    descriptions_vector <- unlist(lapply(descriptions_list, function(x) unlist(x$value)))

    data.frame(
      source_pid = pid,
      language = names(labels_vector),
      label = as.character(labels_vector)
    ) %>%
      left_join(
        data.frame(
          language = names(descriptions_vector),
          description = as.character(descriptions_vector),
          success = TRUE
        ),
        by = "language"
      )
  } else {
    jsonlite::toJSON(
      list(
        labels = labels_list,
        descriptions = descriptions_list,
        datatype = response$entities[[1]]$datatype
      ),
      auto_unbox = T
    )
  }
}

