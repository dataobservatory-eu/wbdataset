#' @title Copy an property
#' @description
#' This code will copy a property label and description from Wikidata to a
#' new instance. It should work between instances, but the authentication to
#' copy from a password protected instance is not yet coded.
#' @param pid_on_source The PID of the property on Wikidata to be copied to
#' your Wikibase. (Only works with non-authenticated sources, this should be
#' changed.)
#' @param wikidata_pid_property The PID in Wikibase that records the equivalent Wikidata
#' PID as an external ID.
#' @param language A vector of languages codes, for example, \code{c("en", "et")}.
#' @param wikibase_api_url For example, \code{'https://reprexbase.eu/demowiki/api.php'}.
#' @param csrf The CSRF token of your session, received with \code{\link{get_csrf}}.
#' @export
#' @return
#' Currently returns a data.frame, this should be a dataset. It has four
#' columns, \code{default_label},
#' \code{pid_on_source} containing the PID of the property on the source instance,
#' \code{pid_on_target} containing the new PID on the target Wikibase instance,
#'  \code{success}, a logical value if the copying was
#' successful.

copy_property <- function(pid_on_source,
                          wikidata_pid_property = "P2",
                          languages = c("en", "hu"),
                          wikibase_api_url = "https://reprexbase.eu/jekyll/api.php",
                          csrf) {
  pid_on_source <- as.character(pid_on_source)

  claim_body <- list(
    action = "wbgetentities",
    ids = pid_on_source,
    format = "json"
  )

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
    message("Could not access ", pid_on_source)
    message(response$error$messages[[1]])
    return(data.frame(
      default_label = default_label,
      pid_on_source = pid_on_source,
      pid_on_target = NA_character_,
      success = FALSE
    ))
  }

  response$entities[[1]]$aliases

  ## We must determine which labels, descriptions, aliases actually exists
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

  message("Default label for ", pid_on_source, ": ", default_label)
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

  descriptions_list <- c(response$entities[[1]]$descriptions[descriptions_present], descriptions_missing_list)

  datastring <- jsonlite::toJSON(
    list(
      labels = labels_list,
      descriptions = descriptions_list,
      datatype = response$entities[[1]]$datatype
    ),
    auto_unbox = T
  )

  datastring

  csrf_token <- get_csrf_token(csrf)

  assertthat::assert_that(!is.null(csrf_token),
    msg = "You do not have a CSRF token"
  )

  assertthat::assert_that(nchar(csrf_token) == 42,
    msg = "Your CSRF token should have 42 characters."
  )

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
  created_item_response

  if (is_response_success(created_item_response)) {
    # Successfully created the property
    message("Successfully created item ", created_item_response$entity$id, " (", created_item_response$entity$labels$en$value, ")")

    if (is_pid(wikidata_pid_property)) {
      wikidata_pid_df <- add_id_statement(
        qid = created_item_response$entity$id,
        pid = wikidata_pid_property,
        o = pid_on_source,
        wikibase_api_url = wikibase_api_url,
        csrf = csrf
      )
    }

    data.frame(
      default_label = default_label,
      pid_on_source = pid_on_source,
      pid_on_target = created_item_response$entity$id,
      success = TRUE
    )
  } else if ("wikibase-validator-label-conflict" %in% unlist(created_item_response$error$messages)) {
    ## Special
    message_strings <- unlist(created_item_response$error$messages)
    message(message_strings)
    message_strings <- message_strings[which(grepl("Property:", message_strings))]
    pattern <- "\\[\\[Property:*(.*?)\\|"
    result <- regmatches(message_strings, regexec(pattern, message_strings))
    old_pid <- result[[1]][2]
    data.frame(
      default_label = default_label,
      pid_on_source = pid_on_source,
      pid_on_target = old_pid,
      success = FALSE
    )
  } else {
    ## Return an empty data.frame if there was some error
    message(created_item_response$error)
    data.frame(
      default_label = default_label,
      pid_on_source = pid_on_source,
      pid_on_target = NA_character_,
      success = FALSE
    )
  }
}
