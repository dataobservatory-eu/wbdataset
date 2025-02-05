#' @title Create a property
#' @description
#' Creates a property entity on a Wikibase instance with a single language
#' labelling and description. New labels and descriptions can be added
#' in further languages later.
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
#' @param equivalence_id An optional PID or other identifier in Wikibase
#' that records the equivalence of the property. An external ID.
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
                            equivalence_property,
                            equivalaence_id,
                            wikibase_api_url,
                            csrf) {

  labels_list <- list ( lang1 = label)
  names(labels_list) <- language

  descriptions_list <- list ( lang1 = description)
  names(descriptions_list) <- language


  datastring <- property_identity_datastring_create(
    labels_list = labels_list,
    descriptions_list = descriptions_list,
    datatype = response$entities[[1]]$datatype
  )

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

    if (is_pid(wikidata_pid_property)) {
      wikidata_pid_df <- add_id_statement(
        qid = created_property_response$entity$id,
        pid = wikidata_pid_property,
        o = pid_on_source,
        wikibase_api_url = wikibase_api_url,
        csrf = csrf
      )
    }

    data.frame(
      default_label = default_label,
      pid_on_target = created_property_response$entity$id,
      success = TRUE
    )
  } else if ("wikibase-validator-label-conflict" %in% unlist(created_property_response$error$messages)) {
    ## Special
    message_strings <- unlist(created_property_response$error$messages)
    message(message_strings)
    message_strings <- message_strings[which(grepl("Property:", message_strings))]
    pattern <- "\\[\\[Property:*(.*?)\\|"
    result <- regmatches(message_strings, regexec(pattern, message_strings))
    old_pid <- result[[1]][2]
    data.frame(
      default_label = default_label,
      equivalence_property = NA_character_,
      equivalence_id = NA_character_,
      success = FALSE
    )
  } else {
    ## Return an empty data.frame if there was some error
    message(created_property_response$error)
    data.frame(
      default_label = default_label,
      pid_on_target = NA_character_,
      equivalence_property = NA_character_,
      equivalence_id = NA_character_,
      success = FALSE
    )
  }

}
