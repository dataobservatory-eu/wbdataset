#' @title Make a call to the wbeditentity API
#' @description Generalized interface for creating or editing a Wikibase entity (item or property).
#' @param csrf_token The CSRF token to authorize the request.
#' @param wikibase_api_url The URL of the Wikibase API (must end with "api.php").
#' @param entity_data A JSON string representing the entity's data (labels, descriptions, etc.).
#' @param new_entity_type Optional. One of "item" or "property" if creating a new entity.
#' @param existing_id Optional. QID or PID if editing an existing entity.
#' @param summary Optional. Edit summary.
#' @param bot Logical. Whether this is a bot edit.
#' @param csrf_handle Optional. httr handle for authenticated sessions.
#' @return Parsed response from the Wikibase API.
#' @keywords internal
call_wbeditentity <- function(csrf_token,
                              wikibase_api_url,
                              entity_data,
                              new_entity_type = NULL,
                              existing_id = NULL,
                              summary = NULL,
                              bot = FALSE,
                              csrf_handle = NULL) {
  if (!is.logical(bot) || length(bot) != 1) {
    stop("The `bot` argument must be TRUE or FALSE.")
  }

  if (isTRUE(bot)) {
    body$bot <- "true"
  }

  body <- list(
    action = "wbeditentity",
    data = entity_data,
    token = csrf_token,
    format = "json"
  )

  if (!is.null(existing_id)) {
    body$id <- existing_id
  }

  if (!is.null(new_entity_type)) {
    if (!new_entity_type %in% c("item", "property")) {
      stop("Invalid value for `new_entity_type`: must be 'item' or 'property'.")
    }
    body$new <- new_entity_type
  }

  if (!is.null(summary)) {
    body$summary <- summary
  }

  if (bot) {
    body$bot <- "true"
  }

  response <- httr::POST(
    url = wikibase_api_url,
    body = body,
    encode = "form",
    handle = csrf_handle
  )

  content <- httr::content(response, as = "parsed", type = "application/json")

  if (!is.null(content$error)) {
    stop("wbeditentity error: ", content$error$code, ": ", content$error$info)
  }

  return(content)
}
