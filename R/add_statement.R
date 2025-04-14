#' @title Add statements to an item
#' @description Adds a claim (statement) to a Wikibase item via `wbcreateclaim`.
#' See
#' \link[https://www.wikidata.org/w/api.php?action=help&modules=wbcreateclaim]{MediaWiki
#' API help}
#' @param qid The QID of the item in the Wikibase instance.
#' @param pid The PID of the property to be added.
#' @param o The object of the statement (either a QID, string, or number).
#' @param wikibase_type One of `"item"`, `"string"` (or `"external-id"`), or `"numeric"`.
#' @param wikibase_api_url The full URL of the Wikibase API (must end with `api.php`).
#' @param csrf The CSRF token of your session, received with
#'   \code{\link{get_csrf}}.
#' @return A data.frame describing the created statement.
#' @export
add_statement <- function(
    qid, pid, o,
    wikibase_type = c("item", "string", "numeric", "external-id"),
    wikibase_api_url = "https://reprexbase.eu/demowiki/api.php",
    csrf) {
  if (!is.character(wikibase_type) || length(wikibase_type) != 1) {
    stop("add_statement(): 'wikibase_type' must be a single character string.")
  }

  # Safely match
  wikibase_type <- match.arg(wikibase_type,
    choices = c(
      "item", "string",
      "numeric", "external-id"
    )
  )

  if (wikibase_type == "external-id") wikibase_type <- "string"

  if (wikibase_type == "string") {
    add_id_statement(
      qid = qid,
      pid = pid,
      o = o,
      wikibase_type = "external-id", # this is what the API expects
      wikibase_api_url = wikibase_api_url,
      csrf = csrf
    )
  } else if (wikibase_type == "item") {
    add_item_statement(
      qid = qid,
      pid = pid,
      o = o,
      wikibase_type = "item",
      wikibase_api_url = wikibase_api_url,
      csrf = csrf
    )
  } else if (wikibase_type == "numeric") {
    stop("The 'numeric' type is not yet implemented in add_statement(). Please use a more specific method.")
  } else {
    stop(
      "Error in add_statement(..., wikibase_type): '",
      wikibase_type, "' is not recognised (yet)."
    )
  }
}
