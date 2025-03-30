#' @title Add statements
#' @description See
#' \link[https://www.wikidata.org/w/api.php?action=help&modules=wbcreateclaim]{MediaWiki
#' API help}
#' @param qid The QID of the item in the Wikibase instance that you use.
#' @param pid The PID of the equivalent Wikidata (or reference Wikibase) URI.
#' @param o The object of the semantic statement.
#' @param wikibase_type A \code{'item'}, \code{'numeric'}, or \code{'string'}.
#' @param wikibase_api_url The full URL of the Wikibase API, which is the
#'   address that the \code{wbdataset} R client sends requests to when
#'   interacting with the knowledge base. For example,
#'   \code{'https://reprexbase.eu/demowiki/api.php'}. The URL must end with
#'   api.php.
#' @param csrf The CSRF token of your session, received with
#'   \code{\link{get_csrf}}.
#' @return A data.frame with four columns:
#' \code{id} containing the statement ID, \code{qid} with the QID,
#' \code{o} with the PID, and \code{p} with the  wikidata_uri.
#' @export

add_statement <- function(
    qid, pid, o,
    wikibase_type,
    wikibase_api_url = "https://reprexbase.eu/demowiki/api.php",
    csrf) {

  if (wikibase_type == "string") {
    add_item_statement(
      qid = qid, pid = pid, o = o,
      wikibase_type == "string",
      wikibase_api_url = wikibase_api_url, csrf_token = csrf_token
    )
  } else if (wikibase_type == "item") {
    add_item_statement(
      qid = qid, pid = pid, o = o,
      wikibase_type == "item",
      wikibase_api_url = wikibase_api_url, csrf_token = csrf_token
    )
  } else if (wikibase_type == "numeric") {

  } else {
    stop("Error in add_statement(..., wikibase_type): '", wikibase_type, "' is not recognised (yet).")
  }
}
