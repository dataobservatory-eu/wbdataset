#' Add a local media file to a Wikibase item
#'
#' @description
#' Create a local media statement linking a Wikibase item to a file
#' stored in the associated MediaWiki repository.
#'
#' @param qid Wikibase item identifier, such as `"Q213"`.
#' @param filename Filename in the local MediaWiki repository.
#' @param pid Property identifier for the localMedia property.
#'   Defaults to `"P37"`.
#' @param wikibase_api_url URL of the Wikibase API endpoint.
#' @param csrf Authenticated session returned by [get_csrf()].
#'
#' @details
#' The file must already exist in the local MediaWiki repository.
#' This function does not upload files.
#'
#' The statement is created using [add_statement()]. The filename is
#' stored as the value of a localMedia property.
#'
#' @return A data frame returned by [add_statement()], containing
#'   the statement identifier, subject, property, and object.
#'
#' @seealso [upload_local_media()], [add_statement()], [get_claim()]
#'
#' @examples
#' \dontrun{
#' result <- add_local_media(
#'   qid = "Q213",
#'   filename = "T000_01151_tier2.jpg",
#'   pid = "P37",
#'   wikibase_api_url = "https://reprexbase.eu/rubik/api.php",
#'   csrf = csrf
#' )
#' }
#'
#' @export
add_local_media <- function(
  qid,
  filename,
  pid = "P37",
  wikibase_api_url = "https://www.wikidata.org/w/api.php",
  csrf
) {
  if (!is.character(qid) || length(qid) != 1L ||
    is.na(qid) || !grepl("^Q[0-9]+$", qid)) {
    stop("`qid` must be a valid Wikibase item identifier.")
  }

  if (!is.character(pid) || length(pid) != 1L ||
    is.na(pid) || !grepl("^P[0-9]+$", pid)) {
    stop("`pid` must be a valid Wikibase property identifier.")
  }

  if (!is.character(filename) || length(filename) != 1L ||
    is.na(filename) || !nzchar(filename)) {
    stop("`filename` must be a single character string.")
  }

  add_statement(
    qid = qid,
    pid = pid,
    o = filename,
    wikibase_type = "string",
    wikibase_api_url = wikibase_api_url,
    csrf = csrf
  )
}
