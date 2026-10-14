#' Add a statement to a Wikibase item
#'
#' @description
#' Create a statement (claim) on an existing Wikibase item using the
#' MediaWiki `wbcreateclaim` API module. A statement connects a Wikibase
#' item to a property and an object value.
#'
#' @details
#' A statement consists of three components: a subject (`qid`), a
#' predicate (`pid`), and an object (`o`). The object representation
#' depends on the datatype of the Wikibase property.
#'
#' The following values of `wikibase_type` are recognised:
#' \describe{
#'   \item{`"item"`}{A reference to another Wikibase item, identified
#'     by its QID.}
#'   \item{`"string"`}{A character string, including values for
#'     string-based properties and local media filenames.}
#'   \item{`"external-id"`}{An external identifier. Internally handled
#'     using the same method as `"string"`.}
#'   \item{`"url"`}{A URL value. Internally handled using the same
#'     method as `"string"`.}
#'   \item{`"numeric"`}{Recognised but not yet implemented.}
#' }
#'
#' The function delegates statement creation to [add_item_statement()]
#' for item references and [add_id_statement()] for string-based values.
#'
#' The property must exist in the target Wikibase instance and accept
#' the supplied value. The function creates a new statement and does
#' not check whether an identical statement already exists.
#'
#' For statements referencing files in the local MediaWiki repository,
#' see [add_local_media()]. Files can be uploaded separately using
#' [upload_local_media()].
#'
#' See the
#' \href{https://www.wikidata.org/w/api.php?action=help&modules=wbcreateclaim}{
#' MediaWiki API documentation for wbcreateclaim}.
#'
#' @param qid Character string. Identifier of the Wikibase item
#'   receiving the statement, such as `"Q213"`.
#' @param pid Character string. Identifier of the Wikibase property,
#'   such as `"P37"`.
#' @param o Object value of the statement. Its interpretation depends
#'   on `wikibase_type`.
#' @param wikibase_type Character string. One of `"item"`, `"string"`,
#'   `"external-id"`, `"url"`, or `"numeric"`. Must be specified
#'   explicitly. Numeric statements are not yet implemented.
#' @param wikibase_api_url Character string. Full URL of the MediaWiki
#'   API endpoint, ending in `api.php`. Defaults to
#'   `https://reprexbase.eu/demowiki/api.php`.
#' @param csrf Authenticated HTTP response returned by [get_csrf()].
#'   The session must have permission to edit the target item.
#'
#' @return
#' A data frame containing the created statement:
#' \describe{
#'   \item{`id`}{Identifier of the newly created statement.}
#'   \item{`s`}{Subject: the Wikibase item QID.}
#'   \item{`p`}{Predicate: the Wikibase property PID.}
#'   \item{`o`}{Object: the statement value.}
#' }
#'
#' @seealso
#' [get_csrf()], [get_claim()], [add_local_media()],
#' [upload_local_media()]
#'
#' @export
add_statement <- function(
  qid, pid, o,
  wikibase_type = c("item", "string", "numeric", "external-id", "url"),
  wikibase_api_url = "https://reprexbase.eu/demowiki/api.php",
  csrf
) {
  if (!is.character(wikibase_type) || length(wikibase_type) != 1) {
    stop("add_statement(): 'wikibase_type' must be a single character string.")
  }

  # Safely match
  wikibase_type <- match.arg(wikibase_type,
    choices = c(
      "item", "string",
      "numeric", "external-id", "url"
    )
  )

  if (wikibase_type == "external-id") wikibase_type <- "string"
  if (wikibase_type == "url") wikibase_type <- "string"

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
