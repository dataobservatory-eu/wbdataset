#' @title Check if a datatype is a valid Wikibase datatype
#'
#' @description This function checks whether a given character vector of
#' datatypes are valid according to the Wikibase Media API specification.
#'
#' @param x A character vector of datatypes to validate. Non-character inputs
#'   will be coerced using \code{as.character()}.
#'
#' @return A logical vector indicating whether each input value is a valid
#'   Wikibase datatype.
#'
#' @examples
#' # Valid inputs
#' is_valid_wikibase_datatype("wikibase-item")
#' is_valid_wikibase_datatype(c("string", "url", "external-id"))
#'
#' # Invalid input (not lowercase, not API-compatible)
#' \dontrun{
#' # FALSE — should be "external-id"
#' is_valid_wikibase_datatype("External ID")
#' }
#' @seealso \href{https://www.mediawiki.org/wiki/Wikibase/DataModel/JSON#Datatypes}{Wikibase JSON DataModel documentation}
#' @export

is_valid_wikibase_datatype <- function(x) {
  valid_datatypes <- c(
    "wikibase-item",
    "wikibase-property",
    "external-id",
    "url",
    "commonsMedia",
    "string",
    "monolingualtext",
    "quantity",
    "time",
    "globe-coordinate",
    "math",
    "geo-shape",
    "tabular-data",
    "musical-notation",
    "wikibase-lexeme",
    "wikibase-form",
    "wikibase-sense"
  )

  if (is.null(x)) return(logical(1))  # empty logical for NULL input

  x <- as.character(x)
  is_valid <- x %in% valid_datatypes
  is_valid[is.na(x)] <- FALSE  # explicitly set NA inputs to FALSE
  return(is_valid)

}
