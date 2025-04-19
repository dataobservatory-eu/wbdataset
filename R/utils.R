#' @title API response success
#' @param response A response from MediaWiki Action API, type response.
#' @keywords internal
is_response_success <- function(response) {
  if ("success" %in% names(response)) response$success == 1 else FALSE
}

#' @title Is an object a non-empty data.frame?
#' @param df An object
#' @return A boolean, \code{TRUE} if the object is a data.frame with at least one
#' row.
#' @keywords internal
is_df_not_empty <- function(df) {
  if (inherits(df, "data.frame")) {
    if (nrow(df) > 0) {
      TRUE
    }
  } else {
    FALSE
  }
}

#' @title Get return dataset column names
#' @keywords internal
get_return_ds_structure <- function() {
  c(
    "rowid", "action", "id_on_target",
    "label", "description", "language",
    "datatype", "wikibase_api_url", "equivalence_property",
    "equivalence_id", "classification_property", "classification_id",
    "success", "comment", "time",
    "logfile"
  )
}


#' @title Check if a CSRF token appears valid
#' @description Validates the basic structure of a MediaWiki-style CSRF token.
#' @param csrf_token A single character string representing the token.
#' @return Logical \code{TRUE} or \code{FALSE}.
#' @keywords internal
is_valid_csrf_token <- function(csrf_token) {
  if (!is.character(csrf_token) || length(csrf_token) != 1 || is.na(csrf_token)) {
    return(FALSE)
  }
  ifelse(nchar(csrf_token)>10, TRUE, FALSE)
}
