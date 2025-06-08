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
#' @title Extract a QID from an entity result
#' @description Coerces `id_on_target` to a character and ensures it starts with "Q".
#' @param x A result object (e.g., from `check_existing_item()`).
#' @return A QID as a character string, or NA if not found or invalid.
#' @keywords internal
get_qid <- function(x) {
  if (is.null(x)) return(NA_character_)

  if (!is.atomic(x) && !is.character(x)) {
    x <- x$id_on_target %||% NA_character_
  }

  x <- as.character(x)

  if (is.na(x) || !startsWith(x, "Q")) {
    warning("Expected QID (starting with 'Q'), got: ", x)
    return(NA_character_)
  }
  x
}

#' @title Extract a PID from a property result
#' @description Coerces `id_on_target` to a character and ensures it starts with "P".
#' @title Extract a PID (property ID)
#' @description Accepts a result object or a scalar ID and ensures it starts with "P".
#' @param x Either a result object (with \code{id_on_target}) or a scalar ID.
#' @return A PID as a character string or NA_character_.
#' @keywords internal
get_pid <- function(x) {
  if (is.null(x)) return(NA_character_)

  # If it's a result object, extract id_on_target
  if (!is.atomic(x) && !is.character(x)) {
    x <- x$id_on_target %||% NA_character_
  }

  x <- as.character(x)

  if (is.na(x) || !startsWith(x, "P")) {
    warning("Expected PID (starting with 'P'), got: ", x)
    return(NA_character_)
  }

  x
}
