#' @title Validate Wikidata Item IDs (QIDs)
#'
#' @description
#' Checks if each element in a character vector conforms to the Wikidata Item ID format,
#' which starts with 'Q' followed by one or more digits (e.g., 'Q42').
#'
#' @param x A character vector to be validated.
#'
#' @return A logical vector indicating whether each element matches the QID format.
#' @export
is_qid <- function(x) {
  if (!is.character(x)) {
    stop("Input must be a character vector.")
  }
  x_upper <- toupper(x) # Convert input to uppercase
  grepl("^Q\\d+$", x_upper)
}

#' @title Validate Wikidata Property IDs (PIDs)
#'
#' @description
#' Checks if each element in a character vector conforms to the Wikidata Property ID format,
#' which starts with 'P' followed by one or more digits (e.g., 'P31').
#'
#' @param x A character vector to be validated.
#'
#' @return A logical vector indicating whether each element matches the PID format.
#' @export
is_pid <- function(x) {
  if (!is.character(x)) {
    stop("Input must be a character vector.")
  }
  x_upper <- toupper(x) # Convert input to uppercase
  grepl("^P\\d+$", x_upper)
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

