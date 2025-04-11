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
