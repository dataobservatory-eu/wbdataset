#' @title Validate a wikibase_session object
#' @description Checks that the session object is a named list and its
#'   components are valid for reuse.
#' @param wikibase_session A list, typically created with
#'   \code{\link{new_wikibase_session}}.
#' @return \code{TRUE} if the session is valid, otherwise an informative
#' error.
#' @keywords internal

validate_wikibase_session <- function(wikibase_session) {
  if (is.null(wikibase_session)) {
    return(invisible(TRUE))
  }

  if (!is.list(wikibase_session)) {
    stop("wikibase_wikibase_session must be a named list, typically created with new_wikibase_session().")
  }

  # Optional field-specific checks
  if (!is.null(wikibase_session$language) &&
    (!is.character(wikibase_session$language) || length(wikibase_session$language) != 1)) {
    stop("wikibase_session$language must be a single character string.")
  }

  if (!is.null(wikibase_session$pid_equivalence_property) &&
    (!is.character(wikibase_session$pid_equivalence_property) || length(wikibase_session$pid_equivalence_property$language) != 1)) {
    stop("wikibase_session$pid_equivalence_property must be a single character string.")
  }

  if (!is.null(wikibase_session$qid_equivalence_property) &&
    (!is.character(wikibase_session$qid_equivalence_property) || length(wikibase_session$qid_equivalence_property) != 1)) {
    stop("wikibase_session$qid_equivalence_property must be a single character string.")
  }


  if (!is.null(wikibase_session$data_curator) &&
    !inherits(wikibase_session$data_curator, "person")) {
    stop("wikibase_session$data_curator must be created using person().")
  }

  if (!is.null(wikibase_session$wikibase_api_url) &&
    (!is.character(wikibase_session$wikibase_api_url) ||
      !grepl("^https?://", wikibase_session$wikibase_api_url))) {
    stop("wikibase_session$wikibase_api_url must be a valid URL (starting with http or https).")
  }

  if (!is.null(wikibase_session$csrf)) {
    if (!is.character(wikibase_session$csrf) || length(wikibase_session$csrf) != 1) {
      stop("wikibase_session$csrf must be a single character string.")
    }

    if (!is_valid_csrf_token(wikibase_session$csrf)) {
      warning("CSRF token format looks unusual. Check if this was retrieved via get_csrf().")
    }
  }

  invisible(TRUE)
}
