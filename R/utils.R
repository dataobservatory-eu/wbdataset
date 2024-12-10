#' @title API response success
#' @param response A response from MediaWiki Action API, type response.
#' @keywords internal
is_response_success <- function(response) {
  if ("success" %in% names(response) ) response$success == 1 else FALSE
}

#' @title Is an object a non-empty data.frame?
#' @param df An object
#' @return A boolean, \code{TRUE} if the object is a data.frame with at least one
#' row.
#' @keywords internal
is_df_not_empty <- function(df) {
  if (inherits(df, "data.frame") ) {
    if (nrow(df)>0) { TRUE }
  } else FALSE
}
