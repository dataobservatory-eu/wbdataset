#' @title Check if a string looks like a PID
#' @param x A string
#' @return A logical vector, \code{TRUE} where the string has a "P[:digits:]" structure,
#' FALSE otherwise.
#' @export

is_pid <- function(x) {
  all_false <- rep(FALSE, length(x))
  all_true  <- rep(TRUE, length(x))

  return_value <- vector("logical", length=length(x))
  if (is.null(x)) {
    return_value <- all_false
    return(return_value)
  } else {
    return_value <- all_true
  }

  if ( all(return_value)==FALSE) return(return_value)
  return_value <- ifelse(is.na(x), FALSE, return_value)
  if ( ! any(return_value)==TRUE) return(return_value)

  return_value <- ifelse(is.nan(x), FALSE, return_value)
  if ( ! any(return_value)==TRUE) return(return_value)

  x <- tolower(as.character(x))
  x <- ifelse(is.na(x), "", x)
  return_value <- ifelse(substr(x,1,1)!="p", FALSE, return_value)
  return_value <- ifelse(!grepl("[[:digit:]]", substr(x, 2, nchar(x))), FALSE, return_value)
  return_value <- ifelse(nchar(x)<2, FALSE, return_value)
  return_value
}

#' @title Check if a string looks like a QID
#' @param x A string
#' @return A logical vector, \code{TRUE} where the string has a "P[:digits:]" structure,
#' FALSE otherwise.
#' @export
is_qid <- function(x) {

  all_false <- rep(FALSE, length(x))
  all_true  <- rep(TRUE, length(x))

  return_value <- vector("logical", length=length(x))
  if (is.null(x)) {
    return_value <- all_false
    return(return_value)
  } else {
    return_value <- all_true
  }

  if ( all(return_value)==FALSE) return(return_value)
  return_value <- ifelse(is.na(x), FALSE, return_value)
  if ( ! any(return_value)==TRUE) return(return_value)

  return_value <- ifelse(is.nan(x), FALSE, return_value)
  if ( ! any(return_value)==TRUE) return(return_value)

  x <- tolower(as.character(x))
  x <- ifelse(is.na(x), "", x)
  return_value <- ifelse(substr(x,1,1)!="q", FALSE, return_value)
  return_value <- ifelse(!grepl("[[:digit:]]", substr(x, 2, nchar(x))), FALSE, return_value)
  return_value <- ifelse(nchar(x)<2, FALSE, return_value)
  return_value
}
