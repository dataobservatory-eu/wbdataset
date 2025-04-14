#' @keywords internal
resolve_from_session <- function(param_name, param_value, session_list) {
  if (!missing(param_value) && !is.null(param_value)) {
    return(param_value)
  }

  if (!is.null(session_list) && !is.null(session_list[[param_name]])) {
    return(session_list[[param_name]])
  }

  return(NULL)
}
