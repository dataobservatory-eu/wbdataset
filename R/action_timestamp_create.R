#' @title Create a timestamp
#' @description Format the timestamp so that it can be used in log file
#' names, too.
#' @keywords internal
action_timestamp_create <- function() {
  action_time <- Sys.time()
  action_timestamp <- gsub("\\s", "_", as.character(action_time))
  action_timestamp <- gsub("\\:", "-", action_timestamp)
  sub("\\..*", "", action_timestamp)
}
