#' @title Join a new column by property
#' @description Get the claims (statements) related to an item.
#' @param ds A dataset object that contains the observations (by QIDs).
#' @param property The property
#' @param label A short, human-readable description of the vector or `NULL`.
#' @param unit A character string of length one containing the unit of measure
#'   or `NULL`.
#' @param definition A character string of length one containing a linked
#'   definition or `NULL`.
#' @param namespace A namespace for individual observations or categories or
#'   `NULL`.
#' @param silent If the function should send messages about individual requests
#'   to the API or not. Defaults to \code{FALSE}.
#' @param wikibase_api_url The URL of the Wikibase API.
#' @param csrf A
#' @importFrom dplyr left_join select
#' @importFrom dataset dataset_df as_dataset_df defined creator dataset_title
#' @importFrom purrr safely
#' @export

left_join_column <- function(
    ds,
    property,
    label = NULL,
    unit = NULL,
    definition = NULL,
    namespace = NULL,
    wikibase_api_url = "https://www.wikidata.org/w/api.php",
    silent = FALSE,
    csrf = NULL) {

  # Initialise a data.frame to return the data.

  new_column <- data.frame(
    qid = vector("character"),
    property = vector("character"),
    type = vector("character")
  )
  names(new_column)[2] <- property

  return_df <- new_column

  safely_get_claims <- purrr::safely(get_claims, NULL)

  items <- seq_along(ds$qid)
  max_item <- max(items)

  for (q in items) {
    if (!silent) message("Left join claims: ", q, "/", max_item, ": ", ds$qid[q], " ", property)
    these_claims <- safely_get_claims(ds$qid[q], property = property, csrf = csrf)

    if (is.null(these_claims$error)) {
      these_claims <- these_claims$result
    } else {
      error_item <- these_claims$error
      if ("message" %in% names(error_item)) {
        error_label <- ifelse(error_item$message == "argument is of length zero",
          "missing", "error"
        )
      } else {
        error_label <- "error"
      }
      these_claims <- data.frame(qid = ds$qid[q], property = property, type = error_label)
      names(these_claims)[2] <- these_claims[[2]][1]
      these_claims[[2]][1] <- NA_character_
    }

    return_df <- rbind(return_df, these_claims)
  }

  column_type <- "error"
  potential_type <- unique(return_df$type)[!unique(return_df$type) %in% c("error", "missing")]
  if (length(potential_type) == 1) column_type <- potential_type

  original_prov <- attributes(ds)$Provenance

  return_df <- return_df[, which(!names(return_df) %in% "type")]

  if (!all(
    c(is.null(label), is.null(unit), is.null(definition), is.null(namespace))
    )) {
    return_df[, 2] <- defined(return_df[, 2],
                              label = label,
                              unit = unit,
                              definition = definition,
                              namespace = namespace)
  }

  new_column_ds <- as_dataset_df(df = return_df,
                                 reference = list(author = creator(ds),
                                                  title = dataset_title(ds))
                                 )
  newcol_prov <- attributes(new_column_ds)

  new_ds <- invisible(
    left_join(ds, new_column_ds,
              by = intersect(names(ds), names(new_column_ds)))
    )

  attr(new_ds, "Provenance") <- list(
    started_at = original_prov$started_at,
    ended_at = newcol_prov$Provenance$ended_at,
    wasAssocitatedWith = original_prov$wasAssocitatedWith
  )

  new_property_definition <- c("property" = column_type)
  names(new_property_definition) <- property

  attr(new_ds, "wikibase_type") <- c(attr(ds, "wikibase_type"),
                                     new_property_definition)
  new_ds
}
