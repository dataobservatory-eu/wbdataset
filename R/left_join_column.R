#' @title Join Wikidata claims to a dataset by QID
#'
#' @description For each item in a data frame containing Wikidata QIDs, this
#'   function retrieves the value of a specified property using the Wikibase
#'   API, and joins it back to the input data.
#'
#' @details This function queries the Wikibase API for claims (statements)
#'   related to a given property for each QID in the dataset. It returns only
#'   the preferred or first available value for each item
#'   (see \code{get_claim(first = TRUE)} for details).
#'
#'   Errors such as missing properties or API issues are gracefully handled, and
#'   NA values are returned where no claim is available. If the dataset already
#'   contains a column with the same name as the property, it will be replaced
#'   in the joined output.
#'
#'   This function is useful for enriching tabular data with values stored in
#'   Wikidata or another Wikibase-compatible knowledge base.
#'
#' @param ds A data frame that includes a column named \code{qid} with Wikidata
#'   QIDs.
#' @param property A property ID (e.g., \code{"P569"}) to retrieve for each QID.
#' @param wikibase_api_url The full URL of the Wikibase API endpoint (must end
#'   with \code{api.php}). Defaults to the Wikidata API.
#' @param csrf A CSRF token, not required for read-only operations.
#'
#' @return A data frame with the original data and a new column containing the
#'   property value for each QID. Rows where no claim is found will contain
#'   \code{NA}.
#'
#' @seealso \code{\link{get_claim}} for underlying claim retrieval logic.
#'
#' @export

left_join_column <- function(
    ds,
    property,
    wikibase_api_url = "https://www.wikidata.org/w/api.php",
    csrf = NULL) {

  safely_get_claim <- purrr::safely(get_claim, NULL)

  result_df <- vector("list", length = nrow(ds))

  for (q in seq_len(nrow(ds))) {
    these_claims <- safely_get_claim(
      qid = ds$qid[q],
      property = property,
      wikibase_api_url = wikibase_api_url,
      csrf = csrf,
      first = TRUE
    )

    if (is.null(these_claims$error)) {
      these_claims <- these_claims$result
    } else {
      error_item <- these_claims$error
      error_label <- if ("message" %in% names(error_item) &&
        error_item$message == "argument is of length zero") {
        "missing"
      } else {
        "error"
      }

      these_claims <- data.frame(qid = ds$qid[q], property = property, type = error_label)
      names(these_claims)[2] <- property
      these_claims[[2]][1] <- NA_character_
    }

    result_df[[q]] <- these_claims
  }

  result_df <- dplyr::bind_rows(result_df)

  if (property %in% names(ds)) {
    ds <- ds |>
      dplyr::select(-dplyr::all_of(property))
  }

  result_df <- dplyr::left_join(ds, result_df, by = "qid")
  return(result_df)
}
