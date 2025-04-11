#' @title Handle Search Term Ambiguity
#' @description Resolves ambiguity in search results by selecting a preferred
#'   match or returning NULL.
#' @param search_results A list of search results from the Wikibase API.
#' @param search_term The original search term used in the query.
#' @param language The language code used in the search.
#' @param prefer_first Logical; if TRUE, returns the first match when multiple
#'   matches are found.
#' @return A single search result or NULL if ambiguity cannot be resolved.
#' @keywords internal
handle_search_term_ambiguity <- function(search_results,
                                         search_term,
                                         language,
                                         strategy = c(
                                           "return_null",
                                           "return_first"
                                         ),
                                         csrf = NULL) {
  strategy <- match.arg(strategy)

  # Filter for exact label-language matches
  exact_matches <- purrr::keep(search_results, function(item) {
    item$label == search_term && item$match$language == language
  })

  if (length(exact_matches) == 1) {
    return(exact_matches[[1]])
  } else if (length(exact_matches) > 1) {
    if (strategy == "return_first") {
      return(exact_matches[[1]])
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}
