#' @title Search Wikibase Entities
#' @description This internal function interfaces with the Wikibase API to
#'   search for entities matching a given search term and language.
#' @param search_term A character string representing the term to search for.
#' @param language A character string specifying the language code (e.g., "en").
#' @param type A character string indicating the type of entity to search for.
#'   Defaults to "item".
#' @param wikibase_api_url A character string providing the URL of the Wikibase
#'   API endpoint.
#' @importFrom httr POST content
#' @return A list containing search results from the Wikibase API.
#' @keywords internal
search_wikibase_entities <- function(search_term,
                                     language = "en",
                                     wikibase_api_url = "https://www.wikidata.org/w/api.php",
                                     csrf = NULL) {
  # Construct the body of the POST request
  body <- list(
    action = "wbsearchentities",
    search = search_term,
    language = language,
    formatversion = 2,
    format = "json",
    type = "item",
    strictlanguage = "true"
  )

  # Include CSRF token if provided
  if (!is.null(csrf)) {
    body$token <- csrf
  }

  # Make the POST request
  response <- httr::POST(
    url = wikibase_api_url,
    body = body,
    encode = "form"
  )

  # Parse the response
  content <- httr::content(response, as = "parsed", type = "application/json")

  # Check for errors in the response
  if (!is.null(content$error)) {
    stop(paste(content$error$code, ":", content$error$info))
  }

  # Return the search results
  return(content$search)
}

