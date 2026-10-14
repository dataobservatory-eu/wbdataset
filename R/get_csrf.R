#' Authenticate with a Wikibase instance
#'
#' @description
#' Authenticate a bot account with a MediaWiki/Wikibase API and obtain
#' a response containing a Cross-Site Request Forgery (CSRF) token.
#' The authenticated HTTP session can subsequently be used for editing,
#' uploading local media, and modifying Wikibase statements.
#'
#' @details
#' MediaWiki uses CSRF tokens (also called *edit tokens*) to protect
#' state-changing API operations against unauthorized requests.
#'
#' Authentication involves three steps:
#' \enumerate{
#'   \item Request a login token from the MediaWiki API.
#'   \item Authenticate using the bot username, password, and login token.
#'   \item Request a CSRF token for the authenticated session.
#' }
#'
#' The function returns the HTTP response from the final request.
#' Use [get_csrf_token()] to extract the CSRF token as a character string.
#'
#' Both authentication cookies and CSRF tokens are session-dependent.
#' Tokens should not be stored permanently or reused across sessions.
#' When an editing operation reports an invalid token, obtain a fresh
#' authenticated session.
#'
#' The returned response is used as `csrf` by other functions in
#' `wbdataset`, including [upload_local_media()] and [add_local_media()].
#'
#' @param username Character string. Bot username on the Wikibase instance.
#' @param password Character string. Password associated with the bot account.
#' @param wikibase_api_url Character string. Full URL of the MediaWiki API
#'   endpoint, ending in `api.php`, for example
#'   `https://reprexbase.eu/rubik/api.php`.
#'
#' @return
#' `get_csrf()` returns an object of class `response` from [httr::GET()].
#' Its JSON response body contains the CSRF token in
#' `query$tokens$csrftoken`.
#'
#' @seealso
#' [get_csrf_token()], [upload_local_media()], [add_local_media()],
#' [add_statement()]
#'
#' @importFrom httr handle GET POST content
#' @export
#'
#' @examples
#' \dontrun{
#' api <- "https://reprexbase.eu/rubik/api.php"
#'
#' csrf <- get_csrf(
#'   username = Sys.getenv("WIKIBASE_USERNAME"),
#'   password = Sys.getenv("WIKIBASE_PASSWORD"),
#'   wikibase_api_url = api
#' )
#'
#' token <- get_csrf_token(csrf)
#' }
get_csrf <- function(username, password, wikibase_api_url) {
  check_api_url(wikibase_api_url = wikibase_api_url) # check if ends with api.php

  ## 1. handle
  handle <- httr::handle(wikibase_api_url)
  message("Received a `", class(handle), "`: ", handle$url)

  ## 2. session ------------
  session <- httr::GET(wikibase_api_url)
  message(class(session), ": Establish session with ", wikibase_api_url, ": ", session$status_code)

  if (session$status_code == 200) message("Session: OK(200)")

  query_token_params <- list(
    action = "query",
    meta = "tokens",
    type = "login",
    format = "json"
  )

  # 3 ---------------
  response_1 <- httr::GET(wikibase_api_url, query = query_token_params)
  message(class(response_1), ": login to ", wikibase_api_url, ": ", response_1$status_code)

  assertthat::assert_that(
    class(response_1) == "response",
    msg = "Login did not result in a response"
  )

  if (response_1$status_code == 200) message("Login: OK(200)")

  ## Create a new login token -------
  login_token <- NULL
  response_data <- httr::content(response_1, as = "parsed", type = "application/json")
  login_token <- response_data$query$tokens$logintoken

  # There is no formally specified minimum length for a valid CSRF token.
  # The security of a CSRF token relies more on its
  # unpredictability and randomness than its absolute length.
  # While the length is often 42, do not use an assertion based on token length.

  assertthat::assert_that(
    inherits(login_token, "character"),
    msg = "The token should be a character string."
  )

  assertthat::assert_that(
    nchar(login_token) >= 2,
    msg = "Token is less than 2 characters long."
  )

  message("Login token: ", substr(login_token, 1, 10), "***********")


  if (!is.character(login_token)) {
    stop("Seeminly error with login_token: ", login_token)
  }

  login_params <- list(
    action = "login",
    lgname = username,
    lgpassword = password,
    lgtoken = login_token,
    format = "json"
  )

  csrf_params <- list(
    action = "query",
    meta = "tokens",
    format = "json"
  )


  response_2 <- httr::POST(wikibase_api_url,
    body = login_params
  )

  response_2_data <- httr::content(response_2,
    as = "parsed",
    type = "application/json"
  )

  if (response_2_data$login$result == "Failed") {
    stop(response_2_data$login$reason)
  }


  message("Post login data to ", wikibase_api_url)

  csrf <- httr::GET(wikibase_api_url, query = csrf_params)

  assertthat::assert_that(
    csrf$status_code == 200,
    msg = "The status code of the returned CSRF is not OK (200)."
  )

  csrf
}

#' @rdname get_csrf
#'
#' @description
#' `get_csrf_token()` extracts the CSRF token from the HTTP response
#' returned by [get_csrf()].
#'
#' @details
#' The token is required for MediaWiki API operations that modify
#' server-side data, including file uploads and Wikibase edits.
#'
#' The token is associated with the authenticated session and is not
#' a substitute for authentication cookies.
#'
#' MediaWiki does not guarantee a fixed token length. Applications
#' should not assume that CSRF tokens always contain 42 characters.
#'
#' @param csrf An HTTP `response` object returned by [get_csrf()].
#'
#' @return
#' `get_csrf_token()` returns a character string of length one containing
#' the CSRF token.
#'
#' @export
get_csrf_token <- function(csrf) {
  csrf_data <- httr::content(csrf,
    as = "parsed",
    type = "application/json"
  )
  csrf_data
  csrf_token <- csrf_data$query$tokens$csrftoken
  if (nchar(csrf_token) < 10) stop("Error: get_csrf_token(csrf): Did not receive a valid token.")
  csrf_token
}


#' @keywords internal
#' @importFrom assertthat assert_that
check_api_url <- function(wikibase_api_url) {
  n_char <- nchar(wikibase_api_url)
  assertthat::assert_that(
    substr(wikibase_api_url, n_char - 6, n_char) == "api.php",
    msg = "wikibase_api_url must end with api.php"
  )
}
