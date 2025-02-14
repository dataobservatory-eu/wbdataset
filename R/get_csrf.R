#' @title Get CSRF object with security token
#' @description Get a Cross-Site Request Forgery (CSRF) object containing your
#'   CSRF token.
#' @details CSRF is a way for a malicious website to exploit your logged-in
#'   session on another website to perform actions as you without your consent.
#'   The MediaWiki API (the API of Wikibase instances) employs CSRF tokens (also
#'   often called "edit tokens" in the MediaWiki context) as a crucial defense
#'   mechanism against Cross-Site Request Forgery attacks.\cr \cr To receive
#'   such a CSRF security token (connected to your current editing session on a
#'   Wikibase instance) you have to run first \code{\link{get_csrf}} to
#'   establish your session and your credentials. As a result, you will receive
#'   a `csrf` object which contains among other data, your CSRF token. Then, now
#'   running the \code{\link{get_csrf_token}} function will unwrap from returned
#'   `csrf` object the token itself, which is often, but not necessarily, a
#'   character string of length 42.\cr \cr The MediaWiki API's CSRF tokens are
#'   not designed to be long-lived.  Their lifespan is intentionally kept short,
#'   typically tied to the user's current editing session or a reasonable
#'   timeframe for a single action.  This is a crucial security measure.
#' @param username Your bot username on the Wikibase instance at
#'   \code{wikibase_api_url}.
#' @param password Your bot password on the Wikibase instance at
#'   \code{wikibase_api_url}.
#' @param wikibase_api_url The API URL for the Wikibase instance.
#' @return \code{\link{get_csrf}} returns a list that contains the CSRF token
#'   among other data.
#' @importFrom httr handle GET POST content
#' @export

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
#' @param csrf The csrf object returned by \code{\link{get_csrf}}.
#' @details CSRF tokens act like a secret handshake between the your R script
#' and the MediaWiki server. Only requests that have the correct "handshake"
#' (the matching token) are considered legitimate. They are returned by
#' \code{\link{get_csrf}}.
#' @return \code{\link{get_csrf_token}} returns a character string of length 1
#' with the user's CSRF token from the list returned by \code{\link{get_csrf}}.
#' @importFrom httr handle GET POST content
#' @export
get_csrf_token <- function(csrf) {
  csrf_data <- httr::content(csrf, as = "parsed", type = "application/json")
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
