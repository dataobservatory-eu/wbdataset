#' @title Get CSRF
#' @description
#' Get a Cross-Site Request Forgery (CSRF) response
#' @param username Your bot username.
#' @param password Your bot password
#' @param wikibase_api_url The API URL for the Wikibase instance.
#' @return A list, the CSRF response
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

  assertthat::assert_that(
    nchar(login_token) == 42,
    msg = "Token is not 42 character long"
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
#' @return The CSRF response token.
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
