#' @title Get CSRF
#' @description
#' Get a Cross-Site Request Forgery (CSRF) response
#' @param username Your bot username.
#' @param password Your bot password
#' @param wikibase_api_url The API URL for the Wikibase instance.
#' @return A list, the CSRF response
#' @importFrom httr handle GET POST content
#' @export

get_csrf <- function( username, password, wikibase_api_url) {

  ## 1. handle
  handle <- httr::handle(wikibase_api_url)
  message ("Received a `", class(handle), "`: ", handle$url)


  ## 2. session ------------
  session <- httr::GET(wikibase_api_url)
  message (class(session), ": Establish session with ", wikibase_api_url, ": ", session$status_code)

  query_token_params <- list(
    action = "query",
    meta = "tokens",
    type = "login",
    format = "json"
  )

  # 3 ---------------
  response_1 <- httr::GET(wikibase_api_url, query = query_token_params)
  message ( class(response_1), ": login to ", wikibase_api_url, ": ", response_1$status_code)

  stopifnot(class(response_1)=="response")

  ## Create a new login token -------
  login_token <- NULL
  data        <- httr::content(response_1, as = "parsed", type = "application/json")
  login_token <- data$query$tokens$logintoken
  stopifnot(nchar(login_token)==42)

  message("Login token: ", substr(login_token, 1, 10), "***********")


  if (!is.character(login_token)) {
    stop("Seeminly error with login_token: ", login_token)
  }

  login_params <- list(
    action = "login",
    lgname     = username,
    lgpassword = password,
    lgtoken    = login_token,
    format = "json"
  )

  csrf_params   <- list(
    action = "query",
    meta = "tokens",
    format = "json")


  login_params
  response_2 <- httr::POST(wikibase_api_url,
                           body = login_params)

  response_2_data <- httr::content(response_2, as = "parsed", type = "application/json")
  response_2_data$login


  message("Post login data to ", wikibase_api_url)

  response_csrf <- httr::GET(wikibase_api_url, query = csrf_params)

  response_csrf$url

  stopifnot(response_csrf$status_code==200)

  response_csrf
}

#' @rdname get_csrf
#' @param csrf The csrf object returned by \code{\link{get_csrf}}.
#' @return The CSRF response token.
#' @importFrom httr handle GET POST content
#' @export
get_csrf_token <- function (csrf) {
  csrf_data    <- httr::content(response_csrf, as = "parsed", type = "application/json")
  csrf_data
  csrf_token   <- csrf_data$query$tokens$csrftoken
  csrf_token
}
