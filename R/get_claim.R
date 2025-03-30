#' @title Get claims of an item
#' @description
#' Get the claims (statements) related to an item.
#' @details
#' A wrapper around
#' \href{https://www.wikidata.org/w/api.php?action=help&modules=wbgetentities}{wbgetentities}.
#' @param qid The QID of the item.
#' @param property The property for which the claim is required.
#' @param wikibase_api_url The full URL of the Wikibase API, which is the
#'   address that the \code{wbdataset} R client sends requests to when
#'   interacting with the knowledge base. For example,
#'   \code{'https://reprexbase.eu/demowiki/api.php'}. The URL must end with
#'   api.php.
#' @param csrf A response csrf received with \code{\link{get_csrf}}.
#' @export

get_claims <- function(qid = "Q528626",
                       property = "P625",
                       wikibase_api_url = "https://www.wikidata.org/w/api.php",
                       csrf = NULL) {
  response <- NULL
  claim_body <- list(
    action = "wbgetentities",
    ids = qid,
    # languages = "en|nl|hu",
    # props = "labels",
    format = "json"
  )

  # get_claim <- httr::POST(
  #  "https://www.wikidata.org/w/api.php",
  #  body = claim_body,
  #  encode = "form"
  # )
  # token = get_csrf_token(csrf),
  get_claim2 <- httr::POST(
    wikibase_api_url,
    body = list(
      action = "wbgetclaims",
      entity = qid,
      property = property,
      formatversion = 2,
      format = "json"
    ),
    encode = "form"
  )

  get_claim2

  response <- httr::content(get_claim2, as = "parsed", type = "application/json")

  response$error
  response$claims
  response$claims[[property]][[1]]$mainsnak$property
  datatype <- response$claims[[property]][[1]]$mainsnak$datatype

  if (datatype == "wikibase-item") {
    value <- response$claims[[property]][[1]]$mainsnak$datavalue$value$id
    type <- datatype
  } else if (datatype == "external-id") {
    value <- response$claims[[property]][[1]]$mainsnak$datavalue$value
    type <- datatype
  } else if (datatype == "string") {
    value <- response$claims[[property]][[1]]$mainsnak$datavalue$value
    type <- datatype
  } else if (datatype == "time") {
    value <- response$claims[[property]][[1]]$mainsnak$datavalue$time
    type <- datatype
  } else if (datatype == "globe-coordinate") {
    raw_value <- response$claims[[property]][[1]]$mainsnak$datavalue$value
    altitude <- ifelse(is.null(raw_value$altitude), "", raw_value$altitude)
    value <- paste0("mlat=", raw_value$latitude, "&mlon=", raw_value$longitude, "&altitude=", altitude, "&precision=", raw_value$precision, "&globe=", raw_value$globe)
    type <- datatype
  }

  return_df <- data.frame(qid = qid, value = value, type = type)
  names(return_df)[2] <- property
  return_df
}
