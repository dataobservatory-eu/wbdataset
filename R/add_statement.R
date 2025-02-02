#' @title Add statements
#' @description
#' See \link[https://www.wikidata.org/w/api.php?action=help&modules=wbcreateclaim]{MediaWiki API help}
#'
#' @param qid The QID of the item in the Wikibase instance that you use.
#' @param pid The PID of the equivalent Wikidata (or reference Wikibase) URI.
#' @param o The object of the semantic statement.
#' @param wikibase_type A \code{'item'}, \code{'numeric'}, or \code{'string'}.
#' @param wikibase_api_url \code{\link{"https://reprexbase.eu/demowiki/api.php"}}.
#' @param csrf The CSRF token of your session, received with \code{\link{get_csrf}}.
#' @return A data.frame with four columns:
#' \code{id} containing the statement ID, \code{qid} with the QID,
#' \code{o} with the PID, and \code{p} with the  wikidata_uri.
#' @export

add_statement <- function(qid, pid, o,
                          wikibase_type,
                          wikibase_api_url = "https://reprexbase.eu/demowiki/api.php",
                          csrf) {
  if (wikibase_type == "string") {
    add_item_statement(
      qid = qid, pid = pid, o = o,
      wikibase_type == "string",
      wikibase_api_url = wikibase_api_url, csrf_token = csrf_token
    )
  } else if (wikibase_type == "item") {
    add_item_statement(
      qid = qid, pid = pid, o = o,
      wikibase_type == "item",
      wikibase_api_url = wikibase_api_url, csrf_token = csrf_token
    )
  } else if (wikibase_type == "numeric") {

  } else {
    stop("Error in add_statement(..., wikibase_type): '", wikibase_type, "' is not recognised (yet).")
  }
}

#' @rdname add_statement
#' @inheritParams add_statement
#' @param wikidata_uri The QID on Wikidata (or a reference Wikibase).
#' @export

add_wikibase_url_statement <- function(qid,
                                       pid,
                                       wikibase_url,
                                       wikibase_api_url = "https://reprexbase.eu/demowiki/api.php",
                                       csrf_token) {
  datavalue <- gsub(
    "changeuri", wikibase_url,
    '{"claims":[{"mainsnak":{"snaktype":"value","property":"pid","datavalue":{"value":"changeuri","type":"string"}},"type":"statement","rank":"normal"}]}'
  )

  datavalue
  datavalue <- gsub("pid", pid, datavalue)

  claim_body <- list(
    action = "wbeditentity",
    id = qid, # in the new wikibase
    property = pid, # in the wikibase where you write
    data = datavalue,
    # data = '{"claims":[{"mainsnak":{"snaktype":"value","property":"P1","datavalue":{"value":"https://www.wikidata.org/wiki/Q43878","type":"string"}},"type":"statement","rank":"normal"}]}',
    token = csrf_token,
    format = "json"
  )

  new_claim <- httr::POST(
    wikibase_api_url,
    body = claim_body,
    encode = "form",
    handle = response_csrf
  )

  response <- content(new_claim)
  response

  if ("error" %in% names(response)) {
    return(data.frame(
      statement_id = NA_character_,
      wikibase_qid = qid,
      pid = NA_character_,
      url = NA_character_
    ))
  }
  if (response$success == 1) {
    data.frame(
      statement_id = l[[1]]$id,
      wikibase_qid = response$entity$id,
      pid          = pid,
      url          = l[[1]]$mainsnak$datavalue$value
    )
  }
}
