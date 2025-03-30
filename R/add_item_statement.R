#' @rdname add_statement
#' @param qid The QID of the item in the Wikibase instance that you use.
#' @param pid The PID of the equivalent Wikidata (or reference Wikibase) URI.
#' @param o The object of the semantic statement.
#' @param wikibase_type It must be \code{'item'}.
#' @export

add_item_statement <- function(qid, pid, o,
                               wikibase_type = "item",
                               wikibase_api_url = NULL,
                               csrf) {
  # api.php?action=wbcreateclaim&entity=Q4115189&property=P9003&snaktype=value&value={"entity-type":"item","numeric-id":1}

  assertthat::assert_that(!is.null(wikibase_api_url),
    msg = "No wikibase_api_url is given"
  )

  csrf_token <- get_csrf_token(csrf = csrf)

  base_string <- '{"entity-type":"item","numeric-id":numid}'
  new_numeric_id <- gsub("Q", "", o)
  datavalue <- gsub("numid", new_numeric_id, base_string)
  datavalue

  claim_body <- list(
    action = "wbcreateclaim",
    entity = qid,
    property = pid,
    snaktype = "value",
    value = datavalue,
    # '"datavalue":{"value":"https://www.wikidata.org/wiki/Q43878","type":"string"}}',
    token = csrf_token,
    format = "json"
  )

  new_claim <- httr::POST(
    wikibase_api_url,
    body = list(
      action = "wbcreateclaim",
      entity = qid,
      property = pid,
      snaktype = "value",
      value = datavalue,
      # '"datavalue":{"value":"https://www.wikidata.org/wiki/Q43878","type":"string"}}',
      token = csrf_token,
      format = "json"
    ),
    encode = "form",
    handle = csrf
  )

  response <- httr::content(new_claim, as = "parsed", type = "application/json")

  if ("error" %in% names(response)) {
    # invalid snak error at type mismatch
    warning_message <- glue::glue("Error in 'wbcreateclaim' wrapper add_item_statement():\n", response$error$code, ": ", response$error$info)
    warning(warning_message)

    return(data.frame(
      id = NA_character_,
      s = qid,
      o = NA_character_,
      p = NA_character_
    ))
  }

  if (response$success == 1) {
    data.frame(
      id = response$claim$id,
      s = qid,
      o = pid,
      p = response$claim$mainsnak$datavalue$value$id
    )
  }
}
