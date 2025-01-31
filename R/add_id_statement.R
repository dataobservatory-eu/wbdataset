#' @rdname add_statement
#' @inheritParams add_statement
#' @param wikibase_type Defaults to \code{"external-id"}.
#' @importFrom httr POST content
#' @importFrom glue glue
#' @export


add_id_statement <- function(qid, pid, o,
                             wikibase_type = "external-id",
                             wikibase_api_url = "https://reprexbase.eu/jekyll/api.php",
                             csrf) {

  datavalue <- paste0('"',o, '"')
  datavalue

  this_csrf_token <- get_csrf_token(csrf = csrf)

  claim_body <- list(
    action = "wbcreateclaim",
    entity   = qid,
    property = pid,
    snaktype = "value",
    value =  datavalue,
    #datatype = "external-id",
    # '"datavalue":{"value":"https://www.wikidata.org/wiki/Q43878","type":"string"}}',
    token = csrf_token,
    format = "json")

  new_claim <- httr::POST(
    wikibase_api_url,
    body = list(
      action = "wbcreateclaim",
      entity    = qid,
      property  = pid,
      snaktype  = "value",
      value =  datavalue,
      token = this_csrf_token,
      format = "json"),
    encode = "form",
    handle = csrf
  )

  response <- httr::content(new_claim, as = "parsed", type = "application/json")

  if ("error" %in% names(response)) {
    warning_message <- glue::glue("Error in 'wbcreateclaim' wrapper add_id_statement():\n", response$error$code, ": ", response$error$info)
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
      p = response$claim$mainsnak$datavalue$value
    ) }
}


#'datavalue\":{\"value\":\"https://www.wikidata.org/wiki/Q43878\",\"type\":\"string\"},\"datatype\":\"external-id\"}'
# api.php?action=wbcreateclaim&entity=Q4115189&property=P9003&snaktype=value&value={"entity-type":"item","numeric-id":1}
