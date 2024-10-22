#' @title Get claims of an item
#' @description
#' Get the claims (statements) related to an item.
#' @param qid The QID of the item.
#' @param wikibase_api_url The URL of the Wikibase API.
#' @param csrf A response csrf
#' @export

get_claims <- function(qid="Q528626",
                       property = "P625",
                       wikibase_api_url="https://www.wikidata.org/w/api.php", csrf=NULL) {

  response <- NULL
  claim_body <- list(
    action = "wbgetentities",
    ids   = qid,
    #languages = "en|nl|hu",
    #props = "labels",
    format = "json")

  #get_claim <- httr::POST(
  #  "https://www.wikidata.org/w/api.php",
  #  body = claim_body,
  #  encode = "form"
  #)
  #token = get_csrf_token(csrf),
  get_claim2 <- httr::POST(
    #wikibase_api_url
    "https://www.wikidata.org/w/api.php",
    body = list(
      action    = "wbgetclaims",
      entity    = qid,
      property = property,
      formatversion = 2,
      format = "json"),
    encode = "form"
  )

  get_claim2

  response <- httr::content(get_claim2, as = "parsed", type = "application/json")

  response$error
  response$claims
  response$claims[[property]][[1]]$mainsnak$property
  datatype <- response$claims[[property]][[1]]$mainsnak$datatype

  if ( datatype == "wikibase-item" ) {
    value <- response$claims[[property]][[1]]$mainsnak$datavalue$value$id
    type <-  datatype
  } else if (  datatype == "external-id") {
    value <- response$claims[[property]][[1]]$mainsnak$datavalue$value
    type <- datatype
  } else  if ( datatype == "string") {
    value <- response$claims[[property]][[1]]$mainsnak$datavalue$value
    type <- datatype
  } else  if ( datatype == "time") {
    value <- response$claims[[property]][[1]]$mainsnak$datavalue$time
    type  <- datatype
  } else if ( datatype == "globe-coordinate") {
    raw_value <- response$claims[[property]][[1]]$mainsnak$datavalue$value
    altitude <- ifelse(is.null(raw_value$altitude), "", raw_value$altitude)
    value <- paste0("mlat=", raw_value$latitude,"&mlon=", raw_value$longitude, "&altitude=", altitude, "&precision=", raw_value$precision, "&globe=", raw_value$globe)
    type <- datatype
      }

  return_df <- data.frame(qid=qid, value=value, type=type)
  names(return_df)[2] <- property
  return_df
}

#' @title Join a new column by property
#' @description
#' Get the claims (statements) related to an item.
#' @param ds A dataset object that contains the observations (QIDs)
#' @param property The property
#' @param wikibase_api_url The URL of the Wikibase API.
#' @param csrf A response csrf
#' @importFrom dplyr left_join select
#' @importFrom dataset as_dataset creator dataset_title dataset_title<-
#' @importFrom purrr safely
#' @export

left_join_column <- function(ds, property,
                             wikibase_api_url="https://www.wikidata.org/w/api.php",
                             silent=FALSE) {

  new_column <- data.frame(qid=vector("character"),
                           property=vector("character"),
                           type=vector("character"))
  names(new_column)[2] <- property

  return_df <- new_column

  safely_get_claims <- purrr::safely(get_claims, NULL)

  items <- seq_along(ds$qid)
  max_item <- max(items)

  for ( q in items ) {
    if (!silent)   message("Left join claims: ",  q, "/", max_item, ": ",  ds$qid[q], " ", property)
     these_claims <- safely_get_claims(ds$qid[q], property = property)

     if (is.null(these_claims$error)) {
       these_claims <- these_claims$result
     } else {
       error_item <- these_claims$error
       if( "message" %in% names(error_item)) {
         error_label <- ifelse(error_item$message == "argument is of length zero",
                               "missing", "error")
       } else {
         error_label <- "error"
       }
       these_claims <- data.frame(qid=ds$qid[q], property=property, type=error_label)
       names(these_claims)[2] <- these_claims[[2]][1]
       these_claims[[2]][1] <- NA_character_
     }

     return_df <- rbind(return_df, these_claims)
  }

  column_type <- "error"
  potential_type <- unique(return_df$type)[! unique(return_df$type) %in% c("error", "missing")]
  if (length(potential_type)==1) column_type <- potential_type

  original_prov <- attributes(ds)$Provenance

  return_df <- return_df[, which(! names(return_df) %in% "type")]

  new_column_ds <- as_dataset(return_df, author = creator(ds), title=dataset_title(ds))
  newcol_prov <- attributes(new_column_ds)

  new_ds <- invisible(left_join(ds, new_column_ds, by = "qid"))


  attr(new_ds, "Provenance") <- list ( started_at = original_prov$started_at,
                                       ended_at  = newcol_prov$Provenance$ended_at,
                                       wasAssocitatedWith = original_prov$wasAssocitatedWith)

  new_property_definition <- c("property"=column_type)
  names(new_property_definition) <- property

  attr(new_ds, "wikibase_type") <- c(attr(ds, "wikibase_type"), new_property_definition)
  new_ds
}


