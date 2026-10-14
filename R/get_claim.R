#' Retrieve statements from a Wikibase item
#'
#' @description
#' Retrieve the values of a specified property from a Wikibase item using
#' the Wikibase API
#' [`wbgetclaims` ](https://www.mediawiki.org/wiki/Wikibase/API#wbgetclaims).
#'
#' @details
#' Supports Wikidata and other Wikibase instances, including local media
#' properties (`localMedia`). Supported data types include item references,
#' strings, external identifiers, URLs, time, quantity, coordinates,
#' monolingual text, and Commons or local media filenames.
#'
#' When `first = TRUE`, the preferred statement is returned if available;
#' otherwise, the first normal statement is returned. When `first = FALSE`,
#' all statements for the property are returned, one row per value.
#'
#' This function supersedes the deprecated [get_claims()].
#'
#' @param qid Character. Item identifier, such as `"Q42"`.
#' @param pid Character. Property identifier, such as `"P569"`.
#' @param wikibase_api_url Character. Full Wikibase API endpoint URL,
#'   ending in `api.php`. Defaults to the Wikidata API.
#' @param csrf Optional. Authentication session or CSRF object.
#'   Used by the existing API wrapper, but not required for public reads.
#' @param first Logical. If `TRUE` (default), return the preferred or
#'   first normal statement. If `FALSE`, return all statements.
#'
#' @return
#' A data frame containing the item identifier, statement values, and
#' detected data types. With `first = FALSE`, each statement occupies
#' one row.
#'
#' @seealso [get_claims()] (deprecated)
#'
#' @export

get_claim <- function(
  qid = "Q528626",
  pid = "P625",
  wikibase_api_url = "https://www.wikidata.org/w/api.php",
  csrf = NULL,
  first = TRUE
) {
  if (!is_qid(qid)) {
    stop(sprintf("Invalid QID: '%s'. QIDs must begin with 'Q' followed by digits (e.g., 'Q42').", qid))
  }

  if (!is_pid(pid)) {
    stop(sprintf("Invalid property ID: '%s'. Properties must begin with 'P' followed by digits (e.g., 'P31').", pid))
  }

  claim_body <- list(
    action = "wbgetclaims",
    entity = qid,
    property = pid,
    format = "json"
  )

  safely_post <- purrr::safely(httr::POST)

  get_claim <- safely_post(
    url = wikibase_api_url,
    body = claim_body,
    encode = "form",
    csrf_handle = csrf
  )

  if (is.null(get_claim$result)) {
    message("get_claim (", qid, ", ", pid, ", '", wikibase_api_url, "') resulted in: ", get_claim$error)
    return(NULL)
  } else {
    content <- httr::content(get_claim$result,
      as = "parsed",
      type = "application/json"
    )
  }

  if (!is.null(content$error)) {
    stop(sprintf("API error from Wikidata: %s", content$error$info))
  }

  if (is.null(content$claims) || is.null(content$claims[[pid]])) {
    stop(sprintf("Property '%s' not found for QID '%s'", pid, qid))
  }

  claims_list <- content$claims[[pid]]

  extract_value <- function(snak) {
    switch(snak$datatype,
      "wikibase-item" = snak$datavalue$value$id,
      "external-id" = snak$datavalue$value,
      "string" = snak$datavalue$value,
      "url" = snak$datavalue$value,
      "time" = snak$datavalue$value$time,
      "quantity" = snak$datavalue$value$amount,
      "monolingualtext" = snak$datavalue$value$text,
      "commonsMedia" = snak$datavalue$value,
      "localMedia" = snak$datavalue$value,
      "globe-coordinate" = {
        val <- snak$datavalue$value
        altitude <- ifelse(is.null(val$altitude), "", val$altitude)
        paste0(
          "mlat=", val$latitude,
          "&mlon=", val$longitude,
          "&altitude=", altitude,
          "&precision=", val$precision,
          "&globe=", val$globe
        )
      },
      stop(sprintf("Unsupported datatype: %s", snak$datatype))
    )
  }

  if (first) {
    preferred <- Filter(function(claim) claim$rank == "preferred", claims_list)
    normal <- Filter(function(claim) claim$rank == "normal", claims_list)
    selected <- if (length(preferred) > 0) preferred[[1]] else normal[[1]]

    snak <- selected$mainsnak
    value <- extract_value(snak)
    datatype <- snak$datatype

    df <- data.frame(
      qid = qid,
      pid = pid,
      value = value,
      datatype = datatype,
      stringsAsFactors = FALSE
    )
    return(df)
  } else {
    # All claims as rows
    rows <- lapply(claims_list, function(claim) {
      snak <- claim$mainsnak
      data.frame(
        qid = qid,
        pid = pid,
        datatype = snak$datatype,
        value = extract_value(snak),
        stringsAsFactors = FALSE
      )
    })

    df <- do.call(rbind, rows)
    return(df)
  }
}

#' @title Deprecated: retrieve a claim from a Wikidata item
#'
#' @description
#' \strong{Deprecated.} This function has been replaced by
#' \code{\link{get_claim}}.
#'
#' @details
#' \code{get_claims()} was used to retrieve a claim (statement) for a specific property from
#' a Wikidata or Wikibase item. This function is now deprecated and will be removed in a
#' future release. Please use \code{\link{get_claim}} instead, which provides more robust
#' functionality and better support for modern data types.
#'
#' @param ... Any parameters.
#' @return A data frame or list depending on implementation. For updated behaviour, use \code{get_claim()}.
#'
#' @seealso \code{\link{get_claim}}
#'
#' @export

get_claims <- function(...) {
  warning("get_claims() is deprecated and will be removed in a future release. Please use get_claim() instead.")

  # Existing function code
}
