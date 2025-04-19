#' @title Retrieve a claim from a Wikidata item
#'
#' @description Retrieve the value(s) of a specified property (claim) from a
#' Wikidata item using the `wbgetclaims` API. Supports multiple data types and
#' can return either the preferred claim or all claims for the property.
#'
#' @details This function provides read-only access to claims (statements) for a
#' given item from a Wikibase instance (such as Wikidata). It supports common
#' data types including string, URL, time, quantity, coordinate, external ID,
#' Commons media, and monolingual text.
#'
#' If \code{first = TRUE}, the function returns the claim ranked as
#' \code{"preferred"} if available, otherwise the first normal claim. If
#' \code{first = FALSE}, all available claims are returned in a tidy data frame,
#' one row per value.
#'
#' This function replaces \code{\link{get_claims}}, which is now deprecated.
#'
#' The function wraps the
#' \href{https://www.wikidata.org/w/api.php?action=help&modules=wbgetclaims}{wbgetclaims}
#' module of the Wikibase API.
#'
#' @param qid A character string giving the QID of the item (e.g.,
#'   \code{"Q42"}).
#' @param pid A character string giving the property ID (e.g.,
#'   \code{"P569"}).
#' @param wikibase_api_url The full URL to the Wikibase API endpoint. Must end
#'   with \code{api.php}. Defaults to the Wikidata API.
#' @param csrf (Optional) A CSRF token for write operations (not used in this
#'   read-only function).
#' @param first Logical; if \code{TRUE} (default), return only the preferred or
#'   first available claim. If \code{FALSE}, return all claims for the given
#'   property as one row per value.
#'
#' @return A data frame with one or more rows, containing the item QID, the
#'   value(s), and the detected data type for the claim.
#'
#' @details Claims are retrieved using the
#' \href{https://www.wikidata.org/w/api.php?action=help&modules=wbgetclaims}{wbgetclaims}
#' module of the Wikibase API. This function supports common Wikidata data
#' types, including string, URL, quantity, time, coordinate, external ID, media,
#' and monolingual text.
#'
#' If \code{first = TRUE}, the function returns only the claim ranked as
#' \code{"preferred"} if present, otherwise the first normal claim. If
#' \code{first = FALSE}, all claims are returned in a tidy format.
#'
#' @seealso \code{\link{get_claims}} (deprecated)
#'
#' @export

get_claim <- function(qid = "Q528626",
                      pid = "P625",
                      wikibase_api_url = "https://www.wikidata.org/w/api.php",
                      csrf = NULL,
                      first = TRUE) {
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
    message("get_claim (", qid, ", ", pid, ", '", wikibase_api_url , "') resulted in: ", get_claim$error)
    return(NULL)
  } else {
    content <- httr::content(get_claim$result,
                             as = "parsed",
                             type = "application/json")
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

    df <- data.frame(qid = qid,
                     pid = pid,
                     value = value,
                     datatype = datatype,
                     stringsAsFactors = FALSE)
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
#' \strong{Deprecated.} This function has been replaced by \code{\link{get_claim}}.
#'
#' @details
#' \code{get_claims()} was used to retrieve a claim (statement) for a specific property from
#' a Wikidata or Wikibase item. This function is now deprecated and will be removed in a
#' future release. Please use \code{\link{get_claim}} instead, which provides more robust
#' functionality and better support for modern data types.
#'
#' @param qid A character string giving the QID of the item.
#' @param property A character string giving the property ID.
#' @param wikibase_api_url The full URL of the Wikibase API endpoint (must end with \code{api.php}).
#' @param csrf (Optional) A CSRF token, not used in this read-only function.
#'
#' @return A data frame or list depending on implementation. For updated behaviour, use \code{get_claim()}.
#'
#' @seealso \code{\link{get_claim}}
#'
#' @export

get_claims <- function(...) {
  warning("get_claims() is deprecated and will be removed in a future release. Please use get_claim() instead.")

  # Existing function code
}
