#' Upload a file to a local MediaWiki repository
#'
#' @description
#' Upload a local file to the MediaWiki media repository associated with a
#' Wikibase instance, using the MediaWiki `action=upload` API and an
#' authenticated session.
#'
#' @param file Character string. Path to an existing local file. Relative
#'   and absolute paths are accepted. The file must be readable.
#' @param filename Character string. Filename to register in the MediaWiki
#'   repository. Defaults to `basename(file)`. This name may differ from
#'   the original filename on disk.
#' @param wikibase_api_url Character string. URL of the MediaWiki API
#'   endpoint associated with the target Wikibase instance.
#' @param csrf Authenticated session obtained with [get_csrf()]. The
#'   session is reused for the upload request and CSRF token retrieval.
#'
#' @details
#' The function uploads one file at a time using a multipart HTTP POST
#' request. The local file path is resolved before uploading.
#'
#' Upload warnings are not bypassed (`ignorewarnings = "0"`), so existing
#' files are not deliberately overwritten.
#'
#' If MediaWiki reports `fileexists-no-change`, the file is already
#' present with identical content. The function issues a warning and
#' returns a normal result with status `"Exists"`, rather than raising
#' an error. Other MediaWiki API errors stop execution.
#'
#' The function only uploads the media file. It does not create or
#' modify Wikibase items, claims, or statements. In particular, it does
#' not automatically create a `localMedia` statement linking the file
#' to a Wikibase item.
#'
#' @return
#' A one-row data frame with the following columns:
#' \describe{
#'   \item{filename}{Character. Filename registered in MediaWiki, or
#'     the requested filename if the file already exists.}
#'   \item{status}{Character. Upload outcome, including `"Success"`
#'     for a completed upload, `"Exists"` for an identical file already
#'     present, or `"Warning"` for other MediaWiki upload warnings.}
#'   \item{url}{Character. MediaWiki file description URL when returned
#'     by the API; otherwise `NA_character_`.}
#' }
#'
#' @seealso
#' [get_csrf()], [add_statement()]
#'
#' @examples
#' \dontrun{
#' csrf <- get_csrf(
#'   username = "MyBot",
#'   password = keyring::key_get("wikibase", "MyBot"),
#'   wikibase_api_url = "https://reprexbase.eu/rubik/api.php"
#' )
#'
#' result <- upload_local_media(
#'   file = "D:/_assets/rubik/Foto_tier2/T000_01151_tier2.jpg",
#'   wikibase_api_url = "https://reprexbase.eu/rubik/api.php",
#'   csrf = csrf
#' )
#'
#' print(result)
#' }
#'
#' @importFrom rlang %||%
#' @export
upload_local_media <- function(
    file,
    filename = basename(file),
    wikibase_api_url = "https://www.wikidata.org/w/api.php",
    csrf
) {

  if (!is.character(file) || length(file) != 1L ||
      is.na(file) || !nzchar(file)) {
    stop("`file` must be a single character string.")
  }

  if (!is.character(filename) || length(filename) != 1L ||
      is.na(filename) || !nzchar(filename)) {
    stop("`filename` must be a single character string.")
  }

  file <- normalizePath(file, winslash = "/", mustWork = TRUE)

  if (!file.exists(file) || dir.exists(file)) {
    stop("File does not exist or is not a regular file: ", file)
  }

  this_csrf_token <- get_csrf_token(csrf = csrf)

  response <- httr::POST(
    wikibase_api_url,
    body = list(
      action = "upload",
      filename = filename,
      file = httr::upload_file(file),
      token = this_csrf_token,
      format = "json",
      ignorewarnings = "0"
    ),
    encode = "multipart",
    handle = csrf
  )

  httr::stop_for_status(response)

  result <- httr::content(
    response,
    as = "parsed",
    type = "application/json"
  )

  if (!is.null(result$error)) {

    if (identical(result$error$code, "fileexists-no-change")) {
      warning("File already uploaded: ", filename, call. = FALSE)

      return(data.frame(
        filename = filename,
        status = "Exists",
        url = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    stop(
      "MediaWiki upload error [", result$error$code %||% "unknown",
      "]: ", result$error$info %||% "Unknown error."
    )
  }

  if (is.null(result$upload)) {
    stop("MediaWiki did not return an upload result.")
  }

  status <- result$upload$result %||% "Unknown"

  if (identical(status, "Warning")) {
    warning(
      "MediaWiki upload warning for ", filename, ": ",
      paste(names(result$upload$warnings), collapse = ", ")
    )
  }

  data.frame(
    filename = result$upload$filename %||% filename,
    status = status,
    url = result$upload$imageinfo$descriptionurl %||% NA_character_,
    stringsAsFactors = FALSE
  )
}
