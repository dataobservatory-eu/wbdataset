#' @title Retrieve Wikidata item metadata (labels and descriptions)
#'
#' @description Retrieves basic metadata (labels and descriptions) for one or
#'   more Wikidata items, given their QIDs. Supports multiple languages and
#'   returns results in a tidy format.
#'
#' @details This function queries the Wikidata API to retrieve the \code{label}
#'   and \code{description} for each item specified in \code{qid_on_wikidata}.
#'   The metadata is returned in a consistent tabular format and can be used for
#'   display, annotation, or joining with other datasets.
#'
#'   By default, the function retrieves metadata in multiple languages,
#'   including English, French, German, Italian, and Spanish.
#'
#' @param title The title of the returned (log) dataset.
#' @param prefix The prefix to be used as a namespace for downloaded items.
#' @param qid_on_wikidata A character vector of Wikidata QIDs (e.g.,
#'   \code{"Q42"}).
#' @param language Defaults to \code{c("en")}. A character string of
#'   the languages in which the users wants to receive the labels and
#'   descriptions of the property. The vector of languages must use \href{https://en.wikipedia.org/wiki/IETF_language_tag}{BCP
#'   47}-compliant language tags (e.g., "en" for English, and "hu"
#'   for Hungarian.)
#' @param wikibase_api_url The full URL of the Wikibase API endpoint (must end
#'   with \code{api.php}).
#' @param data_curator The name of the data curator who runs the function and
#'   creates the log file, created with \link[utils]{person}.
#'   It is either given as a parameter or resolved from
#'   \code{wikibase_session}. If no curator is given, then filled with
#'   \code{person("Unknown", "Curator")}.
#' @param fallback_language A BCP 47 language code (e.g., "en") used as a fallback
#'   when no label is available in the requested language(s). If a label is missing
#'   in one or more specified languages, this fallback will be used instead when available.
#'   Defaults to \code{"en"}.
#'
#' @return A data frame with one row per QID, including columns \code{qid},
#'   \code{label}, and \code{description}, as well as optional language-specific
#'   metadata.
#'
#' @seealso \code{\link{get_claim}} to retrieve claims (properties) for Wikidata
#'   items.
#' @importFrom dplyr select mutate filter bind_rows relocate left_join
#' @importFrom dplyr everything
#' @importFrom tibble tibble
#' @importFrom dataset dataset_df defined dublincore
#' @importFrom utils write.table
#' @examples
#' \dontrun{
#' get_wikidata_item("Q42", language = c("hu"), fallback_language = "en")
#' }
#' @export

get_wikidata_item <- function(
    qid_on_wikidata,
    language,
    prefix = "http://www.wikidata.org/entity/",
    wikibase_api_url = "https://www.wikidata.org/w/api.php",
    data_curator = NULL,
    title = "Dataset title",
    fallback_language = "en") {
  if (!is.character(language) || length(language) == 0 || anyNA(language)) {
    stop("get_wikidata_item(): 'language' must be a non-empty character vector.")
  }

  # Credit the person who curates the data
  if (is.null(data_curator)) data_curator <- person("Jane", "Doe")

  assertthat::assert_that(
    inherits(data_curator, "person"),
    msg = 'copy_wikidata_item(..., data_curator): data_curator must be a person, like person("Jane, "Doe").'
  )

  qid_on_wikidata <- gsub(prefix, "", as.character(qid_on_wikidata))

  if (!all(is_qid(qid_on_wikidata))) {
    stop("Some of elements of the the qid_on_wikidata do not appear to look like QIDs.")
  }

  if (length(qid_on_wikidata) > 1) {
    # If there are more QIDs, then query them in a loop ....
    for (i in seq_along(qid_on_wikidata)) {
      if (i == 1) {
        # Initialise the return_df
        return_df <- get_single_item(
          qid_on_wikidata = qid_on_wikidata[i],
          language = language,
          wikibase_api_url = wikibase_api_url
        )
      } else {
        tmp <- get_single_item(
          qid_on_wikidata = qid_on_wikidata[i],
          language = language,
          wikibase_api_url = wikibase_api_url
        )
        return_df <- rbind(return_df, tmp)
      }
    }
  } else {
    # No loop is needed if there is only a single item that needs to be
    # fetched by QID.
    return_df <- get_single_item(
      qid_on_wikidata = qid_on_wikidata,
      language = language,
      wikibase_api_url = wikibase_api_url
    )
  }

  return_ds <- dataset_df(
    qid_on_wikidata = defined(
      return_df$qid_on_wikidata,
      label = paste0("qid_on_wikidata on ", wikibase_api_url),
      namespace = wikibase_api_url
    ),
    label = defined(return_df$label, label = "Label of entity"),
    description = defined(return_df$description, label = "Description of entity"),
    language = defined(return_df$language, label = "Language of label and description"),
    dataset_bibentry = dublincore(
      title = title,
      creator = data_curator,
      dataset_date = Sys.Date()
    )
  )

  wikibase_type <- c(qid_on_wikidata = "qid_on_wikidata")
  attr(return_ds, "wikibase_type") <- wikibase_type
  attr(return_ds, "class") <- c("wbdataset", attr(return_ds, "class"))
  return_ds
}

#' @keywords internal
get_single_item <- function(qid_on_wikidata,
                            language = c("en", "nl", "hu"),
                            fallback_language = "en",
                            wikibase_api_url = "https://www.wikidata.org/w/api.php") {
  default_label <- ""
  claim_body <- list(
    action = "wbgetentities",
    ids = qid_on_wikidata,
    format = "json"
  )

  get_claim <- httr::POST(
    url = wikibase_api_url,
    body = claim_body,
    encode = "form"
  )

  response <- httr::content(get_claim, as = "parsed", type = "application/json")

  if (!is_response_success(response)) {
    message("Could not access ", qid_on_wikidata)


    return(
      dataset_df(
        qid_on_wikidata = defined(qid_on_wikidata, label = "qid_on_wikidata", namespace = wikibase_api_url),
        language = NA_character_,
        label = NA_character_,
        description = NA_character_
      )
    )
  }

  claims <- response$entities[[1]]$claims

  message("Downloaded ", response$entities[[1]]$id)

  labels_present <- language[which(language %in% names(response$entities[[1]]$labels))]
  labels_missing <- language[which(!language %in% names(response$entities[[1]]$labels))]

  descriptions_present <- language[which(language %in% names(response$entities[[1]]$descriptions))]
  descriptions_missing <- language[which(!language %in% names(response$entities[[1]]$descriptions))]

  labels_missing

  if ("en" %in% names(response$entities[[1]]$labels)) {
    default_label <- response$entities[[1]]$labels$en$value
  } else {
    default_label <- tryCatch(
      {
        response$entities[[1]]$sitelinks$enwiki$title
      },
      error = function(e) NA_character_
    )
  }

  labels_missing_list <- list()

  for  (l in labels_missing) {
    labels_missing_list <- c(labels_missing_list, tmp = list(list(
      language = l,
      value = default_label
    )))
    names(labels_missing_list)[which(names(labels_missing_list) == "tmp")] <- l
  }

  labels_list <- c(response$entities[[1]]$labels[labels_present], labels_missing_list)
  labels_list
  descriptions_missing_list <- list()
  for  (d in descriptions_missing) {
    descriptions_missing_list <- c(descriptions_missing_list,
      tmp = list(list(language = d, value = ""))
    )
    names(descriptions_missing_list)[which(names(descriptions_missing_list) == "tmp")] <- d
  }

  descriptions_list <- c(response$entities[[1]]$descriptions[descriptions_present], descriptions_missing_list)
  a <- lapply(labels_list, unlist)
  b <- lapply(descriptions_list, unlist)
  label_df <- as.data.frame(do.call(rbind, a))
  description_df <- as.data.frame(do.call(rbind, b))

  if (!"value" %in% names(label_df)) label_df$value <- NA_character_
  if (!"value" %in% names(description_df)) description_df$value <- NA_character_

  if (is_df_not_empty(label_df)) {
    row.names(label_df) <- 1:dim(label_df)[1]
    names(label_df) <- c("language", "label")
  } else {
    label_df <- data.frame(language = NA_character_, label = NA_character_)
  }

  if (is_df_not_empty(description_df)) {
    row.names(description_df) <- 1:dim(description_df)[1]
    names(description_df) <- c("language", "description")
  } else {
    description_df <- data.frame(language = NA_character_, description = NA_character_)
  }

  return_df <- left_join(label_df,
    description_df,
    by = "language"
  ) %>%
    mutate(qid_on_wikidata = qid_on_wikidata) %>%
    relocate(qid_on_wikidata, .before = everything()) %>%
    relocate(language, .after = everything())

  return_df
}
