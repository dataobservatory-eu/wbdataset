#' @title Get items
#' @description Get item definitions by QID from a Wikibase instance or Wikidata.
#' @param qid A single QID or a vector of QIDs on a Wikibase instance (or Wikidata itself.)
#' @param prefix The prefix to use before the QID, for example, defaults to
#' \code{"http://www.wikidata.org/entity/"}.
#' @param language A character vector of language codes, for example,
#' \code{c("en", "nl", "hu")}.
#' @param wikibase_api_url Defaults to \code{"https://www.wikidata.org/w/api.php"}.
#' @param creator The creator (author) of the dataset.
#' @param title The title of the dataset.
#' @importFrom dplyr left_join mutate relocate everything
#' @importFrom httr POST content
#' @importFrom utils person
#' @importFrom dataset dataset_df defined
#' @return A dataset with the QIDs, labels, description, and the language codes
#' of the labels and descriptions.
#' @export
#' @examples
#' get_item("Q42", language=c("en", "nl"))
#'

get_item <- function(qid,
                     language,
                     prefix = "http://www.wikidata.org/entity/",
                     wikibase_api_url = "https://www.wikidata.org/w/api.php",
                     creator = person("Jane", "Doe"),
                     title = "Dataset title") {

  qid <- gsub(prefix, "", as.character(qid))

  if (!all(is_qid(qid))) { stop("The QIDs do not appear to look like QIDs.") }

  if (length(qid)>1) {
    for (i in seq_along(qid)) {
      if (i==1) {
        return_df <- get_singe_item(qid=qid[i], language=language, wikibase_api_url=wikibase_api_url)
      } else {
        tmp <- get_singe_item(qid=qid[i], language=language, wikibase_api_url=wikibase_api_url)
        return_df <- rbind(return_df, tmp)
      }
    }
  } else {
    return_df <- get_singe_item(qid=qid, language=language, wikibase_api_url=wikibase_api_url)
  }

  return_ds <- dataset_df(qid = defined(return_df$qid, label = paste0("QID on ", wikibase_api_url), namespace=wikibase_api_url),
                          label = defined(return_df$label, label = "Label of item"),
                          description = defined(return_df$description, label = "Description of item"),
                          language = defined(return_df$language, label = "Language of label and description"),
                          reference=list(title = title, author=creator))

  wikibase_type <- c(qid = "QID")
  attr(return_ds, "wikibase_type") <- wikibase_type
  attr(return_ds, "class") <- c("wbdataset", attr(return_ds, "class"))
  return_ds
}

#' @keywords internal
get_singe_item <- function(qid,
                     language = c("en", "nl", 'hu'),
                     wikibase_api_url = "https://www.wikidata.org/w/api.php") {

  default_label  <- ""
  claim_body <- list(
    action = "wbgetentities",
    ids   = qid,
    format = "json")

  get_claim <- httr::POST(
    url = wikibase_api_url,
    body = claim_body,
    encode = "form"
  )

  response <- httr::content(get_claim, as = "parsed", type = "application/json")

  if (!is_response_success(response)) {
    message("Could not access ", qid)


    return(
      dataset_df(qid=defined(qid, label = "QID", namespace=wikibase_api_url),
                language = NA_character_,
                label = NA_character_ ,
                description = NA_character_)
    )
  }

  claims <- response$entities[[1]]$claims

  message("Downloaded ", response$entities[[1]]$id )

  labels_present <- language[which(language %in% names(response$entities[[1]]$labels))]
  labels_missing <- language[which(!language %in% names(response$entities[[1]]$labels))]

  descriptions_present <- language[which(language %in% names(response$entities[[1]]$descriptions))]
  descriptions_missing <- language[which(!language %in% names(response$entities[[1]]$descriptions))]

  labels_missing

  if ( "en" %in% names(response$entities[[1]]$labels) ) {
    default_label <-  response$entities[[1]]$labels$en$value
  } else {
    default_label <- response$entities[[1]]$sitelinks$enwiki$title
  }

  labels_missing_list <- list()

  for  ( l in labels_missing ) {

    labels_missing_list  <- c(labels_missing_list, tmp = list(list(language = l,
                                                                   value =default_label)))
    names(labels_missing_list)[which(names(labels_missing_list)=="tmp")] <- l
  }

  labels_list <- c(response$entities[[1]]$labels[labels_present], labels_missing_list)
  labels_list
  descriptions_missing_list <- list()
  for  ( d in descriptions_missing ) {
    descriptions_missing_list <- c(descriptions_missing_list,
                                   tmp = list(list(language = d, value = "")))
    names(descriptions_missing_list)[which(names(descriptions_missing_list)=="tmp")] <- d
  }

  descriptions_list <- c(response$entities[[1]]$descriptions[descriptions_present], descriptions_missing_list)
  a <- lapply(labels_list, unlist)
  b <- lapply(descriptions_list, unlist)
  label_df <- as.data.frame(do.call(rbind, a))
  description_df <- as.data.frame(do.call(rbind, b))

  if ( ! "value" %in% names(label_df) ) label_df$value <- NA_character_
  if ( ! "value" %in% names(description_df) ) description_df$value <- NA_character_

  if ( is_df_not_empty(label_df) ) {
    row.names(label_df) <- 1:dim(label_df)[1]
    names(label_df) <- c("language", "label")
  } else {
    label_df <- data.frame (language = NA_character_, label = NA_character_ )
  }

  if ( is_df_not_empty(description_df) ) {
    row.names(description_df) <- 1:dim(description_df)[1]
    names(description_df) <- c("language", "description")
  } else {
    description_df <- data.frame (language = NA_character_, description = NA_character_ )
  }

  return_df <- left_join(label_df,
                         description_df,
                         by = "language") %>%
    mutate (qid = qid) %>%
    relocate ( qid, .before=everything()) %>%
    relocate (language, .after=everything())

  return_df
}



