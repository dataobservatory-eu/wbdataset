#' @title Get item
#' @param qid QID
#' @param language descr
#' @importFrom dplyr left_join mutate relocate everything
#' @importFrom httr POST content
#' @export
#' @examples
#' qid = "Q42"
#'

get_item <- function(qid, languages) {

  if (!all(is_qid (qid))) { stop("Jaj") }

  if (length(qid)>1) {

  } else {
    get_singe_item(qid=qid, languages=languages)
  }
}

#' @keywords internal
get_singe_item <- function(qid,
                     languages = c("en", "nl", 'hu')) {

  default_label  <- ""
  claim_body <- list(
    action = "wbgetentities",
    ids   = qid,
    #languages = "en|nl|hu",
    #props = "labels",
    format = "json")

  get_claim <- httr::POST(
    "https://www.wikidata.org/w/api.php",
    body = claim_body,
    encode = "form"
  )

  response <- httr::content(get_claim, as = "parsed", type = "application/json")

  if (!is_response_success(response)) {
    message("Could not access ", qid_on_wikidata)
    return(  data.frame ( qid_on_wikidata  = qid_on_wikidata,
                          qid_on_wikibase =  NA_character_,
                          success = FALSE) )
  }

  claims <- response$entities[[1]]$claims

  message("Downloaded ", response$entities[[1]]$id )

  labels_present <- languages[which(languages %in% names(response$entities[[1]]$labels))]
  labels_missing <- languages[which(!languages %in% names(response$entities[[1]]$labels))]

  descriptions_present <- languages[which(languages %in% names(response$entities[[1]]$descriptions))]
  descriptions_missing <- languages[which(!languages %in% names(response$entities[[1]]$descriptions))]

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
  a <- lapply(labels_list , unlist)
  b <- lapply(labels_list , unlist)
  label_df <- as.data.frame(do.call(rbind, a))
  description_df <- as.data.frame(do.call(rbind, b))

  if (nrow(label_df) >1 ) {
    row.names(label_df) <- 1:dim(label_df)[1]
    names(label_df) <- c("language", "label")
  }

  if (nrow(description_df) >1 ) {
    row.names(description_df) <- 1:dim(description_df)[1]
    names(description_df) <- c("language", "description")
  }

  left_join(label_df, description_df, by = "language") %>%
    mutate (qid = qid) %>%
    relocate ( qid, .before=everything(),
               language, .after=everything())

}
