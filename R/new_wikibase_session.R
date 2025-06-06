#' @title Create a reusable Wikibase session list
#'
#' @description Creates a named list of commonly reused parameters for functions
#'   such as \code{create_item()} and \code{create_property()}, especially
#'   useful in batch workflows.
#'
#' @param language A BCP 47 language tag (e.g., \code{"en"}).
#' @param csrf A CSRF token object, typically obtained from
#'   \code{\link{get_csrf}}.
#' @param data_curator A \code{person()} object identifying the user running the
#'   session.
#' @param wikibase_api_url The full URL of the Wikibase API (must end with
#'   \code{api.php}).
#' @param log_file_name Optional. A path to a log file where edits should be
#'   recorded.
#' @param classification_property The PID of a property like "instance of" or
#'   "subclass of" to classify items created during the session. Defaults to
#'   \code{NA_character_}.
#' @param equivalence_property The PID of a property (e.g., \code{"P2"}) used to
#'   express equivalence with an external concept (e.g., from Wikidata or
#'   CIDOC-CRM).
#' @param pid_equivalence_property The PID in Wikibase that records the
#'   equivalent Wikidata PID as an external ID.
#' @param qid_equivalence_property The QID in Wikibase that records the
#'   equivalent Wikidata QID as an external ID.
#' @return A named list that can be passed to the \code{wikibase_session}
#'   argument of various creation functions.
#'
#' @examples
#' \dontrun{
#' my_session <- new_wikibase_session(
#'   language = "en",
#'   csrf = get_csrf("username", "password", "https://example.org/w/api.php"),
#'   data_curator = person("Jane", "Doe"),
#'   wikibase_api_url = "https://example.org/w/api.php",
#'   classification_property = "P2",
#'   equivalence_property = "P7"
#' )
#'
#' create_item(
#'   label = "example concept",
#'   description = "example description",
#'   classification_id = "Q5",
#'   equivalence_id = "Q42",
#'   wikibase_session = my_session
#' )
#' }
#'
#' @export

new_wikibase_session <- function(
    language = "en",
    csrf = NULL,
    data_curator = NULL,
    wikibase_api_url = NULL,
    log_file_name = NULL,
    pid_equivalence_property = NA_character_,
    qid_equivalence_property = NA_character_,
    classification_property = NA_character_,
    equivalence_property = NA_character_) {

  list(
    language = language,
    csrf = csrf,
    data_curator = data_curator,
    wikibase_api_url = wikibase_api_url,
    log_file_name = log_file_name,
    pid_equivalence_property = pid_equivalence_property,
    qid_equivalence_property = qid_equivalence_property,
    classification_property = classification_property,
    equivalence_property = equivalence_property
  )
}
