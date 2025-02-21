#' @title Initialise a logfile
#' @param logfile_description A short description of the log file. Defaults to \code{"A wbdataset logfile."}.
#' @param data_curator The name of the data curator who runs the function and
#'   creates the log file, created with \link[utils]{person}. If left to default \code{NULL},
#' \code{utils::person("Person", "Unknown")} will be used.
#' @param wikibase_api_url For example,
#'   \code{'https://example.com/wikibase/api.php'}; defaults to \code{NULL} when
#'   this example value will be filled in.
#' @export
#' @seealso [utils::person()]
#' @return Returns a dataset_df object. The columns are:
#' \itemize{
#'  \item{"rowid"}{ A row identifier. }
#'  \item{"action"}{ The create_item() function name. }
#'  \item{"id_on_target"}{ The new item identifier (QID) on the targeted Wikibase.}
#'  \item{"label"}{ The propery label}
#'  \item{"description"}{ The description label}
#'  \item{"language"}{ The language code of the label.}
#'  \item{"datatype"}{ The datatype of the property, for example, `string`}
#'  \item{"wikibase_api_url"}{ The MediaWiki API URL where the new property is created.}
#'  \item{"equivalence_property"}{ The PID that connects an equivalence ID to the property.}
#'  \item{"equivalence_id"}{ The ID of an equivalent property defined elsewhere.}
#'  \item{"success"}{ TRUE if successfully created, FALSE if there was an error.}
#'  \item{"comment"}{ A summary of the error messages(s), if success is FALSE.}
#'  \item{"time"}{ The time when the action started.}
#'  \item{"logfile"}{ The name of the CSV logfile.}
#' }
#' @examples
#' initialise_logfile(
#'    logfile_description = "My first logs",
#'    data_curator = person("Joe", "Doe"),
#'    wikibase_api_url = ""https://example.com/wikibase/api.php""
#'    )

initialise_logfile <- function(logfile_description="A wbdataset logfile.",
                               data_curator=NULL,
                               wikibase_api_url=NULL ) {

  if(is.null(data_curator)) data_curator <- person("Person", "Unknown")
  if(is.null(data_curator)) wikibase_api_url <- "https://example.com/wikibase/api.php"

  action_time <- Sys.time()

  return_dataframe <- data.frame(
    action = NA_character_,
    id_on_target = NA_character_,
    label = NA_character_,
    description = NA_character_,
    language =  NA_character_,
    datatype = NA_character_,
    wikibase_api_url = NA_character_,
    equivalence_property =  NA_character_,
    equivalence_id = NA_character_,
    success = FALSE,
    comment = NA_character_,
    time = NA_character_,
    logfile = NA_character_
  )


  return_ds <-  dataset_df(return_dataframe,
             dataset_bibentry = dublincore(
               title = paste0(
                 "Wikibase Data Model Log (",
                 strftime(action_time, "%Y-%m-%d %H:%M:%OS0"), ")"),
               creator = data_curator,
               description = logfile_description,
               dataset_date = Sys.Date()
             ),
             identifier = c("wbi" = wikibase_api_url))

  return_ds$rowid <- defined(return_ds$rowid, namespace = wikibase_api_url)

  return_ds[-1,]
}




