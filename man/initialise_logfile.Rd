% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initialise_logfile.R
\name{initialise_logfile}
\alias{initialise_logfile}
\title{Initialise a logfile}
\usage{
initialise_logfile(
  logfile_description = "A wbdataset logfile.",
  data_curator = NULL,
  wikibase_api_url = NULL
)
}
\arguments{
\item{logfile_description}{A short description of the log file. Defaults to \code{"A wbdataset logfile."}.}

\item{data_curator}{The name of the data curator who runs the function and
creates the log file, created with \link[utils]{person}. If left to default \code{NULL},
\code{utils::person("Person", "Unknown")} will be used.}

\item{wikibase_api_url}{For example,
\code{'https://example.com/wikibase/api.php'}; defaults to \code{NULL} when
this example value will be filled in.}
}
\value{
Returns a dataset_df object. The columns are:
\itemize{
\item{"rowid"}{ A row identifier. }
\item{"action"}{ The create_item() function name. }
\item{"id_on_target"}{ The new item identifier (QID) on the targeted Wikibase.}
\item{"label"}{ The propery label}
\item{"description"}{ The description label}
\item{"language"}{ The language code of the label.}
\item{"datatype"}{ The datatype of the property, for example, \code{string}}
\item{"wikibase_api_url"}{ The MediaWiki API URL where the new property is created.}
\item{"equivalence_property"}{ The PID that connects an equivalence ID to the property.}
\item{"equivalence_id"}{ The ID of an equivalent property defined elsewhere.}
\item{"classification_property"}{ Not applicable for properties.}
\item{"classification_id"}{ Not applicable for properties.}
\item{"success"}{ TRUE if successfully created, FALSE if there was an error.}
\item{"comment"}{ A summary of the error messages(s), if success is FALSE.}
\item{"time"}{ The time when the action started.}
\item{"logfile"}{ The name of the CSV logfile.}
}
}
\description{
Initialise a logfile
}
\examples{
initialise_logfile(
   logfile_description = "My first logs",
   data_curator = person("Joe", "Doe"),
   wikibase_api_url = ""https://example.com/wikibase/api.php""
   )
}
\seealso{
\code{\link[utils:person]{utils::person()}}
}
