#' @title Create a JSON for wbeditentity properties
#' @description
#' Creates a JSON string of the label(s), descriptions(s) in various languages,
#' adds the datatype. Required to define a property on Wikibase.
#' @param labels_list A list property labels organised by language.
#' @param descriptions_list A list of descriptions organised by language.
#' @param datatype An allowed datatype of the wbeditidentity, for example,
#' \code{"quantity"}.
#' @importFrom assertthat assert_that
#' @importFrom jsonlite toJSON
#' @return A JSON string for the wbeditentity API call
#' @keywords internal

property_identity_datastring_create <- function(labels_list,
                                                descriptions_list,
                                                datatype) {
  # Assert that the inputs are of correct types
  assertthat::assert_that(
    inherits(labels_list, "list"),
    msg = "property_datastring_create(labels_list, ...) must be a list."
  )

  assertthat::assert_that(
    inherits(descriptions_list, "list"),
    msg = "property_datastring_create(descriptions_list, ...) must be a list."
  )

  assertthat::assert_that(
    inherits(datatype, "character"),
    msg = "property_datastring_create(datatype, ...) must a character vector of length 1."
  )


  jsonlite::toJSON(
    list(
      labels = labels_list,
      descriptions = descriptions_list,
      datatype = datatype
    ),
    auto_unbox = T
  )
}
