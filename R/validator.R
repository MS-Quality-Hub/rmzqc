##
## Author: Chris Bielow
##

## Functions to validate an mzQC file or in-memory object or an mzQC JSON string


#'
#' Get a validator for mzQC
#'
#' @import jsonvalidate
#'
getValidator = function()
{
  schema = system.file("schema/mzQC_schema.json", package = "rmzqc", mustWork = TRUE)
  v = jsonvalidate::json_validator(schema, engine = "ajv") ## we need schema draft-07
  return(v)
}

#'
#' Validates an mzQC document which is present as a file.
#'
#' The returned TRUE/FALSE has additional attributes in case of errors.
#' Use attributes(result) to access them.
#'
#' @param filepath A path to a file (e.g. "c:/my.mzQC", or "test.mzQC")
#' @param verbose Show extra information if validation fails
#' @return TRUE/FALSE if validation was successful/failed
#'
#' @export
#'
validateFromFile = function(filepath, verbose = TRUE)
{
  data = readLines(filepath)
  validateFromString(data, verbose)
}


#'
#' Validates an mzQC document which is already in memory as JSON string.
#' e.g. the string "{ mzQC : {}}"
#'
#' If the string object passed into this function contains multiple elements (length > 1).
#' then they will be concatenated using '\\n' before validation.
#'
#' The returned TRUE/FALSE has additional attributes in case of errors.
#' Use attributes(result) to access them.
#'
#' @param JSON_string A string which contains JSON (multiple lines allowed)
#' @param verbose Show extra information if validation fails
#' @return TRUE/FALSE if validation was successful/failed
#'
#' @export
#'
validateFromString = function(JSON_string, verbose = TRUE)
{
  v = getValidator()
  if (length(JSON_string) > 1) JSON_string = paste0(JSON_string, collapse = "\n") ## we need a single line for the validator
  res = v(JSON_string, verbose = verbose)
  return(res)
}


#'
#' Validates an mzQC document which is already in memory as mzQC root object, as obtained by, e.g. readMZQC().
#'
#' This method is less performant than validateFromString,
#' because it needs to convert the R object to
#' a JSON string first.
#'
#' The returned TRUE/FALSE has additional attributes in case of errors.
#' Use attributes(result) to access them.
#'
#' @param mzqc_root An mzQC root object
#' @param verbose Show extra information if validation fails
#' @return TRUE/FALSE if validation was successful/failed
#'
#' @export
#'
validateFromObj = function(mzqc_root, verbose = TRUE)
{
  if (!("MzQCmzQC" %in% class(mzqc_root))) stop("Argument is not a class of 'MzQCmzQC'.")
  JSON_string = jsonlite::toJSON(mzqc_root, pretty = TRUE, auto_unbox = TRUE)
  validateFromString(JSON_string, verbose)
}
