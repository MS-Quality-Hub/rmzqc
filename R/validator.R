##
## Author: Chris Bielow
##

## Functions to validate an mzQC file or in-memory object or an mzQC JSON string


#'
#' Get a syntax validator for mzQC
#'
#' @import jsonvalidate
#'
getSyntaxValidator = function()
{
  schema = system.file("schema/mzqc_schema.json", package = "rmzqc", mustWork = TRUE)
  v = jsonvalidate::json_validator(schema, engine = "ajv") ## we need schema draft-07
  return(v)
}

#'
#' Syntactically validates an mzQC document which is present as a file.
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
  if (!file.exists(filepath)) {
    stop("The file '", filepath, "' does not exist (or is not readable).")
  }
  data = readLines(filepath)
  validateFromString(data, verbose)
}


#'
#' Syntactically validates an mzQC document which is already in memory as JSON string.
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
  v = getSyntaxValidator()
  if (length(JSON_string) > 1) JSON_string = paste0(JSON_string, collapse = "\n") ## we need a single line for the validator
  valid_JSON = jsonlite::validate(JSON_string)
  if (!valid_JSON)
  {
    warning(attributes(valid_JSON), immediate. = TRUE)
    stop("The JSON string is not valid JSON.")
  }
  res = v(JSON_string, verbose = verbose)
  return(res)
}


#'
#' Syntactically validates an mzQC document which is already in memory as mzQC root object, as obtained by, e.g. readMZQC().
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
