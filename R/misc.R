##
## Author: Chris Bielow
##

## Functions which are unrelated to mzQC, but helpful

#'
#' Converts a NULL to NA_character_; or returns the argument unchanged otherwise
#'
#' This is useful for missing list elements (which returns NULL),
#' but when the missing element in refClass should be NA_character_ (and NULL would return an error)
#'
#' @param char_or_NULL A string or NULL
#'
#' @examples
#'   NULL_to_charNA(NA)   ## NA
#'   NULL_to_charNA(NULL) ## NA_character_
#'   NULL_to_charNA("hi") ## "hi"
#'
#' @export
#'
NULL_to_charNA = function(char_or_NULL) {
  if (is.null(char_or_NULL)) return(NA_character_)
  return(char_or_NULL)
}


#'
#' Converts a NULL to NA; or returns the argument unchanged otherwise
#'
#' This is useful for missing list elements (which returns NULL),
#' but when the missing element in refClass should be NA (and NULL would return an error)
#'
#' @param var_or_NULL A variable of any kind or NULL
#'
#' @examples
#'   NULL_to_NA(NA)   ## NA
#'   NULL_to_NA(NULL) ## NA
#'   NULL_to_NA("hi") ## "hi"
#'
#' @export
#'
NULL_to_NA = function(var_or_NULL) {
  if (is.null(var_or_NULL)) return(NA)
  return(var_or_NULL)
}


#'
#' Tell if a string is undefined (NA or NULL); If yes, and its required by the mzQC standard,
#' we can raise an error.
#'
#' You can pass multiple strings, which are all checked.
#' If **any** of them is undefined, the function returns TRUE
#'
#' @param s A string to be checked for NA/NULL
#' @param ... More strings to be checked
#' @param verbose If TRUE and 's' is NULL/NA, will print the name of the variable which was passed in
#'
#' @examples
#' isUndefined(NA)       ## TRUE
#' isUndefined(NULL)     ## TRUE
#' isUndefined(NA, NULL) ## TRUE
#' isUndefined("")       ## FALSE
#' isUndefined("", NA)   ## TRUE
#' isUndefined(NA, "")   ## TRUE
#' isUndefined(1)        ## FALSE
#' myVar = NA
#' isUndefined(myVar)    ## TRUE, with warning "Variable 'myVar' is NA/NULL!"
#'
#' @export
#'
isUndefined = function(s, ..., verbose = TRUE)
{
  r = (is.na(s) || is.null(s))
  name_of_var = deparse(substitute(s))
  # omit the '.self' part of the variable's name
  name_of_var = gsub("^.self\\$", "", name_of_var)
  if (verbose && r) {
    warning(paste0("Variable '", name_of_var, "' is NA/NULL!"), immediate. = TRUE, call. = FALSE)
  }

  # anchor
  if (...length() == 0) return(r);

  ## check remaining args from ... by using '+'
  ## This forces evaluation, since we want the error messages for all arguments
  ## , not just the first which failed
  return(r + isUndefined(..., verbose = verbose) > 0)
}



#'
#' Removes the last suffix (including the last dot) from a filename.
#' If no dot exists, the full string is returned.
#'
#' @param filepath A filename (with optional path -- which is retained)
#' @return The input with removed suffix
#'
#' @examples
#'  removeFileSuffix("test.tar.gz")  # --> 'test.tar'
#'  removeFileSuffix("test.mzML")  # --> 'test'
#'  removeFileSuffix("/path/to/test.mzML")  # --> '/path/to/test'
#'  removeFileSuffix("test_no_dot")  # --> 'test_no_dot'
#'
#' @export
#'
removeFileSuffix = function(filepath)
{
  gsub("(.*)\\..*$", "\\1", filepath)
}

#'
#' Checks if filepath ends in suffix (ignoring lower/upper case differences). If suffix does not start with a '.' it is prepended automatically.
#'
#' @param filepath A relative or absolute path to a file, whose suffix is checked
#' @param suffix This is the suffix we expect (the '.' is prepended internally if missing)
#' @return TRUE if yes, FALSE otherwise
#'
#' @export
#'
#' @import tools
#'
#' @examples
#'   hasFileSuffix("bla.txt", "txt")    # TRUE
#'   hasFileSuffix("bla.txt", ".txt")   # TRUE
#'   hasFileSuffix("bla.txt", ".TXT")   # TRUE
#'   hasFileSuffix("foo", "")           # TRUE
#'   hasFileSuffix("", "")              # TRUE
#'   hasFileSuffix("bla.txt", "doc")    # FALSE
#'   hasFileSuffix("bla.txt", ".doc")   # FALSE
#'   hasFileSuffix("fo", ".doc")        # FALSE
#'   hasFileSuffix("", ".doc")          # FALSE
#'
hasFileSuffix = function(filepath, suffix)
{
  dot = '.'
  if (substr(suffix,1,1) == '.') suffix = substring(suffix, 2) # get rid of prefix '.'

  filepath = tolower(filepath)
  suffix = tolower(suffix)

  return(suffix == tools::file_ext(filepath))
}

#'
#' Checks the value's class type, which should match at least of the types given in
#' any_expected_class_types.
#'
#' @param value A certain value (e.g. a single value, data.frame etc)
#' @param any_expected_class_types A vector of valid class types, any of which the @p value should have
#' @param expected_length The expected length of value (usually to check if its a single value); 0 (default) indicates that length can be ignored
#'
#' @examples
#'   check_type(1, "numeric", 1)   # TRUE
#'   check_type("1", "numeric", 1) # FALSE
#'   check_type(1, "numeric", 2)   # FALSE
#'   check_type("ABC", "character", 1)             # TRUE
#'   check_type("ABC", "character")                # TRUE
#'   check_type("ABC", "character", 2)             # FALSE
#'   check_type(c("ABC", "DEF"), "character", 2)   # TRUE
#'   check_type(1.1, c("numeric", "double"))    # TRUE
#'   check_type(1.1, c("numeric", "double"), 1) # TRUE
#'   check_type(matrix(1:9, nrow=3), "matrix")   # TRUE
#'   check_type(data.frame(a=1:3, b=4:6), c("something", "data.frame"))   # TRUE
#'
#' @export
#'
check_type = function(value, any_expected_class_types, expected_length = 0)
{
  class_ok = length(intersect(any_expected_class_types, class(value))) > 0
  length_ok = (expected_length == 0) || (length(value) == expected_length)
  return (class_ok & length_ok)
}





