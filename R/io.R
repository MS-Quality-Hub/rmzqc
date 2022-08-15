##
## Author: Chris Bielow
##

#'
#' Writes a full mzQC object to disk.
#'
#' The filename should have an '.mzQC' as suffix (warning otherwise).
#'
#' @param filepath A filename (with path) to write to.
#' @param mzqc_obj An mzQC object, which is serialized to JSON and then written to disk
#'
#' @export
#'
writeMZQC = function(filepath, mzqc_obj)
{

  if (!hasFileSuffix(filepath, ".mzQC")) {
    warning("'", filepath, "' does not end in '.mzQC'. Please fix the output filename.")
  }

  content = jsonlite::toJSON(mzqc_obj, pretty = TRUE, auto_unbox = TRUE)

  cat(content, file = filepath)
}
