##
## Author: Chris Bielow
##


#' Fills a MzQCqualityMetric object with id(accession) and name.
#' The value (if any) and unit (if any) need to be set afterwards.
#'
#' The accession must be valid (or allow_unknown_id must be TRUE)
#'
#' @param accession The ID (=accession) of the term in the CV
#' @param CV A CV dictionary, as obtained by getCVDictionary(); defaults to the global singleton, which is populated automatically
#' @param allow_unknown_id Allows invalid accession; if 'FALSE' this function errors if accession is unknown

#' @return An instance of MzQCqualityMetric
#'
#' @export
#'
getQualityMetricTemplate = function(accession, CV = getCVSingleton(), allow_unknown_id = FALSE)
{
  mzcv_dict = CV$getCV()
  idx = which(accession == mzcv_dict$id)
  if (length(idx) == 0 && !allow_unknown_id) stop("Accession '", accession, "' is not a valid CV term in the current dictionary (", length(mzcv_dict$id), " entries].")

  ## as.character() avoids the names() of the arguments to be forwarded into 'out'
  out = MzQCqualityMetric$new(accession = accession,
                              name = as.character(mzcv_dict$name[idx]),
                              description = as.character(mzcv_dict$def[idx]))
  return(out)
}


#' Fills a MzQCcvParameter object with id(accession) and name.
#' The value (if any) needs to be set afterwards.
#'
#' @param accession The ID (=accession) of the term in the CV
#' @param CV A CV dictionary, as obtained by getCVDictionary(); defaults to the global singleton, which is populated automatically
#'
#' @return An instance of MzQCcvParameter
#'
#' @export
#'
getCVTemplate = function(accession, CV = getCVSingleton())
{
  mzcv_dict = CV$getCV()
  idx = which(accession == mzcv_dict$id)
  if (length(idx) == 0) stop("Accession '", accession, "' is not a valid CV term in the current dictionary.")

  ## as.character() avoids the names() of the arguments to be forwarded into 'out'
  out = MzQCcvParameter$new(accession, as.character(mzcv_dict$name[idx]))
  return(out)
}
