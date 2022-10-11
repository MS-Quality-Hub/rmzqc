##
## Author: Chris Bielow
##

# Convert strings to mzQC IDs or vice versa, etc.

#'
#' Create an 'MzQCqualityMetric' object from two inputs
#'
#' @details
#' The inputs are:
#'  * an ID of a QC metric, e.g. "MS:4000059" (number of MS1 spectra)
#'  * a value
#'
#' The value must be in the correct format depending on the metric. The value type (see below) is checked (a warning/error is given if mismatching):
#' The following requirements for values apply:
#'  * single value: R single value; the unit is obtained from the CVs 'has_units'
#'  * n-tuple: an R vector, e.g. using c(1,2,3), i.e. all values have the same type; the unit is obtained from the CVs 'has_units'
#'  * table:  an R data.frame(); all columns defined using CVs 'has_column' must be present (a warning/error is given otherwise)
#'  * matrix: an R matrix, i.e. all values have the same type; the unit is obtained from the CVs 'has_units'
#'
#'  Upon violation, an error (default) or a warning is emitted:
#'  \preformatted{
#'    toQCMetric(id = "MS:4000059", value = data.frame(n = 1)) # errors: wrong value format
#'  }
#'
#' @param id The CV accession
#' @param value The data, as computed by some QC software in the required format.
#' @param on_violation What to do when 'value' is not of the correct type (according to the given 'id')? Default: "error"; or "warn"
#'
#' @return An MzQCanalysisSoftware object
#'
#' @examples
#'    toQCMetric(id = "MS:4000059", value = 13405) # number of MS1 spectra
#'
#' @export
#'
toQCMetric = function(id, value, on_violation = c("error", "warn"))
{
  # set up reaction to violating CV restrictions
  if (on_violation[1] == "warn") on_vio_func = function(message){ warning(message)} else
  if (on_violation[1] == "error") on_vio_func = function(message){ stop(message)} else
    stop("on_violation must be 'warn' or 'error'")

  check_and_print = function(value, any_expected_class_types, metric_id, expected_length = 0)
  {
    if (!check_type(value, any_expected_class_types, expected_length)) {
      on_vio_func(paste0("Passed value of class '", paste0(class(value), collapse=","), "'. Expected any of: '", paste0(any_expected_class_types, collapse = ","),"' with expected length '", expected_length,  "' (given:", length(value), ") (due to metric with ID=", metric_id, ")"))
    }
  }


  CV = getCVSingleton()
  entry = CV$byID(id)
  is_a = CV$byID(entry$is_a)
  if (is_a$id == "MS:4000006") {        # matrix
    check_and_print(value, "matrix", id)
  } else if (is_a$id == "MS:4000005") { # table
    check_and_print(value, "data.frame", id)
  } else if (is_a$id == "MS:4000004") { # n-tuple
    check_and_print(value, c("integer", "numeric", "double", "character", "logical"), id)
  } else if (is_a$id == "MS:4000003") { # single value
    check_and_print(value, c("integer", "numeric", "double", "character", "logical"), id, 1)
  } else
  {
    on_vio_func("value type ", is_a$id, "/", is_a$name, " not supported!")
  }

  MzQCqualityMetric(accession = entry$id, name = entry$name, description = entry$def, value = value)
}

#'
#' From an ID, e.g. "MS:1003162" (for PTX-QC), and some additional information,
#' create an 'analysisSoftware' node for mzQC
#'
#'
#' @param id The CV accession
#' @param version The version of the tool which created the metric/mzQC
#' @param uri URI to the homepage, or if NULL (default), will be extracted from the definition in the PSI MS-CV (if possible)
#' @param value An optional name for the software (if different from the CV's name)
#'
#' @return An MzQCanalysisSoftware object
#'
#' @examples
#'    toAnalysisSoftware(id = "MS:1003162", version = "1.0.13")
#'
#' @export
#'
toAnalysisSoftware = function(id, version = "unknown", uri = NULL, value = NA_character_)
{
  CV = getCVSingleton()
  entry = CV$byID(id)
  if (is.null(uri))
  { # e.g.
    # def: "Proteomics (PTX) - QualityControl (QC) software for QC report generation and visualization." [DOI:10.1021/acs.jproteome.5b00780, PMID:26653327, https://github.com/cbielow/PTXQC/]
    uri = sub(".*,[ ]*(http.*)[ ]*][ ]*", "\\1", entry$def)
  }
  MzQCanalysisSoftware(accession = entry$id, name = entry$name, version = version, uri = uri, description = entry$def, value = value)
}


#'
#' For a given filename (e.g. "test.mzML"), check the suffix and translate it to an PSI-MS CV term, e.g. 'MS:1000584'
#'
#' The following mapping is currently known:
#' .raw    : MS:1000563 ! Thermo RAW format
#' .mzML   : MS:1000584 ! mzML format
#' .mzData : MS:1000564 ! PSI mzData format
#' .wiff   : MS:1000562 ! ABI WIFF format
#' .pkl    : MS:1000565 ! Micromass PKL format
#' .mzXML  : MS:1000566 ! ISB mzXML format
#' .yep    : MS:1000567 ! Bruker/Agilent YEP format
#' .dta    : MS:1000613 ! Sequest DTA format
#' .mzMLb  : MS:1002838 ! mzMLb format
#'
#' Falls back to 'MS:1000560 ! mass spectrometer file format' if no match could be found.
#'
#' Upper/lowercase is ignored, i.e. "mzML == mzml".
#'
#' @param filepath A filename (with optional path)
#' @return A CV term accession as string, e.g. 'MS:1000584'
#'
#' @examples
#'   filenameToCV("test.mZmL")  # MS:1000584
#'   filenameToCV("test.raw")  # MS:1000563
#'   filenameToCV(c("test.raw", "bla.mzML"))
#'
#' @export
#'
filenameToCV = function(filepath)
{
  if (length(filepath) > 1) return(sapply(filepath, filenameToCV))

  if (hasFileSuffix(filepath, ".raw")) return ("MS:1000563");
  if (hasFileSuffix(filepath, ".mzML")) return ("MS:1000584");
  if (hasFileSuffix(filepath, ".mzData")) return ("MS:1000564");
  if (hasFileSuffix(filepath, ".wiff")) return ("MS:1000562");
  if (hasFileSuffix(filepath, ".pkl")) return ("MS:1000565");
  if (hasFileSuffix(filepath, ".mzXML")) return ("MS:1000566");
  if (hasFileSuffix(filepath, ".yep")) return ("MS:1000567");
  if (hasFileSuffix(filepath, ".dta")) return ("MS:1000613");
  if (hasFileSuffix(filepath, ".mzMLb")) return ("MS:1002838");

  warning("File '", filepath, "' has an unknown suffix. Falling back to 'MS:1000560 ! mass spectrometer file format'.")
  return("MS:1000560")
}
