##
## Author: Chris Bielow
##

# Convert strings to mzQC IDs or vice versa, etc.


#'
#' For a given filename, check the suffix (e.g. ".mzML") and translate it to an PSI-MS CV term, e.g. 'MS:1000584'
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
#' @param filepath A filename (with optional path)
#' @return A CV term accession as string, e.g. 'MS:1000584'
#'
#' @examples
#'   fileSuffixToCV("test.mZmL")  # MS:1000584
#'   fileSuffixToCV("test.raw")  # MS:1000563
#'   fileSuffixToCV(c("test.raw", "bla.mzML"))
#'
#' @export
#'
fileSuffixToCV = function(filepath)
{
  if (length(filepath) > 1) return(sapply(filepath, fileSuffixToCV))

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
