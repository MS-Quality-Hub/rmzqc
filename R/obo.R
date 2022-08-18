
#'
#' Get the information of each CV term from an obo file.
#'
#' @import ontologyIndex
#'
#' @param cv_obo_file A path to an .obo file
#' @return A data.frame containing CV term information
#'
#'
parseOBO = function(cv_obo_file){
  if (!file.exists(cv_obo_file))
  {
    warning("File ", cv_obo_file, " not found!\n")
  }
  suppressWarnings({
    # warns that some parent terms are not found (we don't want to bother the user with that)
    ontology = ontologyIndex::get_ontology(cv_obo_file, extract_tags = "everything")
  })
  return(as.data.frame(ontology))
}

#'
#' Parse the content of 'psi-ms.obo', 'pato.obo', and 'uo.obo' from the 'rmzqc/cv/' folder as ontology and return their union
#'
#' See CV_ class to use this function efficiently.
#'
#' @return a data.frame with columns 'id', 'name', 'def', 'parents', 'children' (and many more) which contains the CV entries
#'
#' @export
#'
getCVDictionary = function()
{
  ms = parseOBO(system.file("./cv/psi-ms.obo", package="rmzqc"))
  pato = parseOBO(system.file("./cv/pato.obo", package="rmzqc"))
  uo = parseOBO(system.file("./cv/uo.obo", package="rmzqc"))
  # combining PSI MS and upstream CVs (rbind does not work, since non-standard columns differ)
  msuo = merge(ms, uo, all = TRUE)
  all = merge(msuo, pato, all = TRUE)
  return(all)
}


#'
#' Returns an MzQCcontrolledVocabulary which points to the PSI-MS CV which is currently shipped with this package
#'
#' @export
#'
getDefaultCV = function()
{
  ver = getDefaultCVVersion()
  MzQCcontrolledVocabulary$new(
    "Proteomics Standards Initiative Mass Spectrometry Ontology",
    paste0("https://github.com/HUPO-PSI/psi-ms-CV/releases/download/v", ver, "/psi-ms.obo"),
    ver)
}

#'
#' Obtains the current 'data-version' from the MS-CV shipped with this package
#'
#' @examples
#'  getDefaultCVVersion() # "4.1.95"
#'
#' @export
#'
getDefaultCVVersion = function()
{
  ## determine the version of the PSI MS-CV we are currently shipping
  ff = system.file("./cv/psi-ms.obo", package="rmzqc")
  head = scan(file = ff, what = "character", nmax = 20, quiet = TRUE)
  idx_v = grep("data-version", head)
  if (length(idx_v) == 0) stop("Parsing 'data-version' from the file '", ff, "' failed. Please report this as a bug.")
  return(head[idx_v + 1])
}


#'
#' Define a Singleton class which can hold a CV dictionary (so we do not have to load the .obo files over and over again)
#'
#' Usage:
#'   cv_dict = CV_$new() ## uses 'getCVDictionary()' to populate the singleton
#'   cv_2 = CV_$new() ## uses the same data without parsing again
#'
#' Wherever you need this data, simply re-grab the singleton using 'CV_$new()$data'
#'
#' @import R6
#' @import R6P
#'
#' @export
#'
CV_ <- R6::R6Class(classname = "CV_",
                   inherit = R6P::Singleton,
                   public = list(
  #' @field data Stores the data of the singleton.
  data = get0("self$data", ifnotfound = getCVDictionary()),
  #' @description A function to retrieve a CV using its ID
  #' @param id A CV accession, e.g. 'MS:1000560'
  byID = function(id) {
    idx = which(self$data$id == id)
    if (length(idx)== 0)
    {
      warning("Could not find id '",id,"' in CV list (length: ", length(self$data$id), ")")
      return(NULL)
    }
    return(self$data[idx,])
  }
))
