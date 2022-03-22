
#'
#' Get the information of each CV term from an obo file.
#'
#' @import ontologyIndex
#'
#' @param cv_obo_file A path to an .obo file
#' @return A list containing CV term information
#'
#'
parseOBO = function(cv_obo_file){
  ontology = ontologyIndex::get_ontology(cv_obo_file, extract_tags = "everything")
  #obo = scan(file = cv_obo_file, what = "character")
  return(ontology)
}

#'
#' Parse the content of 'qc-cv.obo' and 'psi-ms.obo' from the 'rmzqc/cv/' folder and return their union as ontology
#'
#' See CVDictionarySingleton to use this function efficiently.
#'
#' @return a list with 'id', 'name', 'def', 'parents', 'children' which contains the CV entries
#'
#' @export
#'
getCVDictionary = function()
{
  qc = parseOBO(system.file("./cv/qc-cv.obo", package="rmzqc"))
  ms = parseOBO(system.file("./cv/psi-ms.obo", package="rmzqc"))
  both = list()
  for (name in names(qc))
  {
    both[[name]] = append(qc[[name]], ms[[name]])
  }
  return(both)
}

#'
#' Define a Singleton class which can hold a CV dictionary (so we do not have to load the .obo files over and over again)
#'
#' Usage:
#'   cv_dict = CVDictionarySingleton$new()
#'   cv_dict$data = getCVDictionary() ## load the data once
#'    --> wherever you need this data, simply re-grab the singleton using 'CVDictionarySingleton$new()$data'
#'
#' @export
#'
CVDictionarySingleton <- R6::R6Class("CVDictionarySingleton", inherit = R6P::Singleton, public = list(
  #' @field data Stores the data of the singleton. Set the data once before using the singleton all over the place
  data = NULL
))

