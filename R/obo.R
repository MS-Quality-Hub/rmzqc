
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
  ontology = ontologyIndex::get_ontology(cv_obo_file, extract_tags = "everything")
  #obo = scan(file = cv_obo_file, what = "character")
  return(as.data.frame(ontology))
}

#'
#' Parse the content of 'psi-ms.obo' from the 'rmzqc/cv/' folder and as ontology
#'
#' See CV_ class to use this function efficiently.
#'
#' @return a list with 'id', 'name', 'def', 'parents', 'children' which contains the CV entries
#'
#' @export
#'
getCVDictionary = function()
{
  ms = parseOBO(system.file("./cv/psi-ms.obo", package="rmzqc"))
  return(ms)
  # combining (not needed anymore, due to merge of qc-cv)
  #qc = parseOBO(system.file("./cv/qc-cv.obo", package="rmzqc"))
  #return rbind(ms, qc)
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
#' @export
#'
CV2_ <- R6::R6Class(classname = "CV_",
                   inherit = R6P::Singleton,
                   public = list(
                     count = 0,
                     add_1 = function(){self$count = self$count + 1; invisible(self)},
  byID = function(id) {
    idx = which(self$data$id == id)
    return(data[idx,])
  },
  #' @field data Stores the data of the singleton.
  data = ifelse( exists("self$data"), data, getCVDictionary() )
                   ))

