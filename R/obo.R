
#'
#' Get the information of each CV term from an obo file.
#'
#' @import ontologyIndex
#'
#' @param cv_obo_file A local path to an .obo file
#' @return A data.frame containing CV term information
#'
#'
parseOBO = function(cv_obo_file){

  if (!file.exists(cv_obo_file))
  {
    stop("File ", cv_obo_file, " not found!\n")
  }
  suppressWarnings({
    # warns that some parent terms are not found (we don't want to bother the user with that)
    ontology = ontologyIndex::get_ontology(cv_obo_file, extract_tags = "everything")
  })
  return(as.data.frame(ontology))
}

#'
#' Fetch and parse the 'psi-ms.obo' and some metadata from the usual sources to use as ontology.
#'
#' A 'pato.obo', and 'uo.obo' from the 'rmzqc/cv/' folder are automatically merged in as well.
#'
#' See CV_ class to use this function efficiently.
#'
#' @param source Where to get the PSI-MS CV from:
#'         - 'latest' will download 'psi-ms.obo' from https://api.github.com/repos/HUPO-PSI/psi-ms-CV/releases/latest
#'         - 'local' will use rmzqc/cv/psi-ms.obo' (which might be outdated, if you need the latest terms)
#'         - 'custom' uses a user-defined URI in 'custom_uri'
#' @param custom_uri Used when 'source' is set to 'custom'. The URI can be local or remote, e.g. 'c:/obo/my.obo' or 'https://www.abc.com/my.obo'
#' @param use_local_fallback When downloading a file from a URI fails, should we fall back to the local psi-ms.obo shipped with rmzqc?
#' @return A list with 'CV', 'URI' and 'version', where 'CV' is a data.frame with columns 'id', 'name', 'def', 'parents', 'children' (and many more) which contains the CV entries
#'
#' @importFrom utils download.file
#'
#' @export
#'
getCVDictionary = function(source = c("latest", "local", "custom"), custom_uri = NULL, use_local_fallback = TRUE)
{
  source = source[1] ## pick first entry if defaulted
  URI_out = "" ## the URI we report to the outside world
  if (source == "latest")
  {
    custom_uri = getLatest_PSICV_URL()
    URI_out = custom_uri
  } else if (source == "local")
  {
    custom_uri = system.file("./cv/psi-ms.obo", package="rmzqc")
    URI_out = paste0("https://github.com/HUPO-PSI/psi-ms-CV/releases/download/v", getLocal_CV_Version(custom_uri), "/psi-ms.obo")
  } else if (source == "custom")
  {
    URI_out = custom_uri
  }
  else stop(paste0("Source ", source, " not supported!"))


  ## make sure its a local file
  if (any(startsWith(custom_uri, c("http://", "https://", "ftp://", "file://")))){
    message(paste0("Downloading obo from '", custom_uri, "' ..."))
    tmp_filename = tempfile()
    if (download.file(custom_uri, tmp_filename) != 0) stop("Could not download.")
    on.exit(file.remove(tmp_filename)) ## clean up when function ends
    local_file = tmp_filename
  } else local_file = custom_uri

  ms = try(parseOBO(local_file))
  if (use_local_fallback && inherits(ms, 'try-error') && source != "local") {
    return(getCVDictionary("local"))
  }

  pato = parseOBO(system.file("./cv/pato.obo", package="rmzqc"))
  uo = parseOBO(system.file("./cv/uo.obo", package="rmzqc"))
  # combining PSI MS and upstream CVs (rbind does not work, since non-standard columns differ)
  msuo = merge(ms, uo, all = TRUE)
  all = merge(msuo, pato, all = TRUE)

  version = getLocal_CV_Version(local_file);

  return(list(CV = all, URI = URI_out, version = version))
}


getLatest_PSICV_URL = function()
{
  temp_filename = tempfile()
  on.exit(file.remove(temp_filename))
  download.file("https://api.github.com/repos/HUPO-PSI/psi-ms-CV/releases/latest", temp_filename)
  cont = paste0(scan(temp_filename, what=character(), quiet = TRUE), collapse="")
  gsub('.*(https.*psi-ms\\.obo).*', '\\1', cont)
}

#'
#' Returns an MzQCcontrolledVocabulary for the currently used CV (see \code{\link{getCVSingleton}})
#'
#' @export
#'
getCVInfo = function()
{
  cv = getCVSingleton()
  MzQCcontrolledVocabulary$new(
    "Proteomics Standards Initiative Mass Spectrometry Ontology",
    cv$data$URI,
    cv$data$version)
}

#'
#' Returns an MzQCcontrolledVocabulary for the currently used CV (see \code{\link{getCVSingleton}})
#'
#' @note This function will be deprecated soon. Use \code{\link{getCVInfo}} instead.
#'
#' @export
#'
getDefaultCV = function()
{
  warning("The function 'rmzqc::getDefaultCV()' is deprecated and will be removed soon. Use rmzqc::getCVInfo() instead.", immediate. = TRUE)
  getCVInfo()
}


#'
#' Obtains the 'data-version' from a local (i.e. non-url) PSI-MS-CV
#'
#' @param local_PSIMS_obo_file A path to a local file, e.g. 'c:/temp/my.obo'
#' @examples
#'  getLocal_CV_Version(system.file("./cv/psi-ms.obo", package="rmzqc")) # "4.1.95"
#'
#' @export
#'
getLocal_CV_Version = function(local_PSIMS_obo_file)
{
  head = scan(file = local_PSIMS_obo_file, what = "character", nmax = 20, quiet = TRUE)
  idx_v = grep("data-version", head)
  if (length(idx_v) == 0) stop("Parsing 'data-version' from the file '", local_PSIMS_obo_file, "' failed. Please report this as a bug.")
  return(head[idx_v + 1])
}


#'
#' Define a Singleton class which can hold a CV dictionary (so we do not have to load the .obo files over and over again)
#'
#' You can set your own custom CV by calling 'setData()'. By default, the latest release of the PSI-MS-CV (see \code{\link{getCVDictionary}}).
#'
#' Usage:
#'   cv_dict = CV_$new() ## uses 'getCVDictionary()' to populate the singleton
#'   cv_2 = CV_$new() ## uses the same data without parsing again
#'   cv_2$setData(getCVDictionary("custom", "https://my.com/custom.obo))
#'
#' Wherever you need this data, simply re-grab the singleton using 'CV_$new()$data' (or use the convenience function getCVSingleton()$data from outside the package)
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
    idx = which(self$data$CV$id == id)
    if (length(idx)== 0)
    {
      warning("Could not find id '",id,"' in CV list (length: ", length(self$data$CV$id), ")")
      return(NULL)
    }
    return(self$data$CV[idx,])
  },
  #' @description Set a user-defined object (consisting of 'CV', 'URI' and 'version'), as obtained from \code{\link{getCVDictionary}}
  #' @param cv_data The result of a call to \code{\link{getCVDictionary}}
  setData = function(cv_data)
  {
    self$data = cv_data
  },
  #' @description Gets the CV data, i.e. the 'CV' part of this class
  getCV= function()
  {
    self$data$CV
  }
))


#'
#' Returns the CV singleton. See \code{\link{CV_}}.
#'
#' @export
#'
getCVSingleton = function()
{
  cv = rmzqc::CV_
  cv_dict = cv$new()
  return(cv_dict)
}
