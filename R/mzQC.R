##
## Author: Chris Bielow
##
## This file implements the basic mzQC data structures (see mzQC specification document)
## using R's ReferenceClasses for tighter control over input/output and invariants.
##
## We provide initialize() functions for all RefClasses to enable unnamed construction (shorter syntax)
##



##
# Defining this function to enable overload
# e.g. setMethod('asJSON', 'mzQC', function(x, ...) x$toJSON())
#  which allows to use
# jsonlite::toJSON(mzQC$new(content))
asJSON <- jsonlite:::asJSON


#'
#' Checks validity (= completeness) of mzQC objects - or lists (JSON arrays) thereof
#'
#' Note: Returns TRUE for empty lists!
#'
#' This function checks if an mzQC object or a list of mzQC objects is valid.
#' For lists, all elements need to be valid for the function to return TRUE.
#' The function provides detailed error messages that include the path to the invalid field,
#' making it easier to identify validation issues in complex nested structures.
#'
#' @param x An mzQC refclass (or list of them), which will be subjected to validation
#' @param parent_context Internal parameter used to track the path in nested validations
#'
#' @examples
#'   isValidMzQC(MzQCcvParameter$new("MS:4000059"))       # FALSE
#'   isValidMzQC(MzQCcvParameter$new("MS:4000059", "Number of MS1 spectra")) # TRUE
#'   isValidMzQC(list(MzQCcvParameter$new("MS:4000059"))) # FALSE
#'   isValidMzQC(list(MzQCcvParameter$new("MS:4000059", "Number of MS1 spectra"))) # TRUE
#'
#' @export
#'
#' @importFrom utils capture.output str
#'
isValidMzQC = function(x, parent_context = NULL)
{
  # anchor
  if (missing(x)) return(TRUE)

  # Build context string for better error reporting
  current_context <- if (!is.null(parent_context)) parent_context else ""

  if (inherits(x, "list")) {
    # Create a vector to store validation results
    idx = logical(length(x))

    # Process each item in the list with its index in the context
    for (i in seq_along(x)) {
      # Build context with index for this list item
      item_context <- paste0(current_context, "[", i, "]")
      # Validate the item with the indexed context
      idx[i] <- isValidMzQC(x[[i]], parent_context = item_context)
    }
    return(all(idx))
  }
  if (!inherits(x, "envRefClass")) {
    output <- capture.output(str(x))
    cat(paste0("Error: variable '", output, "' is not a RefClass, but should be!"))
    return(FALSE)
  }
  if (!("isValid" %in% x$getRefClass()$methods())) {
    stop("Invalid object: does not support 'isValid()'")
  }

  # Pass the context to the isValid method
  class_name <- class(x)[1]
  context_with_class <- paste0(current_context, if(nchar(current_context) > 0) "$" else "", class_name)

  # Call isValid with context information
  return(x$isValid(context = context_with_class))
}


#'
#' Allow conversion of a plain R object (obtained from jSON) to an mzQC object
#'
#' If you have a list of elements, call fromDatatoMzQC.
#'
#' @param mzqc_class Prototype of the class to convert 'data' into
#' @param data A datastructure of R lists/arrays as obtained by 'jsonlite::fromJSON()'
#'
#' @examples
#'  data = MzQCcvParameter$new("acc", "myName", "value")
#'  data_recovered = fromDatatoMzQCobj(MzQCcvParameter, jsonlite::fromJSON(jsonlite::toJSON(data)))
#'  data_recovered
#'
#' @export
#'
fromDatatoMzQCobj = function(mzqc_class, data)
{
  ## if not a list or vector and NA/NULL --> return a list
  if ((length(data) == 1) && (is.na(data) || is.null(data))) return(list())
  obj = mzqc_class$new()
  obj$fromData(data)
  return(obj)
}


#'
#' Allow conversion of plain named lists of R objects (from jSON) to mzQC objects
#'
#' @param mzqc_class Prototype of the class to convert 'data' into
#' @param data A list of: A datastructure of R lists/arrays as obtained by 'jsonlite::fromJSON()'
#'
#' @examples
#'     data = rmzqc::MzQCcvParameter$new("acc", "myName", "value")
#'     data_recovered = rmzqc::fromDatatoMzQC(rmzqc::MzQCcvParameter,
#'                          list(jsonlite::fromJSON(jsonlite::toJSON(data))))
#'
#' @export
#'
fromDatatoMzQC = function(mzqc_class, data)
{
  return(sapply(data, function(x) {
      fromDatatoMzQCobj(mzqc_class, x)
    }))
}


#'
#' An mzQC-formatted date+time in ISO8601 format, as required by the mzQC spec doc.
#'
#' The format is "%Y-%m-%dT%H:%M:%S".
#'
#' @field datetime A correctly formatted date time (use as read-only)
#'
#' @examples
#'    dt1 = MzQCDateTime$new("1900-01-01")  ## yields "1900-01-01T00:00:00Z"
#'    dt2 = MzQCDateTime$new(Sys.time())
#'    ## test faulty input
#'    ## errors with 'character string is not in a standard unambiguous format'
#'    try(MzQCDateTime$new('lala'), silent=TRUE)
#'    ## test roundtrip conversion from/to JSON
#'    dt2$fromData(jsonlite::fromJSON(jsonlite::toJSON(dt1)))
#     dt1$datetime == dt2$datetime    ## TRUE
#'
#' @exportClass MzQCDateTime
#' @export MzQCDateTime
#'
MzQCDateTime = setRefClass(
  'MzQCDateTime',
  fields = list(datetime = 'character'),
  methods = list(
    initialize = function(date = as.character(Sys.time()))
    {
      set(date)
    },
    set = function(.self, date)
    {
      .self$datetime = format(as.POSIXct(date, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")  # using ISO8601 format with UTC time
    },
    isValid = function(.self, context = "MzQCDateTime")
    {
      return(TRUE) ## always valid, because it's designed that way
    },
    toJSON = function(.self, ...)
    {
      if (!isValidMzQC(.self)) stop(paste0("Object of class '", class(.self), "' is not in a valid state for writing to JSON"))
      return(jsonlite:::asJSON(.self$datetime, ...))
    },
    fromData = function(.self, data)
    {
      .self$set(data)
      return(.self)
    }
  )
)
setMethod('asJSON', 'MzQCDateTime', function(x, ...) x$toJSON(...))


#'
#' A controlled vocabulary document, usually pointing to an .obo file
#'
#' @field name Full name of the controlled vocabulary.
#' @field uri Publicly accessible URI of the controlled vocabulary.
#' @field version (optional) Version of the controlled vocabulary.
#'
#' @examples
#'   MzQCcontrolledVocabulary$new(
#'     "Proteomics Standards Initiative Quality Control Ontology",
#'     "https://github.com/HUPO-PSI/psi-ms-CV/releases/download/v4.1.129/psi-ms.obo",
#'     "4.1.129")
#'
#' @exportClass MzQCcontrolledVocabulary
#' @export MzQCcontrolledVocabulary
#'
MzQCcontrolledVocabulary = setRefClass(
  'MzQCcontrolledVocabulary',
  fields = list(name = 'character',
                uri = 'character',
                version = 'character'    # optional
  ),
  methods = list(
    initialize = function(name = NA_character_, uri = NA_character_, version = NA_character_)
    {
      .self$name = name
      .self$uri = uri
      .self$version = version
    },
    isValid = function(.self, context = "MzQCcontrolledVocabulary") {
      if (isUndefined(.self$name, context = context)) {
        return(FALSE)
      }
      if (isUndefined(.self$uri, context = context)) {
        return(FALSE)
      }
      return(TRUE)
    },
    toJSON = function(.self, ...)
    {
      if (!isValidMzQC(.self)) stop(paste0("Object of class '", class(.self), "' is not in a valid state for writing to JSON"))

      r = list("name" = .self$name,
               "uri" = .self$uri,
               "version" = .self$version)
      return (jsonlite:::asJSON(r, ...))
    },
    fromData = function(.self, data)
    {
      .self$name = data$name
      .self$uri = data$uri
      .self$version = NULL_to_charNA(data$version)
      return(.self)
    }
  )
)
setMethod('asJSON', 'MzQCcontrolledVocabulary', function(x, ...) x$toJSON(...))


#'
#' A controlled vocabulary parameter, as detailed in the OBO file
#'
#' @field accession Accession number identifying the term within its controlled vocabulary.
#' @field name Name of the controlled vocabulary term describing the parameter.
#' @field value (optional) Value of the parameter.
#' @field description (optional) Definition of the controlled vocabulary term.
#'
#' @examples
#'   MzQCcvParameter$new("MS:4000070",
#'                       "retention time acquisition range",
#'                       c(0.2959, 5969.8172))
#'   isValidMzQC(MzQCcvParameter$new("MS:0000000"))
#'
#' @exportClass MzQCcvParameter
#' @export MzQCcvParameter
#'
MzQCcvParameter = setRefClass(
  'MzQCcvParameter',
  fields = list(accession = 'character',
                name = 'character',
                value = 'ANY',              # optional
                description = 'character'   # optional
  ),
  methods = list(
    initialize = function(accession = NA_character_, name = NA_character_, value = NA, description = NA_character_)
    {
      .self$accession = accession
      .self$name = name
      .self$value = value
      .self$description = description
    },
    isValid = function(.self, context = "MzQCcvParameter") {
      if (isUndefined(.self$accession, context = context)) {
        return(FALSE)
      }
      if (isUndefined(.self$name, context = context)) {
        return(FALSE)
      }
      return(TRUE)
    },
    toJSON = function(.self, ...)
    {
      if (!isValidMzQC(.self)) stop(paste0("Object of class '", class(.self), "' is not in a valid state for writing to JSON"))

      r = list("accession" = .self$accession,
               "name" = .self$name)
      if (!is.na(.self$description)) r["description"] = .self$description
      if (!is.na(.self$value)) r["value"] = .self$value
      return (jsonlite:::asJSON(r, ...))
    },
    fromData = function(.self, data)
    {
      .self$accession = data$accession
      .self$name = data$name
      .self$description = NULL_to_charNA(data$description)
      .self$value = NULL_to_NA(data$value)
      return(.self)
    }
  )
)
setMethod('asJSON', 'MzQCcvParameter', function(x, ...) x$toJSON(...))

#'
#' An inputfile within metadata for a run/setQuality
#'
#' @field name The name MUST uniquely match to a location (specified below) listed in the mzQC file.
#' @field location Unique file location, REQUIRED to be specified as a URI. The file URI is RECOMMENDED to be publicly accessible.
#' @field fileFormat An MzQCcvParameter with 'accession' and 'name'.
#' @field fileProperties An array of MzQCcvParameter, usually with 'accession', 'name' and 'value'. Recommended are at least two entries:
#'        a) Completion time of the input file (MS:1000747) and b) Checksum of the input file (any child of: MS:1000561 ! data file checksum type).
#'
#' @exportClass MzQCinputFile
#' @export MzQCinputFile
#'
MzQCinputFile = setRefClass(
  'MzQCinputFile',
  fields = list(name = 'character',
                location = 'character',
                fileFormat = 'MzQCcvParameter',
                fileProperties = 'list'         # array of MzQCcvParameter, optional
  ),
  methods = list(
    # defaults are required, otherwise refClasses do not work.
    initialize = function(name = NA_character_, location = NA_character_, fileFormat = MzQCcvParameter$new(), fileProperties = list())
    {
      .self$name = name
      .self$location = location
      .self$fileFormat = fileFormat
      .self$fileProperties = fileProperties
    },
    isValid = function(.self, context = "MzQCinputFile")
    {
      if (isUndefined(.self$name, context = context)) {
        return(FALSE)
      }
      if (isUndefined(.self$location, context = context)) {
        return(FALSE)
      }

      # Check fileFormat validity with proper context
      fileFormat_context <- paste0(context, "$fileFormat")
      if (!.self$fileFormat$isValid(context = fileFormat_context)) {
        return(FALSE)
      }

      if (!grepl(":", .self$location, fixed = TRUE) || (grepl("\\", .self$location, fixed = TRUE))) {
        # URI needs a ':' but must not contain a '\'
        warning(paste0(context, "$location (value: '", .self$location, "') is not a URI (e.g. 'file:///c:/tmp/test.raw' or 'http://...'). No '\\' are allowed"), immediate. = TRUE, call. = FALSE)
        return(FALSE)
      }

      # Check fileProperties with proper context
      return(isValidMzQC(.self$fileProperties, parent_context = paste0(context, "$fileProperties"))) ## TRUE for empty list, which is ok
    },
    toJSON = function(.self, ...)
    {
      if (!isValidMzQC(.self)) stop(paste0("Object of class '", class(.self), "' is not in a valid state for writing to JSON"))
      r = list(name = .self$name, location = .self$location, fileFormat = .self$fileFormat)
      if (length(.self$fileProperties) > 0) r$fileProperties = .self$fileProperties
      return (jsonlite:::asJSON(r, ...))
    },
    fromData = function(.self, data)
    {
      .self$name = data$name
      .self$location = data$location
      .self$fileFormat$fromData(data$fileFormat)
      .self$fileProperties = fromDatatoMzQC(MzQCcvParameter, data$fileProperties) ## for lists, call the free function
      return(.self)
    }
  )
)
setMethod('asJSON', 'MzQCinputFile', function(x, ...) x$toJSON(...))

#
# file_format = MzQCcvParameter$new("MS:1000584", "mzML format")
# nif = MzQCinputFile$new("tmp.mzML", "c:\\", file_format)
# nif
# nif2 = nif
# l2 = list(file_format, file_format)
# nif2$fileProperties = l2
# x = jsonlite::toJSON(nif, pretty = TRUE)
# x
# x2 = jsonlite::toJSON(nif2)
# xdata = jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
# xdata
# inherits(fromDatatoMzQC(MzQCcvParameter, xdata$fileProperties), "list")
# jsonlite::toJSON(xdata, pretty = TRUE, auto_unbox = T)
# isValidMzQC(l2)
# nif$fromData(xdata)




#'
#' Details of the software used to create the QC metrics
#'
#' @field accession Accession number identifying the term within its controlled vocabulary.
#' @field name Name of the controlled vocabulary term describing the software tool.
#' @field version Version number of the software tool.
#' @field uri Publicly accessible URI of the software tool or documentation.
#' @field description (optional) Definition of the controlled vocabulary term.
#' @field value (optional) Name of the software tool.
#'
#' @exportClass MzQCanalysisSoftware
#' @export MzQCanalysisSoftware
#'
MzQCanalysisSoftware = setRefClass(
  'MzQCanalysisSoftware',
  fields = list(accession = 'character',
                name = 'character',
                version = 'character',
                uri = 'character',          # optional
                description = 'character',  # optional
                value = 'character'         # optional
  ),
  methods = list(
    # defaults are required, otherwise refClasses do not work.
    initialize = function(accession = NA_character_,
                          name = NA_character_,
                          version = NA_character_,
                          uri = NA_character_,         ## optional
                          description = NA_character_, ## optional
                          value = NA_character_        ## optional
    )
    {
      .self$accession = accession
      .self$name = name
      .self$version = version
      .self$uri = uri
      .self$description = description
      .self$value = value
    },
    isValid = function(.self, context = "MzQCanalysisSoftware")
    {
      if (isUndefined(.self$accession, context = context)) {
        return(FALSE)
      }
      if (isUndefined(.self$name, context = context)) {
        return(FALSE)
      }
      if (isUndefined(.self$version, context = context)) {
        return(FALSE)
      }
      return(TRUE)
    },
    toJSON = function(.self, ...)
    {
      if (!isValidMzQC(.self)) stop(paste0("Object of class '", class(.self), "' is not in a valid state for writing to JSON"))

      r = list("accession" = .self$accession,
               "name" = .self$name,
               "version" = .self$version)
      if (!isUndefined(.self$uri, verbose = FALSE)) r$uri = .self$uri
      if (!isUndefined(.self$description, verbose = FALSE)) r$description = .self$description
      if (!isUndefined(.self$value, verbose = FALSE)) r$value = .self$value
      return (jsonlite:::asJSON(r, ...))
    },
    fromData = function(.self, data)
    {
      .self$accession = data$accession
      .self$name = data$name
      .self$version = data$version
      .self$uri = NULL_to_charNA(data$uri)
      .self$description = NULL_to_charNA(data$description)
      .self$value = NULL_to_charNA(data$value)
      return(.self)
    }
  )
)
setMethod('asJSON', 'MzQCanalysisSoftware', function(x, ...) x$toJSON(...))



#'
#' The metadata for a run/setQuality
#'
#' @field label Unique name for the run (for runQuality) or set (for setQuality).
#' @field inputFiles Array/list of MzQCinputFile objects
#' @field analysisSoftware Array/list of MzQCanalysisSoftware objects
#' @field cvParameters (optional) Array of cvParameters objects
#'
#' @exportClass MzQCmetadata
#' @export MzQCmetadata
#'
MzQCmetadata = setRefClass(
  'MzQCmetadata',
  fields = list(label = 'character',
                inputFiles = 'list',       # array of MzQCinputFile
                analysisSoftware = 'list', # array of MzQCanalysisSoftware
                cvParameters = 'list'      # optional array of MzQCcvParameter
  ),
  methods = list(
    initialize = function(label = NA_character_, inputFiles = list(), analysisSoftware = list(), cvParameters = list())
    {
      .self$label = label
      .self$inputFiles = inputFiles
      .self$analysisSoftware = analysisSoftware
      .self$cvParameters = cvParameters
    },
    isValid = function(.self, context = "MzQCmetadata")
    {
      if (isUndefined(.self$label, context = context)) {
        return(FALSE)
      }

      # Check inputFiles with proper context
      if (!isValidMzQC(.self$inputFiles, parent_context = paste0(context, "$inputFiles"))) {
        return(FALSE)
      }

      # Check analysisSoftware with proper context
      if (!isValidMzQC(.self$analysisSoftware, parent_context = paste0(context, "$analysisSoftware"))) {
        return(FALSE)
      }

      # Check cvParameters with proper context
      if (!isValidMzQC(.self$cvParameters, parent_context = paste0(context, "$cvParameters"))) {
        return(FALSE)
      }

      return(TRUE)
    },
    toJSON = function(.self, ...)
    {
      if (!isValidMzQC(.self)) stop(paste0("Object of class '", class(.self), "' is not in a valid state for writing to JSON"))

      r = list("label" = .self$label,
               "inputFiles" = .self$inputFiles,
               "analysisSoftware" = .self$analysisSoftware)
      ## only add if present (otherwise leads to 'cvParameters = []')
      if (length(.self$cvParameters) > 0 ) r$cvParameters = list(.self$cvParameters) ## extra list for the enclosing '[ ... ]'
      return (jsonlite:::asJSON(r, ...))
    },
    fromData = function(.self, data)
    {
      .self$label = data$label
      .self$inputFiles = fromDatatoMzQC(MzQCinputFile, data$inputFiles)
      .self$analysisSoftware = fromDatatoMzQC(MzQCanalysisSoftware, data$analysisSoftware)
      .self$cvParameters = fromDatatoMzQC(MzQCcvParameter, data$cvParameters)
      return(.self)
    }
  )
)
setMethod('asJSON', 'MzQCmetadata', function(x, ...) x$toJSON(...))

################################################################################################################################
#################################################################################################################################'
#' The central class to store QC information
#'
#' @field accession Accession number identifying the term within its controlled vocabulary.
#' @field name Name of the controlled vocabulary element describing the metric.
#' @field description (optional) Definition of the controlled vocabulary term.
#' @field value (optional) Value of the metric (single value, n-tuple, table, matrix).
#'        The structure is not checked by our mzQC implementation and must be handled by the caller, see \code{\link{toQCMetric}}.
#' @field unit (optional) Array of unit(s), stored as MzQcvParameter
#'
#' @exportClass MzQCqualityMetric
#' @export MzQCqualityMetric
#'
MzQCqualityMetric = setRefClass(
  'MzQCqualityMetric',
  fields = list(accession = 'character',
                name = 'character',
                description = 'character', # optional
                value = 'ANY',             # optional value of unspecified type
                unit = 'list'              # optional array of MzQCcvParameter
  ),
  methods = list(
    initialize = function(accession = NA_character_, name = NA_character_, description = NA_character_, value = NA, unit = list())
    {
      .self$accession = accession
      .self$name = name
      .self$description = description
      if (!missing(value)) .self$value = value else .self$value = NA  ## need to set as NA explicitly, because the default value 'uninitialized class ANY' cannot be converted to JSON
      .self$unit = unit
    },
    isValid = function(.self, context = "MzQCqualityMetric")
    {
      if (isUndefined(.self$accession, context = context)) {
        return(FALSE)
      }
      if (isUndefined(.self$name, context = context)) {
        return(FALSE)
      }

      # Check unit with proper context if it exists
      if (length(.self$unit) > 0) {
        if (!isValidMzQC(.self$unit, parent_context = paste0(context, "$unit"))) {
          return(FALSE)
        }
      }

      return(TRUE)
    },
    toJSON = function(.self, ...)
    {
      if (!isValidMzQC(.self)) stop(paste0("Object of class '", class(.self), "' is not in a valid state for writing to JSON"))

      r = list("accession" = .self$accession,
               "name" = .self$name,
               "description" = .self$description,
               "value" = .self$value   ## NA is written as "value": [null] and read back as NA
      )
      if (length(.self$unit) > 0) r$unit = .self$unit  ## optional
      return (jsonlite:::asJSON(r, ...))
    },
    fromData = function(.self, data)
    {
      .self$accession = data$accession
      .self$name = data$name
      .self$description = NULL_to_charNA(data$description)
      if (length(data$value) > 1 ||  ## could be an n-tuple or a single value
          !is.na(data$value)) .self$value = data$value
      if (exists('data$unit')) .self$unit = fromDatatoMzQC(MzQCcvParameter, list(data$unit)) ## if data$unit is empty, or NA, the empty list will be returned
      return(.self)
    }
  )
)
setMethod('asJSON', 'MzQCqualityMetric', function(x, ...) x$toJSON(...))


#a_qc_metric = MzQCqualityMetric$new("acc", "nnam")
#xq = jsonlite::toJSON(a_qc_metric)
#jsonlite::fromJSON(xq)


#'
#' Base class of runQuality/setQuality
#'
#' @field metadata The metadata for this run/setQuality
#' @field qualityMetrics Array of MzQCqualityMetric objects
#'
#' @exportClass MzQCbaseQuality
#' @export MzQCbaseQuality
#'
MzQCbaseQuality = setRefClass(
  'MzQCbaseQuality',
  fields = list(metadata = 'MzQCmetadata',
                qualityMetrics = 'list'), # array of MzQCqualityMetric
  methods = list(
    initialize = function(metadata = MzQCmetadata$new(), qualityMetrics = list())
    {
      .self$metadata = metadata
      .self$qualityMetrics = qualityMetrics
    },
    isValid = function(.self, context = "MzQCbaseQuality")
    {
      # Check metadata with proper context
      metadata_context <- paste0(context, "$metadata")
      if (!.self$metadata$isValid(context = metadata_context)) {
        return(FALSE)
      }

      # Check qualityMetrics with proper context
      if (!isValidMzQC(.self$qualityMetrics, parent_context = paste0(context, "$qualityMetrics"))) {
        return(FALSE)
      }

      return(TRUE)
    },
    getMetric = function(.self, accession = NULL, name = NULL) {
      if (! (is.null(accession) ^ is.null(name)))
      {
        stop("Exactly one of 'accession' or 'name' are required.")
      }

      results = lapply(.self$qualityMetrics, function(qmetric) {
        if (!is.null(accession) && qmetric$accession == accession)
        {
          return(qmetric)
        }
        if (!is.null(name) && qmetric$name == name)
        {
          return(qmetric)
        }
        else return(NULL) ## not the wanted metric
      })
      results_filtered <- Filter(Negate(is.null), results)
      if (length(results_filtered) == 0)
      {
        stop(paste0("No metric with name='", name, "' / accession='", accession, "' found."))
      }
      results_filtered
    },
    toJSON = function(.self, ...)
    {
      if (!isValidMzQC(.self)) stop(paste0("Object of class '", class(.self), "' is not in a valid state for writing to JSON"))

      r = list("metadata" = .self$metadata,
               "qualityMetrics" = .self$qualityMetrics)
      return (jsonlite:::asJSON(r, ...))
    },
    fromData = function(.self, mdata)
    {
      .self$metadata = fromDatatoMzQCobj(MzQCmetadata, mdata$metadata) ## metadata is a single element
      .self$qualityMetrics = fromDatatoMzQC(MzQCqualityMetric, mdata$qualityMetrics) ## if mdata$qualityMetrics is empty, or NA, the empty list will be returned
      return(.self)
    }
  )
)
setMethod('asJSON', 'MzQCbaseQuality', function(x, ...) x$toJSON(...))


#' Extract a certain metric from a runQuality's list of MzQCqualityMetric
#'
#' You must provide either the accession or the name of the metric, but not both.
#'
#' Usually there should be only one MzQCqualityMetric which matches,
#' however, this function will return all matches.
#'
#' Note: this function will stop() if not results are found
#'
#' @name MzQCbaseQuality_getMetric
#'
#' @param accession Accession of the MzQCqualityMetric
#' @param name Name of the MzQCqualityMetric (less stable than accession)
#' @return A list of MzQCqualityMetric's which match.
NULL


#'
#' A runQuality object. Use to report metrics for individual runs which are independent of other runs.
#'
#' The object is an alias for MzQCbaseQuality.
#'
#' @exportClass MzQCrunQuality
#' @export MzQCrunQuality
#'
MzQCrunQuality =  setRefClass(
  "MzQCrunQuality",
  contains = "MzQCbaseQuality"
)

#'
#' A setQuality object. Use it for metrics which are specific to sets, i.e. only for values which
#' only make sense in the set context and cannot be stored as runQuality (see mzQC spec doc).
#'
#' The object is an alias for MzQCbaseQuality.
#'
#' @exportClass MzQCsetQuality
#' @export MzQCsetQuality
#'
MzQCsetQuality =  setRefClass(
  "MzQCsetQuality",
  contains = "MzQCbaseQuality"
)

###########################################################################

#' Root element of an mzQC document
#'
#' At least one of runQualities or setQualities MUST be present.
#'
#' @field version Version of the mzQC format.
#' @field creationDate Creation date of the mzQC file.
#' @field contactName Name of the operator/creator of this mzQC file.
#' @field contactAddress Contact address (mail/e-mail or phone)
#' @field description Description and comments about the mzQC file contents.
#' @field runQualities Array of MzQCrunQuality;
#' @field setQualities Array of MzQCsetQuality
#' @field controlledVocabularies Array of CV domains used (obo files)
#'
#' @exportClass MzQCmzQC
#' @export MzQCmzQC
#'
MzQCmzQC = setRefClass(
  'MzQCmzQC',
  fields = list(version = 'character',
                creationDate = 'MzQCDateTime',
                contactName = 'character',            # optional
                contactAddress = 'character',         # optional
                description = 'character',            # optional
                runQualities = 'list',                # array of MzQCrunQuality         # hint: at least runQuality or a setQuality must be present
                setQualities = 'list',                # array of MzQCsetQuality
                controlledVocabularies = 'list'),     # array of MzQCcontrolledVocabulary
  methods = list(
    initialize = function(version = NA_character_,
                          creationDate = MzQCDateTime$new(),
                          contactName = NA_character_,
                          contactAddress = NA_character_,
                          description = NA_character_,
                          runQualities = list(),
                          setQualities = list(),
                          controlledVocabularies = list())
    {
      .self$version = version
      .self$creationDate = creationDate
      .self$contactName = contactName
      .self$contactAddress = contactAddress
      .self$description = description
      .self$runQualities = runQualities
      .self$setQualities = setQualities
      .self$controlledVocabularies = controlledVocabularies
    },
    isValid = function(.self, context = "MzQCmzQC")
    {
      if (isUndefined(.self$version, context = context))
      {
        return(FALSE)
      }

      # Check creationDate with proper context
      creationDate_context <- paste0(context, "$creationDate")
      if (!.self$creationDate$isValid(context = creationDate_context)) {
        return(FALSE)
      }

      # Check runQualities with proper context
      if (!isValidMzQC(.self$runQualities, parent_context = paste0(context, "$runQualities"))) {
        return(FALSE)
      }

      # Check setQualities with proper context
      if (!isValidMzQC(.self$setQualities, parent_context = paste0(context, "$setQualities"))) {
        return(FALSE)
      }

      # Check controlledVocabularies with proper context
      if (!isValidMzQC(.self$controlledVocabularies, parent_context = paste0(context, "$controlledVocabularies"))) {
        return(FALSE)
      }

      # at least one must be present
      if (length(.self$runQualities) + length(.self$setQualities) == 0)
      {
        warning(paste0(context, " must have at least one runQuality or setQuality (currently all empty)"), immediate. = TRUE, call. = FALSE)
        return(FALSE)
      }

      return(TRUE)
    },
    toJSON = function(.self, ...)
    {
      if (!isValidMzQC(.self)) stop(paste0("Object of class '", class(.self), "' is not in a valid state for writing to JSON"))

      r = list("version" = .self$version,
               "creationDate" = .self$creationDate)
      if (!isUndefined(.self$contactName, verbose = FALSE)) r$contactName = .self$contactName
      if (!isUndefined(.self$contactAddress, verbose = FALSE)) r$contactAddress = .self$contactAddress
      if (!isUndefined(.self$description, verbose = FALSE)) r$description = .self$description
      ## do not write them out if they are empty (leads to 'runQuality: []', which is invalid)
      if (length(.self$runQualities) > 0) r$runQualities = (.self$runQualities)
      if (length(.self$setQualities) > 0) r$setQualities = (.self$setQualities)
      r$controlledVocabularies = .self$controlledVocabularies
      return (jsonlite:::asJSON(list("mzQC" = r), ...))
    },
    fromData = function(.self, data)
    {
      root = data$mzQC
      if (is.null(root)) stop(gettextf("No valid mzQC root %s found. Cannot read data.", sQuote("mzQC")))
      .self$version = root$version
      .self$creationDate = fromDatatoMzQCobj(MzQCDateTime, root$creationDate)
      .self$contactName = NULL_to_charNA(root$contactName)
      .self$contactAddress = NULL_to_charNA(root$contactAddress)
      .self$description = NULL_to_charNA(root$description)
      .self$runQualities = fromDatatoMzQC(MzQCrunQuality, root$runQualities) ## if root$runQualities is empty, or NA, the empty list will be returned
      .self$setQualities = fromDatatoMzQC(MzQCsetQuality, root$setQualities) ## if root$setQualities is empty, or NA, the empty list will be returned
      .self$controlledVocabularies = fromDatatoMzQC(MzQCcontrolledVocabulary, root$controlledVocabularies)
      return(.self)
    }
  )
)
setMethod('asJSON', 'MzQCmzQC', function(x, ...) x$toJSON(...))
