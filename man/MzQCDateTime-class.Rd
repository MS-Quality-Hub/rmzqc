% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mzQC.R
\docType{class}
\name{MzQCDateTime-class}
\alias{MzQCDateTime-class}
\alias{MzQCDateTime}
\title{An mzQC-formatted date+time in ISO8601 format, as required by the mzQC spec doc.}
\description{
The format is "\%Y-\%m-\%dT\%H:\%M:\%S".
}
\section{Fields}{

\describe{
\item{\code{datetime}}{A correctly formatted date time (use as read-only)}
}}


\examples{
   dt1 = MzQCDateTime$new("1900-01-01")  ## yields "1900-01-01T00:00:00Z"
   dt2 = MzQCDateTime$new(Sys.time())
   ## test faulty input
   ## errors with 'character string is not in a standard unambiguous format'
   try(MzQCDateTime$new('lala'), silent=TRUE)
   ## test roundtrip conversion from/to JSON
   dt2$fromData(jsonlite::fromJSON(jsonlite::toJSON(dt1)))

}
