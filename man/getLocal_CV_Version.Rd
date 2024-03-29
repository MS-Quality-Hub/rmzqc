% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obo.R
\name{getLocal_CV_Version}
\alias{getLocal_CV_Version}
\title{Obtains the 'data-version' from a local (i.e. non-url) PSI-MS-CV}
\usage{
getLocal_CV_Version(local_PSIMS_obo_file)
}
\arguments{
\item{local_PSIMS_obo_file}{A path to a local file, e.g. 'c:/temp/my.obo'}
}
\description{
Obtains the 'data-version' from a local (i.e. non-url) PSI-MS-CV
}
\examples{
 getLocal_CV_Version(system.file("./cv/psi-ms.obo", package="rmzqc")) # "4.1.95"

}
