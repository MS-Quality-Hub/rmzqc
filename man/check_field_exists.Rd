% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{check_field_exists}
\alias{check_field_exists}
\title{Check if a field exists in data and warn if it doesn't, then return the field value}
\usage{
check_field_exists(
  data,
  field_name,
  class_name,
  context = NULL,
  default_value = NA
)
}
\arguments{
\item{data}{The data structure to check}

\item{field_name}{The name of the field to check for}

\item{class_name}{The name of the class being populated}

\item{context}{Optional context information for the warning message}

\item{default_value}{Value to return if the field doesn't exist (default: NA)}
}
\value{
The field value if it exists, otherwise the default_value
}
\description{
Check if a field exists in data and warn if it doesn't, then return the field value
}
\keyword{internal}
