% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enforce_dendritic.R
\name{correct_complex}
\alias{correct_complex}
\title{Correct complex confluences}
\usage{
correct_complex(net, correct = TRUE, quiet = FALSE)
}
\arguments{
\item{net}{A \code{\link{river_net}} object.}

\item{correct}{Logical. If \code{FALSE} (default), no changes are made and
topological issues are identified only. If \code{TRUE}, issues are
automatically corrected.}

\item{quiet}{Logical. If \code{FALSE}, prints the global DCI and a plot of river
segments to the console. Defaults to \code{TRUE}.}
}
\value{
If correct is \code{TRUE} a \code{\link[sf]{sf}} object of rivers with complex confluences separated into two closely located valid confluences. If correct is \code{FALSE} a \code{\link[sf]{sf}} object with rivers participating in complex confluences labeled with the same number ina  new "complex" column.
}
\description{
Correct complex confluences
}
\keyword{internal}
