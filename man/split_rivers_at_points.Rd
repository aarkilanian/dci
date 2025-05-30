% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{split_rivers_at_points}
\alias{split_rivers_at_points}
\title{Split river lines around point locations}
\usage{
split_rivers_at_points(rivers, pts, tolerance = NULL)
}
\arguments{
\item{rivers}{A \code{rivers} object returned by \code{\link[=import_rivers]{import_rivers()}}.}

\item{pts}{An \code{\link[sf]{sf}} object, points at which to split river lines.}

\item{tolerance}{A numeric value specifying the snapping distance
(in map units) to align points to the river network. Defaults to \code{NULL},
meaning no snapping.}
}
\value{
A \code{\link{rivers}} object with non-split rivers replaced with two new features each at opposite sides of the node which splits it. All attributes assumed to be constant.
}
\description{
Split river lines around point locations
}
\keyword{internal}
