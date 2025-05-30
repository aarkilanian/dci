% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{join_attributes}
\alias{join_attributes}
\title{Join variables from network nodes to \code{\link{river_net}} object.}
\usage{
join_attributes(net, nodes, tolerance = NULL)
}
\arguments{
\item{nodes}{An \code{\link[sf]{sf}} object of "barrier", "outlet", and "poi" points.}

\item{tolerance}{A numeric value specifying the snapping distance
(in map units) to align points to the river network. Defaults to \code{NULL},
meaning no snapping.}
}
\value{
A \code{\link{river_net}} object with the supplied nodes' attributes joined to its nodes.
}
\description{
Join variables from network nodes to \code{\link{river_net}} object.
}
\keyword{internal}
