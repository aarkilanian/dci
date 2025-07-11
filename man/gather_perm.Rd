% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_dci.R
\name{gather_perm}
\alias{gather_perm}
\title{Gather passability between two nodes}
\usage{
gather_perm(from, to, nodes)
}
\arguments{
\item{from}{The from node's label}

\item{to}{The to node's label}

\item{nodes}{An \code{sf} object of the nodes of the \code{\link{river_net}} object with river attributes joined.}
}
\value{
The passability from 0 to 1 between the given nodes.
}
\description{
Gather passability between two nodes
}
\keyword{internal}
