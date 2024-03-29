% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_dci.R
\name{calculate_dci}
\alias{calculate_dci}
\title{Calculate different forms of the DCI for a \code{\link{river_net}} object}
\usage{
calculate_dci(
  net,
  form,
  pass = NULL,
  weight = NULL,
  threshold = NULL,
  n.cores = 1,
  quiet = FALSE
)
}
\arguments{
\item{net}{A \code{\link{river_net}} object.}

\item{form}{A string specifying the form of the DCI to calculate: either
"potamodromous", "diadromous", or "invasive".}

\item{pass}{The name of a column in the nodes table of net which holds the
numeric passability of nodes. If none is specified all barriers are
automatically considered to have 0 passability.}

\item{weight}{The name of column in the edges tables of net which holds
numeric weights to be applied to river lengths. If none is specified, the
DCI is calculated only with river lengths.}

\item{threshold}{An optional numeric value specifying a dispersal limit in
map units. If NULL, the default, no limit is considered.}

\item{n.cores}{An optional integer value indicating the number of cores to
use. Defaults to 1. Currently only works on MacOS and Linux.}

\item{quiet}{A logical value indicating whether the global DCI and a map of
segments should be printed to the console. Defaults to TRUE.}
}
\value{
A \code{\link{sf}} object of the rivers from the provided
\code{\link{river_net}} object with new columns specifying the segmental
DCI values at each river location. If sites is not \code{NULL}, a \code{\link{sf}}
object of the site points with their associated DCI scores.
}
\description{
Calculate different forms of the DCI for a \code{\link{river_net}} object
}
\details{
Passability values are probabilities from 0 to 1 where 0 indicates fully
impassable and 1 indicates fully passable. If the values in the supplied
passability column are not within this range they will be normalized.

Similarly weighting values are probability from 0 to 1. Rivers with weights
of either 0 or NA will not be considered when calculating the DCI.

When DCI results are returned succesfully teh global DCI of the given river
network will be printed to the console.
}
\examples{
\dontrun{
calculate_dci(net = net_name, form = "all", pass = "pass",
  weight = "river_weight", threshold = 1500)
calculate_dci(net = net_name, form = "potamodromous")
calculate_dci(net = net_name, form = "diadromous", threshold = 2100)
}
}
