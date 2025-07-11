
# dci

<!-- badges: start -->

[![R-CMD-check](https://github.com/aarkilanian/dci/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aarkilanian/dci/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test coverage](https://codecov.io/gh/aarkilanian/dci/branch/master/graph/badge.svg)](https://app.codecov.io/gh/aarkilanian/dci?branch=main)
<!-- badges: end -->

## Overview

The `dci` package provides an R interface to the Dendritic Connectivity Index (DCI) allowing for the analysis of longitudinal connectivity in river networks. Given a set of spatial data representing river lines, barrier point locations and the location of the target watershed's outlet the `dci` package provides tools to calculate ecological connectivity with the DCI. There are options to calculate the potamodromous or diadromous form of the DCI as well as options to consider a distance threshold and/or weights to be applied to river lengths.

This package extends the tidy representation of geospatial network data from the [sfnetworks package](https://luukvdmeer.github.io/sfnetworks/). This provides access to a rich set of further network analysis tools from the [tidygraph package](https://tidygraph.data-imaginist.com/index.html) as well as data manipulation from the [tidyverse](https://www.tidyverse.org/) family of packages.

## Installation

### Development version

To install the development version of the `dci` package from this github page follow these instructions:
```r
require(devtools)
install_github("aarkilanian/dci")
```
