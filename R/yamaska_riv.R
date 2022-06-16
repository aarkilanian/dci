#' River data for the Yamaska watershed
#'
#' An \code{\link[sf]{sf}} object reprenting river lines of the Yamaska watershed
#'
#' @format An object of class \code{\link[sf]{sf}} objects representing 712 \code{LINESTRING} geometries and 3 columns:
#' \describe{
#'   \item{FID}{A unique integer ID}
#'   \item{qual}{A habitat quality measure ranging from 0 to 10}
#'   \item{geometry}{the geometry list column}
#'   ...
#' }
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/grhq}
"yamaska_riv"
