#' Barrier data for the Yamaska watershed
#'
#' An \code{\link[sf]{sf}} object reprenting barrier points of the Yamaska watershed
#'
#' @format An object of class \code{\link[sf]{sf}} objects representing 14 \code{POINT} geometries and 3 columns:
#' \describe{
#'   \item{pass_1}{the passability of nodes in the network from 0 to 1}
#'   \item{pass_2}{the binary passability of nodes in the network, 0 if a barrier and 1 otherwise}
#'   \item{geometry}{the geometry list column}
#'   ...
#' }
#'
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/structure}
"yamaska_bar"
