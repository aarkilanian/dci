#' River network for the Yamaska watershed
#'
#' A spatial dataset (a \code{\link{river_net}} object) containing a topologically dendritic representation of the rivers of the Yamaska watershed in Southern Quebec along with topological, barrier, and the outlet nodes identified.
#'
#' @format An object of class \code{\link{river_net}} holding an \code{\link[sf]{sf}} edges object with 588 \code{LINESTRING} geometries and 7 columns :
#' \describe{
#'   \item{geometry}{the geometry list column}
#'   \item{pass_1}{the passability of nodes in the network from 0 to 1}
#'   \item{pass_2}{the binary passability of nodes in the network, 0 if a barrier and 1 otherwise}
#'   \item{type}{the type of the node}
#'   \item{id}{an id column}
#'   \item{node.label}{a logical vector representing each node's unique topoligical ID}
#'   \item{member.label}{the segment membership of the node}
#' } and an \code{\link[sf]{sf}} nodes object with 589 \code{POINT} geometries and 7 columns:
#' \describe{
#'   \item{from}{the origin node of the line}
#'   \item{to}{the destination node of the line}
#'   \item{OBJECTID}{a unique river line id}
#'   \item{geometry}{the geometry list column}
#'   \item{riv_length}{the length of rivers, in meters}
#'   \item{weight}{a river weighting term}
#'   \item{rivID}{a unique river id}
#'   ...
#' }
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/grhq}
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/structure}
"yamaska"
