#' River network for the Yamaska watershed
#'
#' A spatial dataset (a \code{\link{river_net}} object) containing a topologically dendritic representation of the rivers of the Yamaska watershed in Southern Quebec along with topological, barrier, and the outlet nodes identified.
#'
#' @format An object of class \code{\link{river_net}} holding a pair of \code{\link[sf]{sf}} objects representing the edges (588 \code{LINESTRING} geometries and 7 columns) and the nodes (589 \code{POINT} geometries and 7 columns) :
#' \describe{
#'   \item{geometry (edges)}{the geometry list column}
#'   \item{pass_1 (edges)}{the passability of nodes in the network from 0 to 1}
#'   \item{pass_2 (edges)}{the binary passability of nodes in the network, 0 if a barrier and 1 otherwise}
#'   \item{type (edges)}{the type of the node}
#'   \item{id (edges)}{an id column}
#'   \item{node.label (edges)}{a logical vector representing each node's unique topoligical ID}
#'   \item{member.label (edges)}{the segment membership of the node}
#'   \item{from (nodes)}{the origin node of the line}
#'   \item{to (nodes)}{the destination node of the line}
#'   \item{OBJECTID (nodes)}{a unique river line id}
#'   \item{geometry (nodes)}{the geometry list column}
#'   \item{riv_length (nodes)}{the length of rivers, in meters}
#'   \item{weight (nodes)}{a river weighting term}
#'   \item{rivID (nodes)}{a unique river id}
#'   ...
#' }
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/grhq}
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/structure}
"yamaska"
