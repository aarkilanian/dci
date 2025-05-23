#' River network for the Yamaska watershed
#'
#' A spatial `river_net` object extending the `tidygraph` representation of
#' spatial graphs. Nodes represent confluences, river endpoints, barriers, and
#' the outlet. Edges represent stream reaches. Spatial features are projected to
#' EPSG:32198.
#'
#' @format A `river_net` object with:
#'\describe{
#'  \item{Nodes}{
#'    \describe{
#'      \item{geometry}{the geometry list column of the node point features}
#'      \item{pass_1}{the passability of nodes in the network from 0 (impassable) to 1}
#'      \item{pass_2}{the binary passability of nodes in the network, 0 if a barrier and 1 otherwise}
#'      \item{type}{the type of the node: topological, barrier, or outlet}
#'    }
#'  }
#'  \item{Edges}{
#'    \describe{
#'      \item{from}{the row index of the origin node of the edge feature}
#'      \item{to}{the row index of the destination node of the edge feature}
#'      \item{qual}{a simulated weighting based on habitat quality of features}
#'      \item{riv_length}{length of edge features in meters}
#'      \item{rivID}{unique river feature integer ID}
#'      \item{geometry}{the geometry list column of edge line features}
#'    }
#'  }
#'}
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/grhq}
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/structure}
"yamaska_net"

#' River line features for the Yamaska watershed
#'
#' An `sf` object of the `LINESTRING` geometries that make up the rivers of the
#' Yamaska watershed of Southern Québec. The rivers make up the edges of the
#' `yamaska_net` object. Features are projected to CRS:32198.
#'
#' @format An `sf` object with 620 features:
#' \describe{
#'  \item{qual}{a simulated weighting based on habitat quality of features}
#'  \item{riv_length}{length of edge features in meters}
#'  \item{geometry}{the geometry list column of edge line features}
#' }
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/grhq}
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/structure}
"yamaska_rivers"

#' Barrier point features for the Yamaska watershed
#'
#' An `sf` object of the `POINT` geometries that make up imagined barriers on the
#' Yamaska watershed of Southern Québec. Features are projected to CRS:32198.
#'
#' @format An `sf` object with 620 LINESTRING features:
#' \describe{
#'  \item{pass_1}{the passability of nodes in the network from 0 (impassable) to 1}
#'  \item{pass_2}{the binary passability of nodes in the network, 0 if a barrier and 1 otherwise}
#'  \item{geometry}{the geometry list column of river `POINT` features}
#' }
"yamaska_barriers"

#' Outlet point feature for the Yamaska watershed
#'
#' An `sf` object of the `POINT` geometry of the outlet of the Yamaska watershed
#' of Southern Québec. The feature is projected to CRS:32198.
#'
#' @format An `sf` object with 1 POINT feature:
#' \describe{
#'  \item{geometry}{the geometry list column of the outlet `POINT` feature}
#' }
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/grhq}
#' @source \url{https://www.donneesquebec.ca/recherche/dataset/structure}
"yamaska_outlet"
