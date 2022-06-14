#' Export DCI results to spatial format
#'
#' @inheritParams calculate_dci
#' @param results \code{dci.results} object of DCI results returned by \code{\link{calculate_dci}}.
#' @param type The element of the river network for which results should be reported. Can be either "rivers" or any of the nodes included in the \code{\link{river_net}} object such as "barriers" or "poi".
#'
#' @return A \code{\link[sf]{sf}} object with the corresponding DCI results joined to each feature.
#' @export
#'
#' @examples
export_dci <- function(net, results, type = "rivers"){

 if(type == "rivers"){

   # Extract nodes
   nodes <- sf::st_as_sf(activate(net, nodes)) %>%
     dplyr::mutate(nodeID = dplyr::row_number())

   # Join results to rivers
   rivers <- sf::st_as_sf(activate(net, edges)) %>%
     dplyr::left_join(as.data.frame(nodes), by = c("from" = "nodeID")) %>%
     dplyr::left_join(results, by = c("member.label" = "segment"))
   rivers <- rivers[,!(names(rivers) == "node.label")]

   # Plot result
   plot(rivers["DCI"])

   # Return result
   invisible(rivers)

 } else if(type == "barriers"){

   # Join results to barriers
   barriers <- sf::st_as_sf(activate(net, nodes)) %>%
     dplyr::filter(.data$type %in% c("barrier", "outlet")) %>%
     dplyr::left_join(results, by = c("member.label" = "segment"))
   barriers <- barriers[,!(names(barriers) == "node.label")]

   # Get rivers for plotting
   plot(sf::st_geometry(sf::st_as_sf(activate(net, edges))), col = "grey75")

   # Plot results
   plot(barriers["DCI"], add = TRUE)

   # Return result
   invisible(barriers)

 } else if(type == "poi"){

   # Join results to others
   others <- sf::st_as_sf(activate(net, nodes)) %>%
     dplyr::filter(.data$type == "poi") %>%
     dplyr::left_join(results, by = c("member.label" = "segment"))
   others <- others[,!(names(others) == "node.label")]

   # Get rivers for plotting
   plot(sf::st_geometry(sf::st_as_sf(activate(net, edges))), col = "grey75")

   # Plot results
   plot(others["DCI"], add = TRUE)

   # Return result
   invisible(others)

 } else{
   stop("type must be either 'rivers', 'barriers', or 'others' ")
 }
}
