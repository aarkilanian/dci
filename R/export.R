#' Export DCI results to spatial format
#'
#' @inheritParams calculate_dci
#' @param A \code{\link{dci.results}} object of DCI results returned by \code{\link{calculate_dci}}.
#' @param type The element of the river network for which results should be reported. Can be either "rivers" or any of the nodes included in the \code{\link{river_net}} object such as "barriers" or "poi".
#'
#' @return A \code{\link{sf}} object with the corresponding DCI results joined to each feature.
#' @export
#'
#' @examples
export_dci <- function(net, results, type = "rivers"){

 if(type == "rivers"){

   # Extract nodes
   nodes <- sf::st_as_sf(activate(net, nodes))

   # Join results to rivers
   rivers <- activate(net, edges) %>%
     sf::st_as_sf(.data) %>%
     dplyr::left_join(.data, as.data.frame(nodes), by = c("from" = "nodeID")) %>%
     dplyr::left_join(.data, results, by = c("member.label" = "segment")) %>%
     dplyr::select(-.data$node.label)

   # Plot result
   print(
     ggplot() +
     geom_sf(data = rivers, aes(col = DCI)) +
     scale_colour_viridis_b()
   )

   # Return result
   invisible(rivers)

 } else if(type == "barriers"){

   # Join results to barriers
   barriers <- activate(net, nodes) %>%
     sf::st_as_sf(.data) %>%
     dplyr::filter(.data$type %in% c("Barrier", "outlet")) %>%
     dplyr::left_join(.data, results, by = c("member.label" = "segment")) %>%
     dplyr::select(-.data$node.label)

   # Plot results
   print(
   ggplot() +
     geom_sf(data = sf::st_as_sf(activate(net, edges)), col = "gray50") +
     geom_sf(data = barriers, aes(col = DCI)) +
     scale_colour_viridis_b()
   )

   # Return result
   invisible(barriers)

 } else if(type == "poi"){

   # Join results to others
   others <- activate(net, nodes) %>%
     sf::st_as_sf(.data) %>%
     dplyr::filter(.data, type == "poi") %>%
     dplyr::left_join(.data, results, by = c("member.label" = "segment")) %>%
     dplyr::select(-.data$node.label)

   # Plot results
   print(
   ggplot() +
     geom_sf(data = sf::st_as_sf(activate(net, edges)), col = "gray50") +
     geom_sf(data = others, aes(col = DCI)) +
     scale_colour_viridis_b()
   )

   # Return result
   invisible(others)

 } else{
   stop("type must be either 'rivers', 'barriers', or 'others' ")
 }
}
