#' Export DCI results to spatial format
#'
#' @inheritParams calculate_dci
#' @param results \code{dci.results} object or a list of objects of DCI results returned by \code{\link{calculate_dci}}.
#' @param type The element of the river network for which results should be reported. Can be either "rivers" or any of the nodes included in the \code{\link{river_net}} object such as "barriers" or "poi".
#'
#' @return A \code{\link[sf]{sf}} object with the corresponding DCI results joined to each feature.
#' @export
#'
#' @examples
#' \dontrun{
#' export_dci(net = net_name, results = dci_results, type = "rivers")
#' export_dci(net = net_name, results = dci_results, type = "poi")
#' export_dci(net = net_name, results = dci_results, type = "barriers")
#' export_dci(net = net_name, results = list(a, b, c), type = "rivers)
#' }
export_dci <- function(net, results, type = "rivers"){

 if(type == "rivers"){

    # Extract nodes
    nodes <- sf::st_as_sf(activate(net, nodes)) %>%
       dplyr::mutate(nodeID = dplyr::row_number())

    # Join nodes to rivers
    rivers <- sf::st_as_sf(activate(net, edges)) %>%
       dplyr::left_join(as.data.frame(nodes), by = c("from" = "nodeID"))

    # Convert single input to list
    if(!(inherits(results, "list"))){
       results <- list(results)
    }

    # Iterate over list of results
    for(i in 1:length(results)){

       # Rename result columns
       if(length(results) > 1){
          names(results[[i]])[names(results[[i]]) == "DCI"] <- paste0("DCI_", i)
          names(results[[i]])[names(results[[i]]) == "DCI_rel"] <- paste0("DCI_rel_", i)
       }

       # Join results
       rivers <- rivers %>%
          dplyr::left_join(results[[i]], by = c("member.label" = "segment"))
    }

   # Remove node label column
   rivers <- rivers[,!(names(rivers) == "node.label")]

   # Return result
   invisible(rivers)

 } else if(type == "barriers"){

   # Join results to barriers
   barriers <- sf::st_as_sf(activate(net, nodes)) %>%
     dplyr::filter(.data$type %in% c("barrier", "outlet")) %>%
     dplyr::left_join(results, by = c("member.label" = "segment"))
   barriers <- barriers[,!(names(barriers) == "node.label")]

   # Plot results
   plot(barriers["DCI"])

   # Return result
   invisible(barriers)

 } else if(type == "poi"){

   # Join results to others
   others <- sf::st_as_sf(activate(net, nodes)) %>%
     dplyr::filter(.data$type == "poi") %>%
     dplyr::left_join(results, by = c("member.label" = "segment"))
   others <- others[,!(names(others) == "node.label")]

   # Plot results
   plot(others["DCI"])

   # Return result
   invisible(others)

 } else{
   stop("type must be either 'rivers', 'barriers', or 'others' ")
 }
}
