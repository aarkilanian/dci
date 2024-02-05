#' Export DCI results to spatial format
#'
#' @inheritParams calculate_dci
#' @param results \code{dci_results} object or a list of objects of DCI results returned by \code{\link{calculate_dci}}.
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
#' export_dci(net = net_name, results = list(a, b, c), type = "rivers")
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

   # Plot result if only one result joined
   if(length(results) == 1){
      plot(rivers["DCI"])
   }

   # Return result
   invisible(rivers)

 } else if(type == "barriers"){

    # Extract only barrier and outlet nodes
    barriers <- sf::st_as_sf(activate(net, nodes)) %>%
       dplyr::filter(.data$type %in% c("barrier", "outlet"))

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
       barriers <- barriers %>%
          dplyr::left_join(results[[i]], by = c("member.label" = "segment"))
    }

    # Remove node label column
    barriers <- barriers[,!(names(barriers) == "node.label")]


   # Plot results if only one result joined
   if(length(results) == 1){
      plot(barriers["DCI"])
   }

   # Return result
   invisible(barriers)

 } else if(type == "poi"){

    # Extract only points of interest
    poi <- sf::st_as_sf(activate(net, nodes)) %>%
       dplyr::filter(.data$type %in% c("barrier", "outlet"))

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
       barriers <- barriers %>%
          dplyr::left_join(results[[i]], by = c("member.label" = "segment"))
    }

    # Remove node label column
    barriers <- barriers[,!(names(barriers) == "node.label")]


    # Plot results if only one result joined
    if(length(results) == 1){
       plot(barriers["DCI"])
    }

    # Return result
    invisible(barriers)

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
