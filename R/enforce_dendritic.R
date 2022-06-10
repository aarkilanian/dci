#' Enforce dendritic river topology
#'
#' This function provides an interface to tools which can correct non-dendritic geometries in a river network. This tool can correct divergent rivers and complex confluences.
#'
#' Divergent rivers are parts of a river network where a single tributary splits into two after a confluence. In a dendritic network small upstream rivers can only combine at confluences into a single river.
#'
#' Complex confluences occur when confluences have over 2 input tributaries. In a dendritic network two tributaries only must combine into one at confluences.
#'
#' If errors are being corrected manually, all divergent pairs must be reduced to only one river and complex confluences modified such that only 2 rivers join together. When corrections are done automatically the shorted divergent stream is kept.
#'
#' @inheritParams river_net
#'
#' @param correct A logical value, when \code{FALSE}, the default, corrections are not automatically applied and a \code{\link{sf}} object of lines is returned with topological errors indicated. If \code{TRUE} errors are automatically corrected.
#'
#' @return If \code{correct} is \code{FALSE}, a \code{sf} object with non-dendritic topology indicated in columns "divergent" and "complex". These error columns indicate for each river line if that river is part of a divergent pair or complex confluence. The columns are populated by integers which indicate with which river they share a topological error. If \code{correct} is \code{TRUE}, a \code{rivers} object with automatic topological corrections applied is returned.
#'
#' @export
enforce_dendritic <- function(rivers, correct = FALSE){

  # Create river network
  net <- sfnetworks::as_sfnetwork(rivers, length_as_weight = TRUE)

  # Correct complex confluences
  # If automatically correcting topology, use network with divergences corrected
  if(correct){
    net <- correct_divergences(net)
    net <- correct_complex(net)
    # Recalculate river lengths
    net$riv_length <- as.double(sf::st_length(net))
    # Return corrected rivers
    invisible(net)

  # If errors are set to be manually edited, use full network
  } else {
    net_div <- correct_divergences(net, correct)
    net_comp <- correct_complex(net_div, correct)
    invisible(net_comp)
  }

}

#' Correct river divergences
#'
#' @inheritParams calculate_dci
#' @inheritParams enforce_dendritic
#'
#' @return If correct is \code{TRUE} a \code{\link{river_net}} object with the shorter of each divergent pair removed. If correct is \code{FALSE} a \code{\link{}} object with divergent pairs identified with a shared number in the new "divergent" column.
#'
#' @keywords internal
#' @export
correct_divergences <- function(net, correct = TRUE){

  # If no corrections desired, find and return divergences
  if(!correct){
    # Find and identify divergent pairs
    riv_divergences <- activate(net, edges) %>%
      sf::st_as_sf(.data) %>%
      dplyr::group_by(.data$from) %>%
      dplyr::mutate(grp_size = dplyr::n()) %>%
      dplyr::mutate(divergent = dplyr::if_else(grp_size > 1, from, NA_integer_))
    # Return non-corrected divergences
    invisible(riv_divergences)
  }

  # Find and correct divergences. Always keep longest stream
  net_corrected <- activate(net, edges) %>%
    dplyr::group_by(.data$from) %>%
    dplyr::filter(.data$weight == max(.data$weight)) %>%
    tidygraph::ungroup(.data)

  # Identify components
  net_comp <- activate(net_corrected, nodes) %>%
    dplyr::mutate(component = tidygraph::group_components()) %>%
    dplyr::group_by(.data$component)

  # Determine largest component and extract
  big_comp <- sort(table(net_comp %>% activate(nodes) %>% data.frame() %>% dplyr::select(component)), decreasing = TRUE)[1]
  big_comp <- as.integer(names(big_comp))
  net_corrected <- net_comp %>%
    dplyr::filter(.data$component == big_comp)

  # Get number of removed rivers
  orig_num <- nrow(as.data.frame(activate(net, edges)))
  cor_num <- nrow(as.data.frame(activate(net_corrected, edges)))
  num_div <- orig_num - cor_num

  # Print number of corrected divergences
  if(num_div == 0){
    message("No divergences detected.")
    invisible(net)
  } else {
    message(paste0(num_div, " divergences corrected."))
    invisible(net_corrected)
  }
}

#' Correct complex confluences
#'
#' @inheritParams calculate_dci
#' @inheritParams enforce_dendritic
#'
#' @return If correct is \code{TRUE} a \code{\link{sf}} object of rivers with complex confluences separated into two closely located valid confluences. If correct is \code{FALSE} a \code{\link{sf}} object with rivers participating in complex confluences labeled with the same number ina  new "complex" column.
#'
#' @keywords internal
#' @export
correct_complex <- function(net, correct = TRUE){

  # Identify complex confluences
  net_undirected <- activate(tidygraph::convert(net, tidygraph::to_undirected), nodes)
  net_degree <- net_undirected %>%
    dplyr::mutate(nodeID = dplyr::n()) %>%
    dplyr::mutate(degree = tidygraph::centrality_degree())
  complex_nodes <- sf::st_as_sf(net_degree) %>%
    dplyr::filter(.data$degree >= 4) %>%
    dplyr::select(.data$degree)

  # If confluences have over 4 inputs recommend manual correction
  if(any(complex_nodes$degree > 4)) stop("Complex confluences with over 3 input tributaries have been detected. Use the standalone `enforce_dendritic()` and correct returned errors manually.")
  # If no errors return unchanged rivers
  if(length(complex_nodes$degree) == 0){
    message("No complex confluences found.")
    invisible(sf::st_as_sf(activate(net, edges)))

  # Correct complex confluences detected
  } else {

    # Print number of complex confluences found
    num_complex <- nrow(complex_nodes)
    message(num_complex, " complex confluences found.")

    # Extract network rivers
    rivers <- sf::st_as_sf(activate(net, edges)) %>%
      dplyr::mutate(rivID = 1:dplyr::n())
    sf::st_agr(rivers) <- "constant"
    # Add ID to complex nodes
    complex_nodes <- complex_nodes %>%
      dplyr::mutate(complexID = 1:dplyr::n())
    # Create small buffer around confluence point
    buffer <- sf::st_buffer(complex_nodes, dist = 1)
    buffer <- sf::st_cast(buffer, "MULTILINESTRING", group_or_split = FALSE)
    sf::st_agr(buffer) <- "constant"

    # If manual editing desired, identify complex confluence rivers
    if(!correct){
      complex_riv <- sf::st_join(rivers, buffer, left = FALSE)
      return(complex_riv)
    }

    # Create points at intersection of rivers and buffers
    buff_intersect <- sf::st_intersection(rivers, buffer)
    buff_intersect <- buff_intersect[c("rivID", "complexID", "to")]
    # Select points out of buffer intersections for new confluence nodes on downstream edges
    new_nodes <- buff_intersect %>%
      dplyr::group_by(.data$complexID, .data$to) %>%
      dplyr::tally() %>%
      dplyr::filter(.data$n == 1)
    # Identify associated rivers
    new_nodes <- dplyr::left_join(new_nodes, as.data.frame(buff_intersect[c("to", "rivID")]))
    new_nodes <- new_nodes[c("complexID", "rivID")]
    # Find closest rivers to new points
    modify_rivers <- integer(length = nrow(complex_nodes))
    for(confluence in new_nodes$complexID){
      # Gather participating rivers
      candidates <- buff_intersect[buff_intersect$complexID == confluence,]
      # Determine closest river to new confluence
      distances <- sf::st_distance(new_nodes[confluence,], candidates)
      ind <- which(distances == min(distances[distances > units::as_units(0, "m")]))
      modify_rivers[confluence] <- candidates$rivID[ind]
    }
    # Move endpoints of closest river lines
    # Split outlet rivers at new confluence locations
    for(i in 1:length(modify_rivers)){
      # Move river to new confluence
      # Get river geometry
      old_river <- sf::st_geometry(rivers[which(rivers$rivID == modify_rivers[i]),])
      # Get point geometry
      new_point <- sf::st_geometry(new_nodes[i,])
      point_x <- new_point[[1]][1]
      point_y <- new_point[[1]][2]
      # Get number of coordinates on river line
      num_coord <- length(old_river[[1]])
      # Update final coordinates with new point
      new_river <- old_river
      new_river[[1]][num_coord/2] <- point_x
      new_river[[1]][num_coord] <- point_y
      # Replace old geometry in rivers
      sf::st_geometry(rivers[which(rivers$rivID == modify_rivers[i]),]) <- sf::st_sfc(new_river)
      # Split outlet river at confluence
      # Get river geometry
      old_river <- sf::st_geometry(rivers[rivers$rivID == new_nodes$rivID[i],])
      num_coord <- length(old_river[[1]])
      start_x <- old_river[[1]][1]
      start_y <- old_river[[1]][num_coord/2 + 1]
      # Create first line segment from old confluence to new confluence
      new_river1 <- sf::st_sf(sf::st_sfc(sf::st_linestring(matrix(c(start_x,
                                               point_x,
                                               start_y,
                                               point_y), 2)), crs = sf::st_crs(rivers))) %>%
        dplyr::mutate(rivID = new_nodes$rivID[i])
      names(new_river1) <- c("geometry", "rivID")
      sf::st_geometry(new_river1) <- "geometry"
      # Create second line segment from new confluence to the end of original
      new_river2 <- sf::st_geometry(old_river)
      new_river2[[1]][1] <- point_x
      new_river2[[1]][num_coord/2 + 1] <- point_y
      new_river2 <- sf::st_sf(new_river2) %>%
        dplyr::mutate(rivID = nrow(rivers) + 1)
      names(new_river2)[names(new_river2) == "new_river2"] <- "geometry"
      sf::st_geometry(new_river2) <- "geometry"
      new_river2$rivID[1] <- nrow(rivers) + 1
      # Remove old river
      rivers <- rivers[!(rivers$rivID == new_nodes$rivID[i]),]
      # Add new rivers
      rivers <- dplyr::bind_rows(rivers, new_river1, new_river2)
    }
    # Return modified rivers
    invisible(rivers)
  }
}
