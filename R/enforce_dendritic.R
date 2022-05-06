#' Enforce dendritic river topology
#'
#' This function provides an interface to tools which can correct non-dendritic geometries in a river network. This tool can correct divergent rivers and complex confluences.
#'
#' Divergent rivers are parts of a river network where a single tributary splits into two after a confluence. In a dendritic network small upstream rivers can only combine at confluences into a single river.
#'
#' Complex confluences occur when confluences have over 2 input tributaries. In a dendritic network two tributaries only must combine into one at confluences.
#'
#' If errors are being corrected manually, all divergent pairs must be reduced to only one river and complex confluences modified such that only 2 rivers join together.
#'
#' @inheritParams river_net
#'
#' @param divergence A logical value, when \code{TRUE}, the default, divergences are corrected.
#'
#' @param complex A logical value, when \code{TRUE}, the default, complex confluences are corrected.
#'
#' @return If \code{export} is \code{FALSE}, a \code{rivers} object with corrected dendritic topology. If \code{export} is \code{TRUE}, a \code{rivers} object with divergent river pairs given shared \code{divID} and rivers participating in complex confluences are given shared \code{complexID}.
#'
#' @export
enforce_dendritic <- function(rivers, divergence = TRUE, complex = TRUE){
  # Create river network
  net <- rivers %>%
    sfnetworks::as_sfnetwork(length_as_weight = TRUE)
  # Correct divergences
  if(divergence){
    net <- correct_divergences(net)
  }
  # Correct complex confluences
  if(complex){
    net <- correct_complex(net)
  }
  # Recalculate river lengths
  net$riv_length <- sf::st_length(net)
  # Return corrected rivers
  if("sfnetwork" %in% class(net)) invisible(net %>% activate(edges) %>% sf::st_as_sf())
  else invisible(net)
}

# Internal divergence correction function
correct_divergences <- function(net){
  # Find and correct divergences. Always keep longest stream
  riv_corrected <- net %>%
    activate(edges) %>%
    dplyr::group_by(from) %>%
    dplyr::filter(weight == max(weight)) %>%
    tidygraph::ungroup()
  # Remove small components (<10 nodes) left over
  riv_trimmed <- riv_corrected %>%
    activate(nodes) %>%
    dplyr::mutate(component = tidygraph::group_components()) %>%
    dplyr::group_by(component) %>%
    dplyr::filter(dplyr::n() > 10) %>%
    tidygraph::ungroup()
  # Get number of removed rivers
  num_div <- nrow(net %>%
                    activate(edges) %>%
                    tibble::as_tibble()) - nrow(riv_corrected %>%
                                                  activate(edges) %>%
                                                  tibble::as_tibble())
  # Print number of corrected divergences
  if(num_div == 0){
    message("No divergences detected.")
    invisible(net)
  } else {
    message(paste0(num_div, " divergences corrected."))
    invisible(riv_trimmed)
  }
}

# Internal complex confluence correction function
correct_complex <- function(net){
  # Identify complex confluences
  complex_nodes <- net %>%
    tidygraph::convert(tidygraph::to_undirected) %>%
    activate(nodes) %>%
    dplyr::mutate(nodeID = dplyr::n()) %>%
    dplyr::mutate(degree = tidygraph::centrality_degree()) %>%
    sf::st_as_sf() %>%
    dplyr::filter(degree >= 4) %>%
    dplyr::select(degree)
  # If confluences have over 4 inputs recommend manual correction
  if(any(complex_nodes$degree > 4)) stop("Complex confluences with over 3 input tributaries have been detected. Use the standalone `enforce_dendritic()` and correct returned errors manually.")
  # If no errors return unchanged network
  if(length(complex_nodes$degree) == 0){
    message("No complex confluences found.")
    invisible(net %>% activate(edges) %>% sf::st_as_sf())
    # Correct complex confluences detected
  } else {
    # Extract network rivers
    rivers <- net %>%
      activate(edges) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(rivID = 1:dplyr::n())
    sf::st_agr(rivers) <- "constant"
    # Add ID to complex nodes
    complex_nodes <- complex_nodes %>%
      dplyr::mutate(complexID = 1:dplyr::n())
    # Create small buffer around confluence point
    buffer <- sf::st_buffer(complex_nodes, dist = 1) %>%
      sf::st_cast("MULTILINESTRING", group_or_split = FALSE)
    sf::st_agr(buffer) <- "constant"
    # Create points at intersection of rivers and buffers
    buff_intersect <- sf::st_intersection(rivers, buffer)%>%
      dplyr::select(rivID, complexID, to)
    # Select points out of buffer intersections for new confluence nodes on downstream edges
    new_nodes <- buff_intersect %>%
      dplyr::group_by(complexID, to) %>%
      dplyr::tally() %>%
      dplyr::filter(n == 1) %>%
      dplyr::left_join(as.data.frame(buff_intersect) %>% dplyr::select(to, rivID), by = c("to")) %>%
      dplyr::select(complexID, rivID)
    # Find closest rivers to new points
    modify_rivers <- integer(length = nrow(complex_nodes))
    for(confluence in new_nodes$complexID){
      # Gather participating rivers
      candidates <- buff_intersect %>%
        dplyr::filter(complexID == confluence)
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
      new_river1 <- sf::st_sfc(sf::st_linestring(matrix(c(start_x,
                                               point_x,
                                               start_y,
                                               point_y), 2)), crs = sf::st_crs(rivers)) %>%
        sf::st_sf() %>%
        dplyr::mutate(rivID = new_nodes$rivID[i])
      # Create second line segment from new confluence to the end of original
      new_river2 <- sf::st_geometry(old_river)
      new_river2[[1]][1] <- point_x
      new_river2[[1]][num_coord/2 + 1] <- point_y
      new_river2 <- sf::st_sf(new_river2) %>%
        dplyr::mutate(rivID = nrow(rivers) + 1) %>%
        dplyr::rename(geometry = new_river2)
      new_river2$rivID[1] <- nrow(rivers) + 1
      # Remove old river and add new ones
      rivers <- rivers %>%
        dplyr::filter(!(rivID == new_nodes$rivID[i])) %>%
        dplyr::bind_rows(new_river1, new_river2)
    }
    # Return modified rivers
    invisible(rivers)
  }
}
