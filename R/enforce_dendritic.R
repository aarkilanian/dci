enforce_dendritic <- function(river_net){

  # Correct divergences
  net_temp <- correct_divergences(river_net)

  # Correct complex confluences
  net_final <- correct_complex(net_temp)
  return(net_final)

}

correct_divergences <- function(river_net){

  # Automatically correct divergent pairs (longest length kept)
  riv_corrected <- river_net %>%
    sfnetworks::activate(edges) %>%
    sf::st_as_sf() %>%
    dplyr::group_by(from) %>%
    dplyr::filter(weight == max(weight))

  # If no rivers are corrected return unchanged network
  num_div <- nrow(river_net %>% sfnetworks::activate(edges) %>% tibble::as_tibble()) - nrow(riv_corrected)
  if(num_div == 0){
    message("No divergences detected.")
    invisible(river_net)
  } else {
    message(paste0(num_div, " divergences corrected."))
    new_net <- sfnetworks::as_sfnetwork(riv_corrected, length_as_weight = TRUE)
    invisible(new_net)
  }

}

correct_complex <- function(river_net){

  # Set buffer distance
  buff <- 0.25

  # Generate unique river IDs
  river_net <- river_net %>%
    sfnetworks::activate(edges) %>%
    dplyr::mutate(rivID = 1:dplyr::n())

  # Identify complex confluences
  complex_nodes <- river_net %>%
    tidygraph::convert(tidygraph::to_undirected) %>%
    sfnetworks::activate(nodes) %>%
    dplyr::mutate(nodeID = dplyr::n()) %>%
    dplyr::mutate(degree = tidygraph::centrality_degree()) %>%
    sf::st_as_sf() %>%
    dplyr::filter(degree >= 4) %>%
    dplyr::select(degree) %>%
    dplyr::mutate(complexID = dplyr::n())

  # If confluences have over 4 inputs recommend manual correction
  if(any(complex_nodes$degree > 4)){
    stop("Complex confluences with over 3 input tributaries have been detected. Use the standalone `enforce_dendritic()` and correct returned errors manually.")
  }

  # If no errors return unchanged network
  if(length(complex_nodes$degree) == 0){
    message("No complex confluences found.")
    invisible(river_net)
  } else {

    # Create small buffer around confluence point
    comp_buffer <- sf::st_buffer(complex_nodes, dist = buff)

    # Create points at intersection of rivers and buffers
    rivers <- river_net %>%
      sfnetworks::activate(edges) %>%
      sf::st_as_sf()
    sf::st_agr(rivers) <- "constant"
    buffer <- sf::st_cast(comp_buffer, "MULTILINESTRING", group_or_split = FALSE)
    sf::st_agr(buffer) <- "constant"
    buff_intersect <- sf::st_intersection(rivers, buffer)%>%
      dplyr::select(rivID, complexID, to)

    # Select points out of buffer intersections for new confluence nodes on downstream edges
    new_nodes <- buff_intersect %>%
      dplyr::group_by(complexID, to) %>%
      dplyr::tally() %>%
      dplyr::filter(n == 1) %>%
      dplyr::select(complexID)

    # Find closest rivers to new points
    modify_rivers <- integer(length = nrow(complex_nodes))
    for(confluence in new_nodes$complexID){

      # Gather participating rivers
      candidates <- buff_intersect %>%
        dplyr::filter(complexID == confluence)

      # Determine closest river to new confluence
      distances <- sf::st_distance(new_nodes[confluence,], candidates)
      ind <- which(distances == min(distances[distances > 0]))
      modify_rivers[confluence] <- candidates$rivID[ind]
    }

    # Move endpoints of closest river lines
    for(i in 1:length(modify_rivers)){

      # Get river geometry
      old_river <- sf::st_geometry(rivers[which(rivers$rivID == modify_rivers[i]),])
      # Get point geometry
      new_point <- sf::st_geometry(new_nodes[i,])
      # Get number of coordinates on river line
      num_coord <- length(old_geometry[[1]])
      # Update final coordinates with new point
      new_river <- old_river
      new_river[[1]][num_coord - num_coord/2] <- new_point[[1]][1]
      new_river[[1]][num_coord] <- new_point[[1]][2]
      # Replace old geometry in rivers
      sf::st_geometry(rivers[which(rivers$rivID == modify_rivers[i]),]) <- sf::st_sfc(new_river)

    }

    # Return modified river network
    new_net <- sfnetworks::sfnetwork(rivers, length_as_weight = TRUE)
    invisible(new_net)

  }

}
