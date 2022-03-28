# Constructor function
rivnet <- function(rivers,
                   barriers,
                   sinks = NULL,
                   riv_weight = NULL,
                   others = NULL,
                   snap_tolerance = 10,
                   correct_topology = TRUE){

  # Match river projection
  barriers <- sf::st_transform(barriers, sf::st_crs(rivers))
  sinks <- sf::st_transform(sinks, sf::st_crs(rivers))
  if(!is.null(others)){
    others <- sf::st_transform(others, sf::st_crs(rivers))
  }

  # Combine nodes together
  user_nodes <- dplyr::bind_rows(barriers, sinks, others)

  # Clean up topology if requested
  if(correct_topology == TRUE){
    # Perform necessary corrections
    rivers <- enforce_dendritic(rivers)
  }

  # Split rivers at user node locations
  rivers <- split_rivers_at_points(rivers, user_nodes, snap_tolerance) %>%
    dplyr::mutate(rivID = 1:dplyr::n())

  # Create final sfnetwork
  suppressWarnings(
    rivnet <- sfnetworks::as_sfnetwork(rivers)
  )

  # Join special node attributes
  rivnet <- join_attributes(rivnet, user_nodes, snap_tolerance)

  # Apply binary labeling
  rivnet <- node_labeling(rivnet)

  # Apply membership labelling
  rivnet <- membership_labeling(rivnet)

  # Define rivnet class
  rivnet <- structure(rivnet, class = c("rivnet", class(rivnet)))
  invisible(rivnet)

}

# Simplified constructor
# For internal use only
new_rivnet <- function(rivers,
                       barriers,
                       sinks = NULL,
                       riv_weight = NULL,
                       others = NULL,
                       snap_tolerance = 10,
                       correct_topology = TRUE){

  # Match river projection
  barriers <- sf::st_transform(barriers, sf::st_crs(rivers))
  sinks <- sf::st_transform(sinks, sf::st_crs(rivers))
  if(!is.null(others)){
    others <- sf::st_transform(others, sf::st_crs(rivers))
  }

  # Combine nodes together
  user_nodes <- dplyr::bind_rows(barriers, sinks, others)

  # Clean up topology if requested
  if(correct_topology == TRUE){
    # Perform necessary corrections
    rivers <- enforce_dendritic(rivers)
  }

  # Split rivers at user node locations
  rivers <- split_rivers_at_points(rivers, user_nodes, snap_tolerance) %>%
    dplyr::mutate(rivID = 1:dplyr::n())

  # Create final sfnetwork
  suppressWarnings(
    rivnet <- sfnetworks::as_sfnetwork(rivers)
  )

  # Join special node attributes
  rivnet <- join_attributes(rivnet, user_nodes, snap_tolerance)

  # Apply binary labeling
  rivnet <- node_labeling(rivnet)

  # Apply membership labelling
  rivnet <- membership_labeling(rivnet)

  # Define rivnet class
  rivnet <- structure(rivnet, class = c("rivnet", class(rivnet)))
  invisible(rivnet)

}
