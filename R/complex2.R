correct_complex <- function(net, correct = TRUE, quiet = FALSE) {

  # Identify complex confluences
  net_undirected <- activate(tidygraph::convert(net, tidygraph::to_undirected), nodes)
  net_degree <- net_undirected %>%
    dplyr::mutate(nodeID = 1:dplyr::n()) %>%
    dplyr::mutate(degree = tidygraph::centrality_degree())
  complex_nodes <- sf::st_as_sf(net_degree) %>%
    dplyr::filter(.data$degree >= 4) %>%
    dplyr::select("degree")

  # If confluences have over 4 inputs recommend manual correction
  if (any(complex_nodes$degree > 4)) {
    if (correct) stop("Complex confluences with over 3 input tributaries have been detected. Use the standalone `enforce_dendritic()` and correct returned errors manually.")
  }

  # If no errors return unchanged rivers
  if (length(complex_nodes$degree) == 0) {
    if (!quiet) message("No complex confluences found.")
    invisible(list(sf::st_as_sf(activate(net, edges)), 0))
  }

  # Correct complex confluences detected
} else {

  # Print number of complex confluences found
  num_complex <- nrow(complex_nodes)
  if (!quiet) message(num_complex, " complex confluences found.")

  # Extract network rivers
  rivers <- sf::st_as_sf(activate(net, edges)) %>%
    dplyr::mutate(rivID = 1:dplyr::n())
  sf::st_agr(rivers) <- "constant"

  # Add ID to complex nodes
  complex_nodes <- complex_nodes %>%
    dplyr::mutate(complexID = 1:dplyr::n())

  # Create small buffer around confluence points
  buffer <- sf::st_buffer(complex_nodes, dist = 1)
  buffer <- sf::st_cast(buffer, "MULTILINESTRING", group_or_split = FALSE)
  sf::st_agr(buffer) <- "constant"

  # If manual editing desired, identify complex confluence rivers
  if (!correct) {
    complex_riv <- sf::st_join(rivers, buffer, left = TRUE)
    return(complex_riv)
  }

  # Create points at intersection of rivers and buffer
  buff_intersect <- sf::st_intersection(rivers, buffer)
  buff_intersect <- buff_intersect[c("rivID", "complexID", "to")]
  num_intersections <- buff_intersect %>%
    group_by(.data$complexID) %>%
    dplyr::count() %>%
    dplyr::pull(.data$n)

  # If any buffer captures more than 4 rivers exit
  if(any(num_intersections > 4)) stop("Rivers are too close together to automatically correct complex confluences.")

  # For each complex confluence
  for(complex in complex_nodes){

    # Extract ID
    curID <- complex$complexID

    # Identify buffer intersections
    cur_int <- buff_intersect %>%
      dplyr::filter(.data$complexID == curID)

    # Identify new confluence point
    new_conf <- cur_int[1,]

    # Identify river associated to confluence
    riv_to_split <- cur_int[1,]$rivID

    # Calculate bearing
    main_bear <- get_bearing(complex, new_conf)

    # Calculate bearing of 3 others
    bear1 <- get_bearing(complex, cur_int[2,])
    bear2 <- get_bearing(complex, cur_int[3,])
    bear3 <- get_bearing(complex, cur_int[4,])

    # Compare bearing and pick perpendicular (~90, not 0 or 180 that's parallel)
    angle_diff <- abs(atan2(sin(c(bear1, bear2, bear3), main_bear) -
                              cos(c(bear1, bear2, bear3), main_bear)))
    # pick closest to 90 or -90, farthest from 0 or 180
    best_angle <- which(angle_diff)

    # Move endpoint of chosen line to new confluence point

    # Split old river at new confluence into 2 rivers

    # Replace old river with new rivers

  }

  # Return new river network
}


}

get_bearing <- function(p1, p2){

  # Get point coordinated
  coord_1 <- sf::st_coordinates(p1)
  coord_2 <- sf::st_coordinates(p2)

  # Calculate change in x and y
  dx <- coord_2[1]- coord_1[1]
  dy <- coord_2[2]- coord_1[2]

  # Calculate and return bearing
  bearing <- atan2(dx, dy)
  return(bearing)

}
