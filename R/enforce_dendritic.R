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

  # If no buffers return unchanged network
  if(nrow(complex_nodes) == 0){
    message("No complex confluences found.")
    invisible(river_net)
  } else {

    # Create small buffer around confluence point
    comp_buffer <- sf::st_buffer(complex_nodes, dist = 0.25)

    # Create points at intersection of rivers and buffer and select node on downstream edge
    new_points <- sf::st_intersection(river_net %>% sfnetworks::activate(edges) %>% sf::st_as_sf(), sf::st_cast(comp_buffer, "MULTILINESTRING", group_or_split = FALSE)) %>%
      dplyr::group_by(complexID, to) %>%
      dplyr::tally() %>%
      dplyr::filter(n == 1)

    # Move end vertex of one complex edge to new point location

    # Create and return repaired network

  }

}

find_rare <- function(x, y){

  counts <- table(buffer_inter[y])
  return(as.numeric(names(counts[counts == min(counts)])))

}
