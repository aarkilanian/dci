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
}



  # Check for divergences
  if(nrow(div_riv) == 0){
    # no divergence or complex confluences
    if(nrow(complex_nodes) == 0){
      message("No divergences or complex confluences found.")
      # Return unchanged rivers
      # To be completed based on format of output

      # no divergences but some complex confluences
    } else{
      message()

    }
  } else {
    # no complex confluences but some divergences
    if(nrow(complex_nodes == 0)){
      # all errors present
    } else {

    }
  }


      div_join <- river_net %>%
        sfnetworks::activate(edges) %>%
        data.frame() %>%
        dplyr::left_join(div_riv %>% dplyr::select(c(divID))) %>%
        sf::st_as_sf()
      invisible(div_join)
