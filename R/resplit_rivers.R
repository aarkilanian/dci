resplit_rivers <- function(river_net){

  # Extract nodes from river network
  nodes <- sf::st_as_sf(sfnetworks::activate(river_net, nodes))
  nodes <- sf::st_combine(nodes)

  # Split rivers according to node locations
  rivers <- sf::st_as_sf(sfnetworks::activate(river_net, edges))
  rivers_prep <- sf::st_collection_extract(lwgeom::st_split(rivers, nodes), "LINESTRING") %>%
    dplyr::select(-c(from,to)) %>%
    dplyr::mutate(rivID = 1:dplyr::n()) %>%
    sf::st_as_sf()

  ######################################################################
  # Need to have a method here to deal with weighting on a split segment
  ######################################################################

  # If no modifications are required, return the original network
  if(nrow(rivers_prep) == nrow(rivers)){
    invisible(river_net)
    # Build and return sfnetwork object
  } else {
    nodes <- sf::st_cast(nodes, "POINT")
    river_net <- sfnetworks::as_sfnetwork(rivers_prep)
    invisible(river_net)
  }
}
