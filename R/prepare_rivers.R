prepare_rivers <- function(rivers, weighting){

  # Remove Z/M dimension
  rivers <- sf::st_zm(rivers)

  # Convert to linestring geometries only
  rivers <- st_cast(rivers, "LINESTRING")

  # Build initial network
  river_net <- sfnetworks::as_sfnetwork(rivers)

  # Split rivers at node locations
  nodes <- river_net %>% sfnetworks::activate(nodes) %>% sf::st_as_sf()
  nodes <- sf::st_combine(nodes)
  rivers_prep <- sf::st_collection_extract(lwgeom::st_split(rivers, nodes), "LINESTRING")

  # Re-incorporate weighting if specified
  if(!is.null(weighting)){
    # To be completed
  }

  invisible(rivers_prep)

}
