prepare_rivers <- function(rivers){

  # Remove Z/M dimension
  rivers <- sf::st_zm(rivers)
  # Convert to linestring geometries only
  rivers <- sf::st_cast(rivers, "LINESTRING")

  # Extract nodes at ends of lines and confluences
  nodes <- extract_nodes(rivers)
  # Split rivers according to node locations
  rivers_prep <- sf::st_collection_extract(lwgeom::st_split(rivers, nodes), "LINESTRING")

  invisible(rivers_prep)

}

extract_nodes <- function(rivers){

  # Create sfnetwork object
  river_net <- sfnetworks::as_sfnetwork(rivers)

  # Extract nodes
  nodes <- sf::st_as_sf(sfnetworks::activate(river_net, nodes))
  nodes <- sf::st_combine(nodes)

  # Return nodes
  invisible(nodes)

}
