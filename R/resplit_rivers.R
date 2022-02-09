resplit_rivers <- function(river_net){

  # Extract nodes from river network
  nodes <- sf::st_as_sf(sfnetworks::activate(river_net, nodes))
  nodes <- sf::st_combine(nodes)

  # Split rivers according to node locations
  rivers <- sf::st_as_sf(sfnetworks::activate(river_net, edges))
  rivers_prep <- sf::st_collection_extract(lwgeom::st_split(rivers, nodes), "LINESTRING")

  invisible(rivers_prep)

}
