#' Split river lines around point locations
#'
#' @inheritParams river_net
#'
#' @param pts An \code{\link[sf]{sf}} object, points at which to split river lines.
#'
#' @param tolerance An integer value, the maximum distance in map units from rivers to given points. Set to NULL by default.
#'
#' @noRd
split_rivers_at_points <- function(rivers, pts, tolerance = NULL){

  # Remove sinks if present
  if("Sink" %in% pts$type){
  pts <- pts %>%
    dplyr::filter(type != "Sink")
}

  for(i in 1:nrow(pts)){

    # Update nearest river features
    riv_distances <- sf::st_distance(rivers, pts[i,])
    riv_ind <- which.min(riv_distances)

    # Skip if distance is above threshold
    if(!is.null(tolerance)){
      min_dist <- riv_distances[riv_ind]
      if(as.double(min_dist) > tolerance){
        next()
      }
    }

    # Place points on rivers
    riv_pts <- sf::st_line_sample(rivers[riv_ind,], density = 1/1) %>%
      sf::st_sf() %>%
      sf::st_cast("POINT") %>%
      dplyr::mutate(group = 1)

    # Find nearest point
    nrst_ind <- which.min(sf::st_distance(pts[i,], riv_pts))

    # Create first segment
    riv_start <- sf::st_geometry(rivers[riv_ind,])
    riv_len <- length(riv_start[[1]])
    # Extract river start point
    riv_start <- sf::st_sfc(sf::st_point(c(riv_start[[1]][1], riv_start[[1]][riv_len/2 + 1])), crs = sf::st_crs(rivers))
    # Move first point on river line to start location
    sf::st_geometry(riv_pts[1,]) <- sf::st_geometry(riv_start)
    # Convert river points to line
    river1 <- riv_pts[1:nrst_ind,] %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(do_union = FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::ungroup()

    # Create second segment
    riv_end <- sf::st_geometry(rivers[riv_ind,])
    riv_end <- sf::st_sfc(sf::st_point(c(riv_end[[1]][riv_len/2], riv_end[[1]][riv_len])), crs = sf::st_crs(rivers))
    sf::st_geometry(riv_pts[nrow(riv_pts),]) <- sf::st_geometry(riv_end)
    river2 <- riv_pts[nrst_ind:nrow(riv_pts),] %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(do_union = FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::ungroup()

    # Add new rivers
    rivers <- rivers %>%
      dplyr::bind_rows(river1, river2)

    # Remove old river
    rivers <- rivers[-riv_ind,]

  }

  invisible(rivers)

}

#' Join variables from network nodes to \code{\link{river_net}} object.
#'
#' @param net A \code{\link{river_net}} object.
#'
#' @param nodes An \code{\link[sf]{sf}} object of binded "barrier", "sink", or "other" points.
#'
#' @inheritParams split_rivers_at_points
#'
#' @noRd
join_attributes <- function(net, nodes, tolerance = NULL){

  # Find nearest river network node
  nrst <- nodes %>%
    sf::st_nearest_feature(net %>% sfnetworks::activate(nodes) %>% sf::st_as_sf())
  nodes$key <- nrst

  if(!is.null(tolerance)){

    # Get distance to nearest node
    net_nodes <- net %>% sfnetworks::activate(nodes) %>% sf::st_as_sf()
    nrst_dist <- sf::st_distance(nodes, net_nodes[nrst,])
    nrst_dist <- diag(nrst_dist)
    within_tolerance <- nrst_dist <= tolerance
    nodes <- nodes[within_tolerance,]

  } else nodes <- nodes

  # Join special nodes' attributes to network nodes
  net <- net %>%
    sfnetworks::activate(nodes) %>%
    dplyr::mutate(rowID = dplyr::row_number()) %>%
    dplyr::left_join(as.data.frame(nodes) %>% dplyr::select(-geometry), by = c("rowID" = "key")) %>%
    dplyr::select(-rowID) %>%
    # Set node type of topological nodes
    dplyr::mutate(type = dplyr::if_else(is.na(type), "Topo", type)) %>%
    # Set topological node permeability
    dplyr::mutate(perm = dplyr::if_else(is.na(perm), 1, perm)) %>%
    # Add unique node IDs
    dplyr::mutate(nodeID = dplyr::row_number())

  # Return joined network
  invisible(net)
}
