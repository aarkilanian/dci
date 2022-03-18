split_rivers_at_points <- function(rivers, pts, tolerance){

  # Remove sinks if present
  pts <- pts %>%
    dplyr::filter(type != "Sink")

  # Find nearest river features
  riv_ind <- sf::st_nearest_feature(pts, rivers)

  for(i in 1:length(riv_ind)){

    # Update nearest river features
    riv_ind[i] <- sf::st_nearest_feature(pts[i,], rivers)

    # Place points on rivers
    riv_pts <- sf::st_line_sample(rivers[riv_ind[i],], density = 1/1) %>%
      sf::st_sf() %>%
      sf::st_cast("POINT") %>%
      dplyr::mutate(group = 1)

    # Find nearest point
    distances <- sf::st_distance(pts[i,], riv_pts)
    nrst_ind <- which.min(distances)
    nrst <- riv_pts[nrst_ind,]

    # Skip if distance is above threshold
    if(!is.null(tolerance)){
      min_dist <- distances[nrst_ind]
      if(as.double(min_dist) > tolerance){
        if(!riv_ind_dup[i]){
        dplyr::bind_rows(split_rivs, old_rivs[i,])
        }
        next()
      }
    }

    # Create first segment
    riv_start <- sf::st_geometry(rivers[riv_ind[i],])
    riv_len <- length(riv_start[[1]])
    riv_start <- sf::st_sfc(sf::st_point(c(riv_start[[1]][1], riv_start[[1]][riv_len/2 + 1])), crs = sf::st_crs(rivers))
    sf::st_geometry(riv_pts[1,]) <- sf::st_geometry(riv_start)
    river1 <- riv_pts[1:nrst_ind,] %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(do_union = FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::ungroup()

    # Create second segment
    riv_end <- sf::st_geometry(rivers[riv_ind[i],])
    riv_end <- sf::st_sfc(sf::st_point(c(riv_end[[1]][riv_len/2], riv_end[[1]][riv_len])), crs = sf::st_crs(rivers))
    sf::st_geometry(riv_pts[nrow(riv_pts),]) <- sf::st_geometry(riv_end)
    river2 <- riv_pts[nrst_ind:nrow(riv_pts),] %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(do_union = FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::ungroup()

    # Remove old river
    rivers <- rivers[-riv_ind[i],]

    # Add new rivers
    rivers <- rivers %>%
      dplyr::bind_rows(river1, river2)

  }

  invisible(rivers)

}


join_attributes <- function(rivnet, nodes, tolerance){

  # Determine user nodes
  within_dist <- nodes %>%
    sf::st_is_within_distance(rivnet %>% sfnetworks::activate(nodes), dist = 10, sparse = T) %>%
    unlist()
  nodes$key <- within_dist

  # Join special nodes' attributes to network nodes
  rivnet <- rivnet %>%
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
  invisible(rivnet)
}
