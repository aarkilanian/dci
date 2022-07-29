#' Split river lines around point locations
#'
#' @inheritParams river_net
#'
#' @param pts An \code{\link[sf]{sf}} object, points at which to split river lines.
#' @param force_nodes A logical value indicating whether points which are too close to confluences should be ignored or not
#'
#' @return A \code{\link{rivers}} object with non-split rivers replaced with two new features each at opposite sides of the node which splits it. All attributes assumed to be constant.
#'
#' @keywords internal
split_rivers_at_points <- function(rivers, pts, tolerance = NULL){

  # Remove sinks if present
  if("outlet" %in% pts$type){
    pts <- pts[pts$type != "outlet",]
  }

  for(i in 1:(nrow(pts))){

    # Update nearest river features
    riv_distances <- sf::st_distance(rivers, pts[i,])

    # If node is too close to line ends issue error
    if(sum(as.vector(riv_distances) <= 0.1) >= 2){
        warning(paste0("Node ", i, " too close to river ends"))
        next()
    }

    # Identify closest river
    riv_ind <- which.min(riv_distances)

    # Skip if distance is above threshold
    if(!is.null(tolerance)){
      min_dist <- riv_distances[riv_ind]
      if(as.double(min_dist) > tolerance) next()
    }

    # Place points on rivers
    riv_pts <- sf::st_sf(sf::st_line_sample(rivers[riv_ind,], density = 1/1))
    riv_pts <- tryCatch({
      sf::st_cast(riv_pts, "POINT") %>%
        dplyr::mutate(group = 1)
    }, warning = function(w) {
      print(paste('warning:', w))
    })
    # If river only has 2 points skip
    if(nrow(riv_pts) == 2){
      warning("River too short to perform splitting.")
      next()
    }

    # Find nearest point (except start and end)
    nrst_ind <- which.min(sf::st_distance(pts[i,], riv_pts))
    # If nearest point is start of line move to 2nd point
    if(nrst_ind == 1) nrst_ind <- 2
    # If nearest point is at end of line move back one point
    if(nrst_ind == nrow(riv_pts)) nrst_ind <- nrst_ind - 1

    # Create first segment
    riv_start <- sf::st_geometry(rivers[riv_ind,])
    riv_len <- length(riv_start[[1]])
    # Extract river start point
    riv_start <- sf::st_sfc(sf::st_point(c(riv_start[[1]][1], riv_start[[1]][riv_len/2 + 1])), crs = sf::st_crs(rivers))
    # Move first point on river line to start location
    sf::st_geometry(riv_pts[1,]) <- sf::st_geometry(riv_start)
    # Convert river points to line
    river1_geom <- riv_pts[1:nrst_ind,] %>%
      sf::st_coordinates(.data) %>%
      sf::st_linestring(.data)%>%
      sf::st_geometry(.data)
    river1 <- rivers[riv_ind,]
    sf::st_crs(river1_geom) <- sf::st_crs(rivers)
    sf::st_geometry(river1) <- river1_geom
    river1$riv_length <- as.double(sf::st_length(river1))
    river1 <- sf::st_sf(river1, crs = sf::st_crs(rivers))

    # Create second segment
    # If point is close to end of river line
    if(nrow(riv_pts) == nrst_ind){
      line_start <- sf::st_geometry(riv_pts[nrst_ind,])
      line_end <- sf::st_geometry(rivers[riv_ind,])
      line_end <- sf::st_sfc(sf::st_point(c(line_end[[1]][riv_len/2], line_end[[1]][riv_len])), crs = sf::st_crs(rivers))
      river2_geom <- sf::st_sfc(sf::st_linestring(matrix(c(line_start[[1]][1], line_end[[1]][1], line_start[[1]][2], line_end[[1]][2]), ncol = 2)))
      river2 <- rivers[riv_ind,]
      sf::st_crs(river2_geom) <- sf::st_crs(rivers)
      sf::st_geometry(river2) <- river2_geom
      river2$riv_length <- as.double(sf::st_length(river2))
    # Otherwise
    } else {
      riv_end <- sf::st_geometry(rivers[riv_ind,])
      riv_end <- sf::st_sfc(sf::st_point(c(riv_end[[1]][riv_len/2], riv_end[[1]][riv_len])), crs = sf::st_crs(rivers))
      sf::st_geometry(riv_pts[nrow(riv_pts),]) <- sf::st_geometry(riv_end)
      river2_geom <- riv_pts[nrst_ind:nrow(riv_pts),]%>%
        sf::st_coordinates(.data) %>%
        sf::st_linestring(.data) %>%
        sf::st_geometry(.data)
      river2 <- rivers[riv_ind,]
      sf::st_crs(river2_geom) <- sf::st_crs(rivers)
      sf::st_geometry(river2) <- river2_geom
      river2$riv_length <- as.double(sf::st_length(river2))
    }
    # Add new rivers
    rivers <- rbind(rivers, river1, river2)
    # Remove old river
    rivers <- rivers[-riv_ind,]
  }
  invisible(rivers)
}

#' Join variables from network nodes to \code{\link{river_net}} object.
#'
#' @inheritParams river_net
#'
#' @param nodes An \code{\link[sf]{sf}} object of "barrier", "outlet", and "poi" points.
#'
#' @return A \code{\link{river_net}} object with the supplied nodes' attributes joined to its nodes.
#'
#' @keywords internal
join_attributes <- function(net, nodes, tolerance = NULL){

  # Extract network nodes
  net_nodes <- sf::st_as_sf(activate(net, nodes))

  # Find nearest river network node
  nrst <-  sf::st_nearest_feature(nodes, net_nodes)
  nodes$key <- nrst

  # If a tolerance is specified, avoid joining nodes outside that tolerance
  if(!is.null(tolerance)){
    # Get distance to nearest node
    nrst_dist <- sf::st_distance(nodes, net_nodes[nrst,])
    nrst_dist <- diag(nrst_dist)
    # Identify nodes outside tolerance
    within_tolerance <- as.double(nrst_dist) <= tolerance
    nodes <- nodes[within_tolerance,]
  # If no tolerance use all nodes
  } else nodes <- nodes

  # Join special nodes' attributes to network nodes
  net <- activate(net, nodes) %>%
    dplyr::mutate(rowID = dplyr::row_number()) %>%
    dplyr::left_join(as.data.frame(nodes) %>% dplyr::select(-.data$geometry), by = c("rowID" = "key")) %>%
    dplyr::select(-.data$rowID) %>%
    # Set node type of topological nodes
    dplyr::mutate(type = dplyr::if_else(is.na(.data$type), "topo", .data$type))

  # Return joined network
  invisible(net)
}
