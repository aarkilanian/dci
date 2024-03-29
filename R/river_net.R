#' Create a \code{river_net} object
#'
#' \code{river_net} is a geospatial network structure of river data. It extends the tidy representation of geospatial network data from the \code{\link[sfnetworks]{sfnetwork}} data structure.
#'
#' @param rivers A \code{rivers} object returned by \code{\link{import_rivers}}.
#' @param barriers A \code{barriers} object returned by \code{\link{import_points}}.
#' @param outlet A \code{outlet} object returned by \code{\link{import_points}}.
#' @param poi A \code{poi} object (points of interest) returned by \code{\link{import_points}}. This data is optional.
#' @param invasions A \code{invasions} object returned by \code{\link{import_points}}. This data is optional
#' @param check A logical value, if \code{TRUE}, the default, dendritic topology of the river network is enforced with \code{\link{enforce_dendritic}}.
#' @param tolerance A numeric value specifying the distance in map units that points should be snapped to rivers. Set to NULL by default.
#'
#' @return An object of class \code{\link{river_net}} representing the river network formed by the geospatial lines and points provided.
#'
#' @export
#'
#' @examples
#' \dontrun{ river_net(rivers = river_data, barriers = barrier_data, outlet = outlet_data)}
#' \dontrun{ river_net(river_data, barrier_data, outlet_data, poi = extra_data)}
#' \dontrun{ river_net(river_data, barrier_data, outlet_data, tolerance = 15}
river_net <- function(rivers,
                   barriers,
                   outlet,
                   poi = NULL,
                   invasions = NULL,
                   check = TRUE,
                   tolerance = NULL){

  # Check rivers
  if(!("rivers" %in% class(rivers))){
    stop("Rivers must first be imported with `import_rivers`")
  }

  # Check barriers
  if(!("barriers" %in% class(barriers))){
    stop("Barriers must first be imported with `import_points`")
  }
  if(sf::st_crs(barriers) != sf::st_crs(rivers)){
    stop("CRS of barriers does not match rivers")
  }

  # Check sinks
  if(!("outlet" %in% class(outlet))){
    stop("outlet must first be imported with `import_points`")
  }
  if(sf::st_crs(outlet) != sf::st_crs(rivers)){
    stop("CRS of outlet does not match rivers")
  }

  # Check points of interest
  if(!(is.null(poi))){
    if(!("poi" %in% class(poi))){
      stop("Points of interest must first be imported with `import_points`")
    }
    if(sf::st_crs(poi) != sf::st_crs(rivers)){
      stop("CRS of points of interest does not match rivers")
    }
  }

  # Check invasions
  if(!(is.null(invasions))){
    if(!("invasions" %in% class(invasions))){
      stop("Invasions must first be imported with `import_points`")
    }
    if(sf::st_crs(invasions) != sf::st_crs(rivers)){
      stop("CRS of invasions does not match rivers")
    }
  }

  # Combine nodes
  if(!is.null(poi)){
    user_nodes <- dplyr::bind_rows(barriers, outlet, poi)
  } else{
    user_nodes <- dplyr::bind_rows(barriers, outlet)
  }

  # Ensure user_nodes has geometry column named "geometry"
  if(!("geometry" %in% colnames(user_nodes))){
    user_nodes <- rename_geometry(user_nodes, "geometry")
  }

  # Clean up topology if requested
  if(check == TRUE){
    # Perform necessary corrections
    rivers <- enforce_dendritic(rivers, correct = TRUE)
  }

  # Split rivers at user node locations
  rivers <- split_rivers_at_points(rivers, user_nodes, tolerance) %>%
    dplyr::mutate(rivID = 1:dplyr::n())

  # Ensure rivers has geometry column named "geometry"
  if(!("geometry" %in% colnames(rivers))){
    rivers <- rename_geometry(rivers, "geometry")
  }

  # Create final sfnetwork
  suppressWarnings(
    net <- sfnetworks::as_sfnetwork(rivers)
  )

  # Join special node attributes
  net <- join_attributes(net, user_nodes, tolerance)

  # Apply binary labeling
  net <- node_labeling(net)

  # Apply membership labelling
  net <- membership_labeling(net)

  # If provided, combine invasions to river attributes
  if(!(is.null(invasions))){
    net <- join_invasions(net, invasions)
  }

  # Define river_net class
  net <- structure(net, class = c("river_net", class(net)))
  invisible(net)

}
