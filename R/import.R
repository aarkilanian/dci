#' Prepare rivers for \code{\link{river_net}} connectivity analyses
#'
#'Read and prepare geospatial river lines data for dci.
#'
#' @param rivers A character string or \code{\link{sf}} object, the path to a shapefile of river lines or \code{\link{sf}} object of rivers.
#' @param quiet A logical value, if \code{FALSE} edited rivers are plotted with the original. Defaults to \code{FALSE}.
#'
#' @return Object of class rivers prepared for input to \code{\link{river_net}}
#'
#' @export

import_rivers <- function(rivers, quiet = FALSE){
  # Check for path type
  if(is.character(rivers)) sf <- FALSE
  else sf <- TRUE
  # Read shapefile from path if not sf object
  if(!sf){
    # Read in river with sf
    rivers <- tryCatch(sf::read_sf(rivers),
      error = function(e) rlang::abort("invalid spatial data provided")
    )
  } else{
    rivers <- rivers
  }

  # Check that spatial data is lines
  if(!any(sf::st_geometry_type(rivers) %in% c("LINESTRING", "MULTILINESTRING"))){
    stop("Provided data contains geometries other than LINESTRING and MULTILINESTRING")
  }

  # Store original rivers for later plotting
  rivers.old <- sf::st_zm(rivers)

  # Prepare rivers
  # Remove Z/M dimensions
  rivers <- sf::st_zm(rivers)
  # Cast all features to linestring geometries
  rivers <- sf::st_cast(rivers, "LINESTRING")

  # Check for valid and empty geometries
  if(any(!(sf::st_is_valid(rivers))) | any(sf::st_is_empty(rivers))){
    stop("Invalid geometries detected in rivers")
  }

  # Identify components
  net <- sfnetworks::as_sfnetwork(rivers) %>%
    dplyr::mutate(component = tidygraph::group_components()) %>%
    dplyr::group_by(.data$component)
 comps <- activate(net, nodes) %>%
   as.data.frame(.data) %>%
   dplyr::select(.data$component)

  # Determine largest component and extract
  big_comp <- sort(table(comps), decreasing = TRUE)[1]
  big_comp <- as.integer(names(big_comp))
  net <- net %>% dplyr::filter(.data$component == big_comp)
  rivers <- activate(net, edges)

  # Calculate river lengths
  rivers$riv_length <- as.double(sf::st_length(rivers))

  # Plot rivers if quiet is set to FALSE
  if(quiet == FALSE){
    plot(sf::st_geometry(rivers.old), col = "red")
    plot(sf::st_geometry(rivers), add = T, lwd = 2)
  }

  # Return rivers
  rivers <- structure(rivers, class = c("rivers", class(rivers)))
  invisible(rivers)
}

#' Prepare point data for \code{\link{river_net}} connectivity analyses
#'
#'Read and prepare geospatial point data for dci.
#'
#' @param pts A character string or \code{\link{sf}} object, the path to a shapefile of points or \code{\link{sf}} object of points.
#' @param type A character string, either of “barrier”, “sink”, or “poi” specifying the type of point.
#'
#' @return Object of class barriers, sink, or poi prepared for input to \code{\link{river_net}}
#'
#' @export
import_points <- function(pts, type){

  # Check that type is valid
  if(!(type %in% c("barriers", "sink", "poi"))) stop("Points must be of 'barriers', 'sink', or 'poi' type.")

  # Check for path type
  if(is.character(pts)) sf <- FALSE
  else sf <- TRUE
  # Read shapefile from path if not sf object
  if(!sf){
    # Read in river with sf
    pts <- tryCatch(sf::read_sf(pts),
                       error = function(e) rlang::abort("invalid spatial data provided")
    )
  } else{
    pts <- pts
  }
  # Check for valid and empty geometries
  if(any(!(sf::st_is_valid(pts))) | any(sf::st_is_empty(pts))){
    stop("Invalid geometries detected in points")
  }

  # Check that id is valid
  if(!is.null(id)){
    user_id <- tryCatch(
      as.character(pts[[id]]),
      error = function(e) {
        stop("Cannot convert id values to character strings: ", e, call. = FALSE)
      }
    )
  }

  # Barriers
  if(type == "barriers"){
    # Prepare barriers
    barriers <- pts %>%
      # Remove Z/M dimension
      sf::st_zm() %>%
      # Assign barrier type
      dplyr::mutate(type = "Barrier")

    # Add user provided ID
    if(!is.null(id)){
      barriers$user_id <- user_id
    }

    # Return barriers
    barriers <- structure(barriers, class = c("barriers", class(barriers)))
    return(barriers)
  }

  # Sink
  if(type == "sink"){

    # Check that there is only 1 point
    if(nrow(pts) == 1) stop("Multiple points found. The sink must be a single point.")

    # Prepare sinks
    sink <- pts %>%
      # Remove Z/M dimensions
      sf::st_zm() %>%
      # Assign sink type
      dplyr::mutate(type = "Sink")

    # Add user provided ID
    if(!is.null(id)){
      sinks$user_id <- user_id
    }

    # Return sinks
    sink <- structure(sinks, class = c("sink", class(sinks)))
    return(sink)

  }
  # Points of interest
  if(type == "poi"){
    # Prepare other points
    poi <- pts %>%
      # Remove Z/M dimensions
      sf::st_zm() %>%
      # Assign other type
      dplyr::mutate(type = "poi")

    # Add user provided ID
    if(!is.null(id)){
      poi$user_id <- user_id
    }

    # Return others
    poi <- structure(poi, class = c("poi", class(poi)))
    return(poi)
  }
}
