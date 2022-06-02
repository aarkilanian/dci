#' Prepare rivers for \code{\link{river_net}} connectivity analyses
#'
#'Read and prepare geospatial river lines data for dci.
#'
#' @param path A character string or \code{\link{sf}} object, the path to a shapefile of river lines or \code{\link{sf}} object of rivers.
#' @param quiet A logical value, if \code{FALSE} edited rivers are plotted with the original. Defaults to \code{FALSE}.
#'
#' @return Object of class rivers prepared for input to \code{\link{river_net}}
#'
#' @export

import_rivers <- function(path, quiet = FALSE){
  # Check for path type
  if(is.character(path)) sf <- FALSE
  else sf <- TRUE
  # Read shapefile from path if not sf object
  if(!sf){
    # Read in river with sf
    rivers <- tryCatch(sf::read_sf(path),
      error = function(e) rlang::abort("invalid spatial data provided")
    )
  } else{
    rivers <- path
  }

  # Check that spatial data is lines
  if(!any(sf::st_geometry_type(rivers) %in% c("LINESTRING", "MULTILINESTRING"))){
    stop("Provided data contains geometries other than LINESTRING and MULTILINESTRING")
  }

  # Store original rivers for later plotting
  rivers.old <- sf::st_zm(rivers)

  # Prepare rivers
  rivers <- rivers %>%
    # Remove Z/M dimensions
    sf::st_zm() %>%
    # Cast all features to linestring geometries
    sf::st_cast("LINESTRING")

  # Check for valid and empty geometries
  if(any(!(sf::st_is_valid(rivers))) | any(sf::st_is_empty(rivers))){
    stop("Invalid geometries detected in rivers")
  }

  # Identify components
  net <- rivers %>%
    sfnetworks::as_sfnetwork() %>%
    dplyr::mutate(component = tidygraph::group_components()) %>%
    dplyr::group_by(component)

  # Determine largest component and extract
  big_comp <- sort(table(net %>% activate(nodes) %>% data.frame() %>% dplyr::select(component)), decreasing = TRUE)[1]
  big_comp <- as.integer(names(big_comp))
  net <- net %>%
    dplyr::filter(component == big_comp)
  rivers <- net %>% activate(edges) %>% sf::st_as_sf()

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
#' @param path A character string or \code{\link{sf}} object, the path to a shapefile of points or \code{\link{sf}} object of points.
#' @param type A character string, either of “barrier”, “sink”, or “poi” specifying the type of point.
#' @param id An optional character vector specifying unique IDs for the points. Defaults to NULL
#'
#' @return Object of class barriers, sinks, or poi prepared for input to \code{\link{river_net}}
#'
#' @export
import_points <- function(path, type, id = NULL){

  # Check that type is valid
  if(!(type %in% c("barriers", "sinks", "poi"))) stop("Points must be of 'barriers', 'sinks', or 'poi' type.")

  # Check for path type
  if(is.character(path)) sf <- FALSE
  else sf <- TRUE
  # Read shapefile from path if not sf object
  if(!sf){
    # Read in river with sf
    points <- tryCatch(sf::read_sf(path),
                       error = function(e) rlang::abort("invalid spatial data provided")
    )
  } else{
    points <- path
  }
  # Check for valid and empty geometries
  if(any(!(sf::st_is_valid(points))) | any(sf::st_is_empty(points))){
    stop("Invalid geometries detected in points")
  }

  # Check that id is valid
  if(!is.null(id)){
    user_id <- tryCatch(
      as.character(points[[id]]),
      error = function(e) {
        stop("Cannot convert id values to character strings: ", e, call. = FALSE)
      }
    )
  }

  # Barriers
  if(type == "barriers"){
    # Prepare barriers
    barriers <- points %>%
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

  # Sinks
  if(type == "sinks"){
    # Prepare sinks
    sinks <- points %>%
      # Remove Z/M dimensions
      sf::st_zm() %>%
      # Assign sink type
      dplyr::mutate(type = "Sink")

    # Add user provided ID
    if(!is.null(id)){
      sinks$user_id <- user_id
    }

    # Return sinks
    sinks <- structure(sinks, class = c("sinks", class(sinks)))
    return(sinks)

  }
  # Points of interest
  if(type == "poi"){
    # Prepare other points
    poi <- points %>%
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
