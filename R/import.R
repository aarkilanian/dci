#' Prepare rivers for \code{\link{river_net}} connectivity analyses
#'
#'Read and prepare geospatial river lines data for dci.
#'
#' @param path A character string or \code{\link{sf}} object, the path to a shapefile of river lines or
#'
#' @param min_comp An integer value, the minimum number of river nodes in a network component. Isolated networks with less than this specified number of nodes will be discarded. Set to 10 by default.
#'
#' @return Object of class rivers prepared for input to \code{\link{river_net}}
#'
#' @export
import_rivers <- function(path, min_comp = 10, quiet = FALSE){
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

  # Discard small component fragments
  net <- rivers %>%
    sfnetworks::as_sfnetwork() %>%
    # Retain only components with set minimum nodes
    dplyr::mutate(component = tidygraph::group_components()) %>%
    dplyr::group_by(component) %>%
    dplyr::filter(dplyr::n() > min_comp)
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
#' @param perm An optional double vector, barrier permeabilities ranging from 0 to 1. These will be ignored for non-barrier points. Set to NULL by default.
#'
#' @return Object of class barriers, sinks, or poi prepared for input to \code{\link{river_net}}
#'
#' @export
import_points <- function(path, type, perm = NULL, quiet = FALSE){
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
  # Check that permeability is valid
  if(!is.null(perm)){
    user_perm <- tryCatch(
      as.double(points[[perm]]),
      error = function(e) {
        stop("Supplied permeability field cannot be assigned because: ", e, call. = FALSE)
      }
    )
    # Check that permeability is between 0 and 1
    if(any(abs(user_perm) > 1)){
      stop("Permeability values must be between 0 and 1.")
    }
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
  if(type == "barriers"){
    # Prepare barriers
    barriers <- points %>%
      # Remove Z/M dimension
      sf::st_zm() %>%
      # Assign barrier IDs
      dplyr::mutate(id = dplyr::row_number()) %>%
      # Assign barrier type
      dplyr::mutate(type = "Barrier")
    # Assign 0% permeability to barriers by default if other permeability is not supplied
    if(is.null(perm)){
      barriers$perm <- 0
      # If barrier permeabilities are supplied attempt to add them to the barriers
    } else {
      # Convert barrier permeabilities to double type
      barriers$perm <- as.double(barriers[[perm]])
    }
    # Print prepared barriers
    if(quiet == FALSE){
      plot(sf::st_geometry(barriers))
    }
    # Return barriers
    barriers <- structure(barriers, class = c("barriers", class(barriers)))
    return(barriers)
  }
  if(type == "sinks"){
    # Prepare sinks
    sinks <- points %>%
      # Remove Z/M dimensions
      sf::st_zm() %>%
      # Assign sink IDs
      dplyr::mutate(id = dplyr::row_number()) %>%
      # Assign sink type
      dplyr::mutate(type = "Sink") %>%
      # Assign permeability of 1
      dplyr::mutate(perm = 1)
    # Print prepared sinks
    if(quiet == FALSE){
      plot(sf::st_geometry(sinks))
    }
    # Return sinks
    sinks <- structure(sinks, class = c("sinks", class(sinks)))
    return(sinks)
  }
  if(type == "poi"){
    # Prepare other points
    poi <- points %>%
      # Remove Z/M dimensions
      sf::st_zm() %>%
      # Assign other IDs
      dplyr::mutate(id = dplyr::row_number()) %>%
      # Assign other type
      dplyr::mutate(type = "poi") %>%
      # Assign permeability of 1
      dplyr::mutate(perm = 1)
    # Print prepared others
    if(quiet == FALSE){
      plot(sf::st_geometry(poi))
    }
    # Return others
    poi <- structure(poi, class = c("poi", class(poi)))
    return(poi)
  }
}
