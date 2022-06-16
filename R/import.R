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
#'
#' @examples
#' \dontrun{ import_rivers(rivers = path_to_shapefile)}
#' \dontrun{ import_rivers{rivers = sf_line_object}}

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
    dplyr::group_by(.data$component) %>%
    dplyr::ungroup()
 comps <- activate(net, nodes) %>%
   as.data.frame(.data) %>%
   dplyr::select(.data$component)

  # Determine largest component and extract
  big_comp <- sort(table(comps), decreasing = TRUE)[1]
  big_comp <- as.integer(names(big_comp))
  net <- net %>% dplyr::filter(.data$component == big_comp)
  rivers <- sf::st_as_sf(activate(net, edges))

  # Calculate river lengths
  rivers$riv_length <- as.double(sf::st_length(rivers))

  # Remove from and to columns
  rivers <- subset(rivers, select = -c(from, to))

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
#' @param type A character string, either of “barrier”, “outlet”, or “poi” specifying the type of point.
#'
#' @return Object of class barriers, outlet, or poi prepared for input to \code{\link{river_net}}
#'
#' @export
#'
#' @examples
#' \dontrun{ import_points(pts = path_to_shapefile)}
#' \dontrun{ import_points(pts = sf_point_object)}
import_points <- function(pts, type){

  # Check that type is valid
  if(!(type %in% c("barriers", "outlet", "poi"))) stop("Points must be of 'barriers', 'outlet', or 'poi' type.")

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

  # Barriers
  if(type == "barriers"){
    # Prepare barriers
    # Remove Z/M dimension
    barriers <- sf::st_zm(pts)
    # Assign barrier type
    barriers$type <- "barrier"

    # Return barriers
    barriers <- structure(barriers, class = c("barriers", class(barriers)))
    return(barriers)
  }

  # outlet
  if(type == "outlet"){

    # Check that there is only 1 point
    if(nrow(pts) != 1) stop("Multiple points found. The outlet must be a single point.")

    # Prepare sinks
    # Remove Z/M dimensions
    outlet <- sf::st_zm(pts)
    # Assign outlet type
    outlet$type <- "outlet"

    # Return sinks
    outlet <- structure(outlet, class = c("outlet", class(outlet)))
    return(outlet)

  }
  # Points of interest
  if(type == "poi"){
    # Prepare poi points
    # Remove Z/M dimensions
    poi <- sf::st_zm(pts)
    # Assign poi type
    poi$type <- "poi"

    # Return others
    poi <- structure(poi, class = c("poi", class(poi)))
    return(poi)
  }
}
