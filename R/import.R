#' Prepare Rivers for Connectivity Analyses
#'
#' Reads and prepares geospatial river line data for use in [river_net()].
#' Only the largest fully connected component of the network is retained;
#' river lines that are part of disconnected secondary networks are discarded.
#'
#' @param rivers A character string specifying the path to a shapefile of river lines,
#'   or an [sf] object representing river geometries.
#' @param quiet Logical. If `FALSE` (default), plots the original and processed
#'   river lines side-by-side for visual inspection.
#'
#' @return An object of class `rivers`, suitable for use with [enforce_dendritic()]
#'   or as input to [river_net()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' import_rivers(rivers = "path/to/shapefile.shp")
#' import_rivers(rivers = sf_line_object)
#' }
import_rivers <- function(rivers, quiet = FALSE) {
  # Check for path type
  if (is.character(rivers)) {
    sf <- FALSE
  } else {
    sf <- TRUE
  }
  # Read shapefile from path if not sf object
  if (!sf) {
    # Read in river with sf
    rivers <- tryCatch(sf::read_sf(rivers),
      error = function(e) rlang::abort("invalid spatial data provided")
    )
  } else {
    rivers <- rivers
  }

  # Check that spatial data is lines
  if (!any(sf::st_geometry_type(rivers) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("Provided data contains geometries other than LINESTRING and MULTILINESTRING")
  }

  # Check projected
  if (sf::st_is_longlat(rivers) == TRUE) {
    stop("Provided spatial data is not projected")
  }

  # Store original rivers for later plotting
  rivers.old <- sf::st_zm(rivers)

  # Remove Z/M dimensions
  rivers <- sf::st_zm(rivers)

  # Cast all features to linestring geometries
  rivers <- sf::st_cast(rivers, "LINESTRING")

  # Check for valid and empty geometries
  if (any(!(sf::st_is_valid(rivers))) | any(sf::st_is_empty(rivers))) {
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
  if (quiet == FALSE) {
    plot(sf::st_geometry(rivers.old), col = "red")
    plot(sf::st_geometry(rivers), add = T, lwd = 2)
  }

  # Return rivers
  rivers <- structure(rivers, class = c("rivers", class(rivers)))
  invisible(rivers)
}

#' Prepare Point Data for Connectivity Analyses
#'
#' Reads and prepares geospatial point data for use with [river_net()].
#'
#' @param pts A character string specifying the path to a shapefile of points,
#'   or an [sf] object containing point features.
#' @param type A character string indicating the type of points. Must be one of:
#'   `"bars"` for barriers, `"out"` for the outlet, or `"poi"` for points of interest.
#'
#' @return An object of class `barriers`, `outlet`, or `poi`, depending on `type`,
#'   prepared for use with [river_net()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' import_points(pts = "path/to/points.shp", type = "bars")
#' import_points(pts = sf_point_object, type = "poi")
#' }
import_points <- function(pts, type) {
  # Check that type is valid
  if (!(type %in% c("bars", "out", "poi"))) stop("Points must be of 'bars', 'out', or 'poi' type.")

  # Check for path type
  if (is.character(pts)) {
    sf <- FALSE
  } else {
    sf <- TRUE
  }

  # Read shapefile from path if not sf object
  if (!sf) {
    # Read in points with sf
    pts <- tryCatch(sf::read_sf(pts),
      error = function(e) rlang::abort("invalid spatial data provided")
    )
  } else {
    pts <- pts
  }

  # Check if projected
  if (sf::st_is_longlat(pts) == TRUE) {
    stop("Provided spatial data is not projected")
  }

  # Check for valid and empty geometries
  if (any(!(sf::st_is_valid(pts))) | any(sf::st_is_empty(pts))) {
    stop("Invalid geometries detected in points")
  }

  # Check for overlap
  if (any(sf::st_intersection(pts)$n.overlaps > 1)) {
    stop("There are overlapping geometries in the data provided")
  }

  # Remove Z/M dimension
  pts <- sf::st_zm(pts)

  # Barriers
  if (type == "bars") {
    # Assign barrier type
    pts$type <- "barrier"

    # Return barriers
    barriers <- structure(pts, class = c("barriers", class(pts)))
    return(barriers)
  }

  # outlet
  if (type == "out") {
    # Check that there is only 1 point
    if (nrow(pts) != 1) stop("Multiple points found. The outlet must be a single point.")

    # Assign outlet type
    pts$type <- "outlet"

    # Return sinks
    outlet <- structure(pts, class = c("outlet", class(pts)))
    return(outlet)
  }
  # Points of interest
  if (type == "poi") {
    # Assign poi type
    pts$type <- "poi"

    # Return others
    poi <- structure(pts, class = c("poi", class(pts)))
    return(poi)
  }
}
