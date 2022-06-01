#' Create a \code{river_net} object
#'
#' \code{river_net} is a geospatial network structure of river data. It extends the tidy representation of geospatial network data from the \code{\link[sfnetworks]{sfnetwork}} data structure.
#'
#' @param rivers A \code{rivers} object returned by \code{\link{import_rivers}}.
#'
#' @param barriers A \code{barriers} object returned by \code{\link{import_points}}.
#'
#' @param sinks A \code{sinks} object returned by \code{\link{import_points}}. This data is optional.
#'
#' @param poi A \code{others} object returned by \code{\link{import_points}}. This data is optional.
#'
#' @param check A logical value, if \code{TRUE}, the default, dendritic topology of the river network is enforced with \code{\link{enforce_dendritic}}.
#'
#' @return An object of class \code{\link{river_net}} representing the river network formed by the geospatial lines and points provided.
#'
#' @export
river_net <- function(rivers,
                   barriers,
                   sinks = NULL,
                   poi = NULL,
                   check = TRUE){

  # Check rivers
  if(!("rivers" %in% class(rivers))){
    stop("Rivers must first be imported with `import_rivers`")
  }

  # Check barriers
  if(!("barriers" %in% class(barriers))){
    stop("Barriers must first be imported with `import_points`")
  }

  # Check sinks
  if(!is.null(sinks)){
    if(!("sinks" %in% class(sinks))){
      stop("Sinks must first be imported with `import_points`")
    }
  }

  # Check other points
  if(!(is.null(others))){
    if(!("others" %in% class(others))){
      stop("Other points must first be imported with `import_points`")
    }
  }

  # Check that there aren't repeating column names across inputs

  # Match river projection
  barriers <- sf::st_transform(barriers, sf::st_crs(rivers))
  sinks <- sf::st_transform(sinks, sf::st_crs(rivers))
  if(!is.null(others)){
    others <- sf::st_transform(others, sf::st_crs(rivers))
    # Combine nodes
    user_nodes <- dplyr::bind_rows(barriers, sinks, others)
  } else{
    user_nodes <- dplyr::bind_rows(barriers, sinks)
  }

  # Clean up topology if requested
  if(check == TRUE){
    # Perform necessary corrections
    rivers <- enforce_dendritic(rivers)
  }

  # Split rivers at user node locations
  rivers <- split_rivers_at_points(rivers, user_nodes, tolerance) %>%
    dplyr::mutate(rivID = 1:dplyr::n())

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

  # Define river_net class
  net <- structure(net, class = c("river_net", class(net)))
  invisible(net)

}
