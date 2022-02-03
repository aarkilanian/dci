# Simplified constructor
# For internal use only
new_rivnet <- function(rivers,
                       barriers,
                       sinks = NULL,
                       bar.perm = NULL,
                       extra.pts = NULL,
                       topology.check = TRUE,
                       snap = TRUE,
                       snap.tolerance = 100){

  # Prepare rivers
  rivers <- prepare_rivers(rivers)

  # Prepare barriers
  barriers <- sf::st_zm(barriers)

  # Assign 0% permeability to barriers by default if other permeability is not supplied
  if(is.null(bar.perm)){
    barriers$perm <- 0

    # If barrier permeabilities are supplied attempt to add them to the barriers
  } else {
    # Check validity of barrier permeability
    if(length(bar.perm) != nrow(barriers)){
      stop("Length of barrier permeabilities is not equal to the number of barriers")
    }
    # Convert barrier permeabilities to double type
    bar.perm <- tryCatch(
      as.double(bar.perm),
      error = function(e){
        stop("Cannot coerce barrier permeabilities to double", call. = FALSE)
      }
    )
    # Assign permeabilities to barriers
    barriers$perm <- bar.perm
  }

  # If specified, perform dendritic topology check of rivers
  if(topology.check){
    rivers <- enforce_dendritic(rivers)
  }

  # If supplied combine barriers and extra points
  if(!is.null(extra.pts)){
    # Prepare extra points
    extra.pts <- sf::st_zm(extra.pts)
    # Add 100% permeability to extra points
    extra.pts$perm <- 1
    nodes <- sf::st_combine(barriers, extra.pts)
  }

  # If supplied combine sinks to nodes
  if(!is.null(sinks)){
    sinks$perm <- 1
    nodes <- sf::st_combine(nodes, sinks)
  }

  # If specified, snap nodes to river edges
  if(snap){
    nodes <- sf::st_snap(nodes, rivers, snap.tolerance)
  }

  # Combine nodes and edges into sfnetwork object
  sfnet <- sfnetworks::sfnetwork(nodes = nodes, edges = rivers, ...)
  rivnet <- structure(sfnet, class = c("rivnet", class(sfnet)))

}

validate_rivnet <- function(){

}

# Constructor function
rivnet <- function(rivers,
                   barriers,
                   sinks = NULL,
                   bar.perm = NULL,
                   extra.pts = NULL,
                   topology.check = TRUE,
                   snap = TRUE,
                   snap.tolerance = 100){

  # Check that spatial inputs are valid sf objects
  stopifnot("Rivers are not a valid sf object" = sf::is.sf(rivers))
  stopifnot("Barriers are not a valid sf object" = sf::is.sf(barriers))
  if(!is.null(extra.pts)){
    stopifnot("Extra points are not a valid sf object" = sf::is.sf(extra.pts))
  }
  if(!is.null(sinks)){
    stopifnot("Sinks are not a valid sf object" = sf::is.sf(sinks))
  }

  # Assign 0% permeability to barriers by default if other permeability is not supplied
  if(is.null(bar.perm)){
    barriers$perm <- 0

  # If barrier permeabilities are supplied attempt to add them to the barriers
  } else {
    # Check validity of barrier permeability
    if(length(bar.perm) != nrow(barriers)){
      stop("Length of barrier permeabilities is not equal to the number of barriers")
    }
    # Convert barrier permeabilities to double type
    bar.perm <- tryCatch(
      as.double(bar.perm),
      error = function(e){
        stop("Cannot coerce barrier permeabilities to double", call. = FALSE)
      }
    )
    # Assign permeabilities to barriers
    barriers$perm <- bar.perm
  }

  # If specified, perform dendritic topology check of rivers
  if(topology.check){
    rivers <- enforce_dendritic(rivers)
  }

  # If supplied combine barriers and extra points
  if(!is.null(extra.pts)){
    # Add 100% permeability to extra points
    extra.pts$perm <- 1
    nodes <- sf::st_combine(barriers, extra.pts)
  }

  # If supplied combine sinks to nodes
  if(!is.null(sinks)){
    sinks$perm <- 1
    nodes <- sf::st_combine(nodes, sinks)
  }

  # If specified, snap nodes to river edges
  if(snap){
    nodes <- sf::st_snap(nodes, rivers, snap.tolerance)
  }

  # Combine nodes and edges into sfnetwork object
  sfnet <- sfnetworks::sfnetwork(nodes = nodes, edges = rivers, ...)
  rivnet <- structure(sfnet, class = c("rivnet", class(sfnet)))

}
