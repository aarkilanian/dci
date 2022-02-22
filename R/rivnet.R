# Simplified constructor
# For internal use only
new_rivnet <- function(rivers,
                       barriers,
                       sinks = NULL,
                       riv.weight = NULL,
                       bar_perm = NULL,
                       extra.pts = NULL,
                       output_errors = FALSE,
                       snap = FALSE,
                       snap.tolerance = 100){

  ### Prepare network nodes
  #
  ## Prepare barriers
  #
  # Remove Z/M dimension from barriers
  barriers <- sf::st_zm(barriers)
  # Assign unique ID to barriers
  barriers$id <- 1:nrow(barriers)
  # Store given attributes and remove them
  barriers_old <- barriers
  barriers <- dplyr::select(barriers, id)
  # Assign 0% permeability to barriers by default if other permeability is not supplied
  if(is.null(bar_perm)){
    barriers$perm <- 0
    # If barrier permeabilities are supplied attempt to add them to the barriers
  } else {
    # Convert barrier permeabilities to double type
    bar_perm <- as.double(bar.perm)
    # Assign permeabilities to barriers
    barriers$perm <- bar.perm
  }
  # Assign a type to barrier nodes
  barriers$nodeType <- "Barrier"
  # Rename barriers to nodes
  nodes <- barriers
  #
  ## Prepare sinks
  #
  # If supplied combine sinks to barriers
  if(!is.null(sinks)){
    # Remove Z/M dimension from sinks
    sinks <- sf::st_zm(sinks)
    # Assign unique ID to sinks
    sinks$id <- 1:nrow(sinks)
    # Store given attributes and remove them
    sinks_old <- sinks
    sinks <- dplyr::select(sinks, id)
    # Add permeability of 1
    sinks$perm <- 1
    # Assign a type to sink nodes
    sinks$nodeType <- "Sink"
    # Combine sinks with existing nodes
    nodes <- rbind(nodes, sinks)
  }
  #
  ## Prepare extra points
  #
  if(!is.null(extra.pts)){
    # Remove Z/M dimension from sinks
    extra.pts <- sf::st_zm(extra.pts)
    # Assign unique ID to points
    extra.pts$id <- 1:nrow(extra.pts)
    # Store given attributes and remove them
    extra.pts_old <- extra.pts
    extra.pts <- dplyr::select(extra.pts, id)
    # Add permeability of 1
    extra.pts$perm <- 1
    # Assign a type to extra nodes
    extra.pts$nodeType <- "Extra"
    # Combine extra points with existing nodes
    nodes <- rbind(nodes, extra.pts)
  }

  ### Prepare network edges
  #
  # Remove Z/M dimension from rivers
  rivers <- sf::st_zm(rivers)
  # Convert to rivers to linestring geometries only
  rivers <- sf::st_cast(rivers, "LINESTRING")
  # Create initial sfnetwork object and topology
  river_net <- sfnetworks::as_sfnetwork(rivers)

  # Correct river splitting
  rivers <- resplit_rivers(river_net)
  # Correct non-dendritic topologies
  rivers <- enforce_dendritic(river_net, output_errors)

  # If specified, snap nodes to river edges
  if(snap){
    nodes <- sf::st_snap(nodes, rivers, snap.tolerance)
  }

  # Combine nodes and edges into sfnetwork object
  sfnet <- sfnetworks::as_sfnetwork(nodes = nodes, edges = rivers)
  rivnet <- structure(sfnet, class = c("rivnet", class(sfnet)))
  invisible(rivnet)

}

validate_rivnet <- function(){

  # Check validity of barrier permeability
  # Ensure length of permeabilities is equal to number of barriers

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
