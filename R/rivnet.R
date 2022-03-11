# Simplified constructor
# For internal use only
new_rivnet <- function(rivers,
                       barriers,
                       sinks = NULL,
                       riv.weight = NULL,
                       bar_perm = NULL,
                       extra.pts = NULL,
                       snap = FALSE,
                       snap.tolerance = 5,
                       correct.topology = TRUE){



  # Prepare barriers
  barriers <- barriers %>%
    # Remove Z/M dimension
    sf::st_zm(barriers) %>%
    # Match river projection
    sf::st_transform(sf::st_crs(rivers)) %>%
    # Assign barrier IDs
    dplyr::mutate(id = dplyr::row_number()) %>%
    # Assign barrier type
    dplyr::mutate(type = "Barrier")
  # Assign 0% permeability to barriers by default if other permeability is not supplied
  if(is.null(bar.perm)){
    barriers$perm <- 0
    # If barrier permeabilities are supplied attempt to add them to the barriers
  } else {
    # Convert barrier permeabilities to double type
    barriers$perm <- as.double(barriers[[bar.perm]])
  }
  # Select only created columns
  barriers <- barriers %>%
    dplyr::select(id, perm, type)

  # Prepare sinks
  if(!is.null(sinks)){
    sinks <- sinks %>%
      # Remove Z/M dimensions
      sf::st_zm() %>%
      # Match river projection
      sf::st_transform(sf::st_crs(rivers)) %>%
      # Assign sink IDs
      dplyr::mutate(id = dplyr::row_number()) %>%
      # Assign sink type
      dplyr::mutate(type = "Sink") %>%
      # Assign permeability of 1
      dplyr::mutate(perm = 1) %>%
      # Select only newly created columns
      dplyr::select(id, perm, type)
  }

  # Prepare extra points
  if(!is.null(extra.pts)){
    # Remove Z/M dimension from sinks
    extra.pts <- extra.pts %>%
      sf::st_zm() %>%
      # Match river projection
      sf::st_transform(sf::st_crs(rivers)) %>%
      # Assign extra IDs
      dplyr::mutate(id = dplyr::row_number()) %>%
      # Assign extra type
      dplyr::mutate(type = "Extra") %>%
      # Assign permeability of 1
      dplyr::mutate(perm = 1) %>%
      # Select only newly created columns
      dplyr::select(id, perm, type)
  }

  # Combine nodes together
  user_nodes <- dplyr::bind_rows(barriers, sinks, extra.pts)

  # Prepare rivers
  rivers <- rivers %>%
    # Remove Z/M dimensions
    sf::st_zm() %>%
    # Cast all fatures to linestring geometries
    sf::st_cast("LINESTRING")

  # Clean up topology if requested
  if(correct.topology == TRUE){
    # Perform necessary corrections
    rivers <- enforce_dendritic(rivers)
  }

  # Split rivers according to node locations
  rivers_resplit <- sf::st_collection_extract(lwgeom::st_split(rivers, user_nodes), "LINESTRING") %>%
    dplyr::mutate(rivID = 1:dplyr::n()) %>%
    sf::st_as_sf()

  # Create final sfnetwork
  rivnet <- as_sfnetwork(rivers_resplit)

  # Join special nodes' attributes to network nodes
  rivnet <- rivnet %>%
    activate(nodes) %>%
    st_join(nodes_snap, largest=TRUE) %>%
    # Set node type of topological nodes
    dplyr::mutate(type = dplyr::if_else(is.na(type), "Topo", type)) %>%
    # Set topological node permeability
    dplyr::mutate(perm = dplyr::if_else(is.na(perm), 1, perm)) %>%
    # Add unique node IDs
    dplyr::mutate(nodeID = dplyr::row_number())

  # Apply binary labeling
  rivnet <- binary_labeling(rivnet)

  # Apply membership labelling
  rivnet <- member_labelling(rivnet)

  # Define rivnet class
  rivnet <- structure(rivnet, class = c("rivnet", class(rivnet)))
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
