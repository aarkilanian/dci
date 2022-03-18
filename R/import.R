import_rivers <- function(path, weight = NULL, sf = FALSE){

  # Check that weight is valid

  # Read shapefile from path if not sf object
  if(!sf){

    # Read in river with sf
    rivers <- sf::read_sf(path)

  }

  # Prepare rivers
  rivers <- rivers %>%
    # Remove Z/M dimensions
    sf::st_zm() %>%
    # Cast all fatures to linestring geometries
    sf::st_cast("LINESTRING")

  # Return rivers
  invisible(rivers)

}

import_points <- function(path, type, perm = NULL, sf = FALSE){

  # Check for valid type

  # Read shapefile from path if not sf object
  if(!sf){
    # Read in point data
    points <- sf::read_sf(path)
  }

  if(type == "Barrier"){

    # Prepare barriers
    barriers <- points %>%
      # Remove Z/M dimension
      sf::st_zm(barriers) %>%
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
    # Select only created columns
    barriers <- barriers %>%
      dplyr::select(id, perm, type)

    # Return barriers
    invisible(barriers)

  }

  if(type == "Sink"){

    # Prepare sinks
    sinks <- points %>%
      # Remove Z/M dimensions
      sf::st_zm() %>%
      # Assign sink IDs
      dplyr::mutate(id = dplyr::row_number()) %>%
      # Assign sink type
      dplyr::mutate(type = "Sink") %>%
      # Assign permeability of 1
      dplyr::mutate(perm = 1) %>%
      # Select only newly created columns
      dplyr::select(id, perm, type)

    # Return sinks
    invisible(sinks)

  }

  if(type == "Other"){

    # Prepare other points
    others <- points %>%
      # Remove Z/M dimensions
      sf::st_zm() %>%
      # Assign other IDs
      dplyr::mutate(id = dplyr::row_number()) %>%
      # Assign other type
      dplyr::mutate(type = "Other") %>%
      # Assign permeability of 1
      dplyr::mutate(perm = 1) %>%
      # Select only newly created columns
      dplyr::select(id, perm, type)

    # Return others
    invisible(others)

  }

}
