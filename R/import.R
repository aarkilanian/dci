import_rivers <- function(path, weight = NULL, sf = FALSE){

  # Read shapefile from path if not sf object
  if(!sf){

    # Read in river with sf
    rivers <- tryCatch(sf::read_sf(path),
      error = function(e) rlang::abort("invalid spatial data provided")
    )
  } else{
    rivers <- path
  }

  # Check for valid and empty geometries
  if(any(!(sf::st_is_valid(rivers))) | any(sf::st_is_empty(rivers))){
    stop("Invalid geometries detected in rivers")
  }

  # Check that weight is valid
  if(!(is.null(weight))){
    user_weight <- tryCatch(
      as.double(rivers[[weight]]),
      error = function(e) {
        stop("Supplied weight field cannot be assigned because: ", e, call. = FALSE)
      }
    )
  }

  # Prepare rivers
  rivers <- rivers %>%
    # Remove Z/M dimensions
    sf::st_zm() %>%
    # Cast all features to linestring geometries
    sf::st_cast("LINESTRING") %>%
    # Calculate river lengths
    dplyr::mutate(riv_length = sf::st_length())

  # Add weighting to rivers
  if(!(is.null(user_weight))){
    rivers$weight <- user_weight
  }

  # Return rivers
  rivers <- structure(rivers, class = c("rivers", class(rivers)))
  invisible(rivers)

}

import_points <- function(path, type, perm = NULL, sf = FALSE){

  # Check for valid type

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

  if(type == "Barrier"){

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
    # Select only created columns
    barriers <- barriers %>%
      dplyr::select(id, perm, type)

    # Return barriers
    barriers <- structure(barriers, class = c("barriers", class(barriers)))
    return(barriers)

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
    sinks <- structure(sinks, class = c("sinks", class(sinks)))
    return(sinks)

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
    others <- structure(others, class = c("others", class(others)))
    return(others)

  }

}
