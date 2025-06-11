#' Enforce dendritic river topology
#'
#' Identifies and optionally corrects features in a river network that violate
#' a strictly dendritic topology.
#'
#' In a dendritic network, two upstream rivers converge into a single downstream
#' river at each confluence. This function can enforce this dendritic topology
#' in a river network by detecting (and optionally correcting) two types of
#' topological errors: (1) divergences, where a single river splits into
#' multiple downstream branches (commonly forming loops or braided channels),
#' and (2) complex confluences, where more than two upstream rivers meet at a
#' single point.
#'
#' If errors are being corrected manually, rerun this function again until no
#' errors remain as correcting divergences can lead to other topological errors
#' that need to be corrected
#'
#' @inheritParams river_net
#'
#' @param correct Logical. If `FALSE` (default), no changes are made and
#'   topological issues are identified only. If `TRUE`, issues are
#'   automatically corrected.
#' @param quiet Logical. If `FALSE`, the function prints a summary including
#'   the global DCI and a map of segments. Defaults to `TRUE`.
#'
#' @return If `correct = FALSE`, returns a `sf` object with the columns
#'   `"divergent"` and `"complex"` indicating topological errors. These columns
#'   contain integer identifiers indicating which features are part of the
#'   same divergent or complex structure. If `correct = TRUE`, returns a
#'   `rivers` object with the topological issues corrected.
#'
#' @export
#'
#' @examples
#' # Import rivers
#' rivers_in <- import_rivers(yamaska_rivers, quiet = TRUE)
#'
#' # Correct errors automatically
#' rivers_cor <- enforce_dendritic(rivers_in, correct = TRUE)
#'
#' # Return highlighted topological errors for manual correction
#' rivers_uncor <- enforce_dendritic(rivers_in, correct = FALSE)
#'
#' # For large river networks it may be better to specify a smaller number of
#' # correction sweeps.
#' rivers_cor <- enforce_dendritic(rivers_in, correct = TRUE, max_iter = 3)
enforce_dendritic <- function(rivers, correct = TRUE, quiet = FALSE,
                              max_iter = 10) {

  # Create river network with length as weight
  net <- suppressWarnings(
    sfnetworks::as_sfnetwork(rivers, length_as_weight = TRUE)
  )

  # Correct complex confluences
  # If automatically correcting topology, use network with divergences corrected
  if (correct) {

    # Print iteration
    i <- 1
    if(!quiet) message(paste0("Iteration ", i, ":"))
    rivers <- correct_divergences(net, correct, quiet)
    divergences <- rivers[[2]]
    rivers <- rivers[[1]]

    # Create network from rivers
    net <- suppressWarnings(
      sfnetworks::as_sfnetwork(rivers)
    )

    rivers <- correct_complex(net, correct, quiet)
    complexes <- rivers[[2]]
    rivers <- rivers[[1]]

    # Keep correcting until no more errors
    i <- 2
    while(divergences != 0 || complexes != 0){

      # Print iteration
      if(!quiet) message(paste0("Iteration ", i, ":"))

      # Prepare new network
      net <- suppressWarnings(
        sfnetworks::as_sfnetwork(rivers, length_as_weight = TRUE)
      )

      # Correct divergences
      rivers <- correct_divergences(net, correct, quiet)
      divergences <- rivers[[2]]
      rivers <- rivers[[1]]

      # Create network from rivers
      net <- suppressWarnings(
        sfnetworks::as_sfnetwork(rivers)
      )

      # Correct complex
      rivers <- correct_complex(net, correct, quiet)
      complexes <- rivers[[2]]
      rivers <- rivers[[1]]

      # Increment counter
      i <- i + 1

      # Issue error if reached maximum iterations
      if(i > max_iter) stop("Maximum number of topology correction iterations
                             reached. Consider manual correction of some or all
                             topological errors with the standalone
                             `enforce_dendritic(correct = FALSE)` or increase
                             `max_iter` parameter.")
    }

    # Recalculate river lengths
    rivers$riv_length <- as.double(sf::st_length(rivers))

    # Return corrected rivers
    invisible(rivers)

    # If errors are set to be manually edited, use full network
  } else {

    # Get original column names
    orig_cols <- colnames(rivers)

    # Find errors
    net_div <- correct_divergences(net, correct, quiet)
    net_comp <- correct_complex(net_div, correct, quiet)

    # Clean up columns
    net_comp <- net_comp %>%
      dplyr::select(c(dplyr::all_of(orig_cols), "complexID", "divergent"))

    # Output rivers
    invisible(net_comp)
  }
}

#' Correct river divergences
#'
#' @inheritParams calculate_dci
#' @inheritParams enforce_dendritic
#'
#' @return If correct is [TRUE] a \code{\link{river_net}} object with the shorter of each divergent pair removed. If correct is \code{FALSE} a \code{\link[sf]{sf}} object with divergent pairs identified with a shared number in the new "divergent" column.
#'
#' @keywords internal
correct_divergences <- function(net, correct = TRUE, quiet = FALSE) {
  # If no corrections desired, find and return divergences
  if (!correct) {
    # Find and identify divergent pairs
    riv_divergences <- activate(net, edges) %>%
      dplyr::group_by(.data$from) %>%
      dplyr::mutate(grp_size = dplyr::n()) %>%
      dplyr::mutate(divergent = dplyr::if_else(.data$grp_size > 1, .data$from, NA_integer_)) %>%
      dplyr::ungroup()
    # Print number of divergences
    num_div <- length(unique(as.data.frame(activate(riv_divergences, edges))$divergent)) - 1
    if (!quiet) {
      message(paste0(num_div, " divergences have been found."))
    }
    # Return non-corrected divergences
    return(riv_divergences)
  }

  # Find and correct divergences. Always keep longest stream
  net_corrected <- activate(net, edges) %>%
    dplyr::group_by(.data$from) %>%
    dplyr::filter(.data$weight == max(.data$weight)) %>%
    tidygraph::ungroup(.data) %>%
    # Remove weight column
    dplyr::select(-tidyselect::all_of("weight"))

  # Identify components
  net_comp <- activate(net_corrected, nodes) %>%
    dplyr::mutate(component = tidygraph::group_components()) %>%
    dplyr::group_by(.data$component) %>%
    tidygraph::ungroup(.data)

  # Determine largest component and extract
  comps <- as.data.frame(activate(net_comp, nodes))$component
  big_comp <- sort(table(comps), decreasing = TRUE)[1]
  big_comp <- as.integer(names(big_comp))
  net_corrected <- net_comp %>%
    dplyr::filter(.data$component == big_comp)

  # Get number of removed rivers
  orig_num <- nrow(as.data.frame(activate(net, edges)))
  cor_num <- nrow(as.data.frame(activate(net_corrected, edges)))
  num_div <- orig_num - cor_num

  # Print number of corrected divergences
  if (num_div == 0) {
    if (!quiet) message("No divergences detected.")
    # Extract rivers
    rivers <- sf::st_as_sf(activate(net, edges))[,-c(1:2)]
    # Return
    invisible(list(rivers, num_div))

  } else {
    if (!quiet) message(paste0(num_div, " divergences corrected."))
    # Extract rivers
    rivers <- sf::st_as_sf(activate(net_corrected, edges))[,-c(1:2)]
    # Return
    invisible(list(rivers, num_div))
  }
}

#' Correct complex confluences
#'
#' @inheritParams calculate_dci
#' @inheritParams enforce_dendritic
#'
#' @return If correct is \code{TRUE} a \code{\link[sf]{sf}} object of rivers with complex confluences separated into two closely located valid confluences. If correct is \code{FALSE} a \code{\link[sf]{sf}} object with rivers participating in complex confluences labeled with the same number ina  new "complex" column.
#'
#' @keywords internal
correct_complex <- function(net, correct = TRUE, quiet = FALSE) {

  # Identify complex confluences
  net_undirected <- activate(tidygraph::convert(net, tidygraph::to_undirected), nodes)
  net_degree <- net_undirected %>%
    dplyr::mutate(nodeID = 1:dplyr::n()) %>%
    dplyr::mutate(degree = tidygraph::centrality_degree())
  complex_nodes <- sf::st_as_sf(net_degree) %>%
    dplyr::filter(.data$degree >= 4) %>%
    dplyr::select("degree")

  # If confluences have over 4 inputs recommend manual correction
  if (any(complex_nodes$degree > 4)) {
    if (correct) stop("Complex confluences with over 3 input tributaries have been detected. Use the standalone `enforce_dendritic()` and correct returned errors manually.")
  }
  # If no errors return unchanged rivers
  if (length(complex_nodes$degree) == 0) {
    if (!quiet) message("No complex confluences found.")
    invisible(list(sf::st_as_sf(activate(net, edges)), 0))

    # Correct complex confluences detected
  } else {
    # Print number of complex confluences found
    num_complex <- nrow(complex_nodes)
    if (!quiet) message(num_complex, " complex confluences found.")

    # Extract network rivers
    rivers <- sf::st_as_sf(activate(net, edges)) %>%
      dplyr::mutate(rivID = 1:dplyr::n())
    sf::st_agr(rivers) <- "constant"
    # Add ID to complex nodes
    complex_nodes <- complex_nodes %>%
      dplyr::mutate(complexID = 1:dplyr::n())
    # Create small buffer around confluence point
    buffer <- sf::st_buffer(complex_nodes, dist = 1)
    buffer <- sf::st_cast(buffer, "MULTILINESTRING", group_or_split = FALSE)
    sf::st_agr(buffer) <- "constant"

    # If manual editing desired, identify complex confluence rivers
    if (!correct) {
      complex_riv <- sf::st_join(rivers, buffer, left = TRUE)
      return(complex_riv)
    }

    # Create points at intersection of rivers and buffers
    buff_intersect <- sf::st_intersection(rivers, buffer)
    buff_intersect <- buff_intersect[c("rivID", "complexID", "to")]
    # Select downstream river out of buffer intersections for new confluence nodes
    new_nodes <- buff_intersect %>%
      dplyr::group_by(.data$complexID, .data$to) %>%
      dplyr::tally() %>%
      dplyr::filter(.data$n == 1) %>%
      dplyr::ungroup()
    # If there are multiple matches pick only one node per complex confluence
    new_nodes <- new_nodes[!duplicated(new_nodes$complexID), ]
    # Identify associated rivers
    new_nodes <- sf::st_join(new_nodes, buff_intersect, suffix = c("", ".new"))
    new_nodes <- new_nodes[c("complexID", "rivID")]
    # Find closest rivers to new points
    modify_rivers <- integer(length = nrow(complex_nodes))
    for (i in 1:nrow(new_nodes)) {
      # Set conlfuence
      confluence <- new_nodes$complexID[i]
      # Gather participating rivers
      candidates <- buff_intersect[buff_intersect$complexID == confluence, ]
      # Determine closest river to new confluence
      distances <- sf::st_distance(new_nodes[new_nodes$complexID == confluence,], candidates)
      ind <- which(distances == min(distances[distances > units::as_units(0, "m")]))
      modify_rivers[i] <- candidates$rivID[ind]
    }

    # Save old rivers
    rivers.old <- rivers

    # Move endpoints of closest river lines
    # Split outlet rivers at new confluence locations
    for (i in 1:length(modify_rivers)) {

      # Move river to new confluence

      # Get river geometry
      old_river <- sf::st_geometry(rivers.old[which(rivers.old$rivID == modify_rivers[i]), ])
      # Get new endpoint geometry
      new_point <- sf::st_coordinates(new_nodes[i, ])
      point_x <- new_point[,1]
      point_y <- new_point[,2]
      # Retrieve X and Y coordinates of the river
      riv_coords <- sf::st_coordinates(old_river)[,1:2]
      # Modify endpoint coordinates to match new point
      riv_coords[nrow(riv_coords),] <- c(point_x, point_y)
      # Create new LINESTRING geometry for river
      new_river <- sf::st_sfc(sf::st_linestring(riv_coords), crs = sf::st_crs(rivers))
      # Replace old geometry in rivers
      sf::st_geometry(rivers[which(rivers$rivID == modify_rivers[i]), ]) <- new_river

      # Split outlet river at confluence

      # Get river geometry
      old_river <- sf::st_geometry(rivers.old[rivers.old$rivID == new_nodes$rivID[i], ])
      # Retrieve old river X and Y coordinates
      old_coords <- sf::st_coordinates(old_river)[ ,1:2]
      # Get start point coordinates
      start_x <- old_coords[1,1]
      start_y <- old_coords[1,2]
      # Create first line segment from old confluence to new confluence
      new_river1 <- sf::st_sf(sf::st_sfc(sf::st_linestring(matrix(c(
        start_x,
        point_x,
        start_y,
        point_y
      ), 2)), crs = sf::st_crs(rivers))) %>%
        dplyr::mutate(rivID = new_nodes$rivID[i])
      # Correct geometry column name
      names(new_river1) <- c("geometry", "rivID")
      sf::st_geometry(new_river1) <- "geometry"

      # Create second line segment from new confluence to the end of original

      # Retrieve old coordinates
      new_coords2 <- old_coords
      # Replace start point
      new_coords2[1, ] <- c(point_x, point_y)
      # Create river geometry
      new_river2 <- sf::st_sf(sf::st_sfc(sf::st_linestring(new_coords2), crs = sf::st_crs(rivers))) %>%
        dplyr::mutate(rivID = nrow(rivers) + 1)
      new_river2$rivID[1] <- nrow(rivers) + 1
      # Correct geometry column name
      names(new_river2) <- c("geometry", "rivID")
      sf::st_geometry(new_river2) <- "geometry"

      # Remove old river
      rivers <- rivers[!(rivers$rivID == new_nodes$rivID[i]), ]
      # Add new rivers
      rivers <- dplyr::bind_rows(rivers, new_river1, new_river2)
    }

    # Remove any duplicate geometries if created
    rivers <- rivers %>%
      dplyr::distinct(.data$geometry, .keep_all = TRUE)

    # Remove any invalid geometries if present
    rivers <- rivers[sf::st_is_valid(rivers),]

    # Return modified rivers
    invisible(list(rivers, num_complex))
  }
}
