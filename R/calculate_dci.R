#' Calculate DCI for a `river_net` Object
#'
#' Calculates the potamodromous and diadromous forms of the Dendritic
#' Connectivity Index (DCI) for a [`river_net`] object.
#'
#' @details
#' Passability values are probabilities between 0 and 1, where 0 indicates a
#' fully impassable barrier and 1 indicates full passability. If values in the
#' specified passability column fall outside this range, they will be
#' normalized.
#'
#' Weighting values should also be probabilities between 0 and 1. River segments
#' with weights of 0 or `NA` will be excluded from the DCI calculation.
#'
#' Upon successful calculation, the global DCI value for the river network will
#' be printed to the console unless `quiet = TRUE`.
#'
#' @param net A [`river_net`] object.
#' @param form A string specifying the DCI form to calculate. Options are:
#'   "pot" for potamodromous or "dia" for diadromous.
#' @param pass The name of a column in the nodes table of `net` containing
#'   numeric passability values. If `NULL`, all barriers are assumed to have 0
#'   passability.
#' @param weight The name of a column in the edges table of `net` containing
#'   numeric weights for river lengths. If `NULL`, DCI is calculated using river
#'   length only.
#' @param threshold Optional numeric value specifying a dispersal limit in map
#'   units. If `NULL` (default), no limit is applied.
#' @param parallel Logical. If `FALSE`, the default, all operations are
#' performed in series. If `TRUE` parallel operation is performed using
#' [furrr::future_pmap()]. Specify the number of workers and strategy using
#' [future::plan()].
#' @param quiet Logical. If `FALSE`, prints the global DCI and a plot of river
#'   segments to the console. Defaults to `TRUE`.
#'
#' @return An `sf` object of the river network with new columns specifying
#' segmental DCI values and relative DCI values. These are each segment's
#' contribution to the global DCI score which is printed. The relative values
#' are simply those values normalized.
#'
#' @export
#'
#' @examples
#' # For the potamodromous DCI
#' res <- calculate_dci(net = yamaska_net, form = "pot", pass = "pass_1",
#' weight = "weight")
calculate_dci <- function(net, form, pass = NULL, weight = NULL,
                          threshold = NULL, parallel = FALSE, quiet = FALSE) {
  # Check that network is valid
  if (!("river_net" %in% class(net))) {
    stop("A valid river_net object is required.")
  }
  # Check that form is valid
  if (!(form %in% c("pot", "dia"))) {
    stop("A valid form of the DCI must be requested. Either `pot` or `dia`")
  }

  # Check parallel dependencies
  if (parallel) {
    if (!requireNamespace("furrr", quietly = TRUE)) {
      stop("Package 'furrr' must be installed to use parallel execution.")
    }
  }

  # Extract edges
  net_edges <- as.data.frame(activate(net, edges))

  # Extract nodes
  net_nodes <- as.data.frame(activate(net, nodes))

  # Check that passability is valid
  if (!is.null(pass)) {
    user_perm <- tryCatch(
      as.double(net_nodes[[pass]]),
      error = function(e) {
        stop("Supplied passability field cannot be assigned:", e, call. = FALSE)
      },
      warning = function(w) {
        stop("Supplied passability field cannot be assigned:", w, call. = FALSE)
      }
    )
    # Set non-barrier node permeabilities to 1
    non_bar <- which(net_nodes$type != "barrier")
    user_perm[non_bar] <- 1
    # Check that passability is between 0 and 1
    if (any(user_perm > 1)) {
      user_perm <- (user_perm - min(user_perm)) /
        (max(user_perm) - min(user_perm))
    }
    # Set active passability column
    net_nodes$pass <- user_perm
    # Set binary passability if pass is left NULL
  } else {
    net_nodes$pass <- 1
    net_nodes[net_nodes$type == "barrier", ]$pass <- 0
  }

  # Check that weight is valid
  if (!(is.null(weight))) {
    user_weight <- tryCatch(
      as.double(net_edges[[weight]]),
      error = function(e) {
        stop("Supplied weight field cannot be assigned:", e, call. = FALSE)
      },
      warning = function(w) {
        stop("Supplied weight field cannot be assigned:", w, call. = FALSE)
      }
    )
    # Replace NA values with 0
    if (any(is.na(user_weight))) {
      user_weight[is.na(user_weight)] <- 0
    }
    # Check that weight is between 0 and 1
    if (any(user_weight > 1)) {
      user_weight <- (user_weight - min(user_weight)) /
        (max(user_weight) - min(user_weight))
    }
    # Set active weighting column
    net_edges$riv_weight <- user_weight
  }

  # Move edge attributes to nodes
  # Weights from edges associated w/ upstream nodes
  net_nodes <- net_nodes %>%
    dplyr::mutate(nodeID = dplyr::row_number()) %>%
    dplyr::left_join(net_edges, by = c("nodeID" = "from"), multiple = "any")
  net_nodes <- sf::st_as_sf(net_nodes, sf_column_name = "geometry.x")
  # Set outlet length to 0
  net_nodes[net_nodes$type == "outlet", ]$riv_length <- 0

  if (!(is.null(weight))) {
    # Calculate total weighted length of segments
    seg_weights <- as.data.frame(net_nodes) %>%
      dplyr::mutate(weighted_len = .data$riv_length * .data$riv_weight) %>%
      dplyr::group_by(.data$member_label) %>%
      dplyr::summarise(segweight = sum(.data$weighted_len, na.rm = TRUE)) %>%
      # Remove members with 0 length
      dplyr::filter(.data$segweight != 0)
  } else {
    # Calculate segment total lengths
    seg_weights <- as.data.frame(net_nodes) %>%
      dplyr::group_by(.data$member_label) %>%
      dplyr::summarise(segweight = sum(.data$riv_length, na.rm = TRUE)) %>%
      dplyr::filter(.data$segweight != 0)
  }

  # Calculate total weight of network
  totweight <- sum(seg_weights$segweight)

  # Gather member IDs
  all_members <- seg_weights$member_label

  # Identify outlet segment for diadromous
  if (form %in% c("dia")) {
    outlet_seg <- activate(net, nodes) %>%
      dplyr::filter(.data$type == "outlet") %>%
      dplyr::pull(.data$member_label)
  }

  # If no distance threshold is supplied
  if (is.null(threshold)) {
    # Calculate relative length of segments
    seg_weights$segweight <- seg_weights$segweight / totweight

    # Potamodromous case
    if (form == "pot") DCIs <- calculate_dci_pot(all_members, net_nodes,
                                                 seg_weights, parallel, quiet)
    # Diadromous case
    if (form == "dia") DCIs <- calculate_dci_dia(all_members, net_nodes,
                                                 seg_weights, outlet_seg,
                                                 parallel, quiet)
    # Return calculated DCI values
    DCIs <- structure(DCIs, class = c("dci_results", class(DCIs)))
    return(DCIs)

    # If distance threshold is supplied
  } else {
    # Check if weighting is requested
    weighted <- FALSE
    if (!is.null(weight)) weighted <- TRUE

    # Potamodromous case
    if (form == "pot") DCIs <- calculate_dci_pot_thresh(net, all_members,
                                                        net_nodes, seg_weights,
                                                        weighted, threshold,
                                                        totweight, parallel,
                                                        quiet)
    # Diadromous case
    if (form == "dia") DCIs <- calculate_dci_dia_thresh(net, all_members,
                                                        net_nodes, seg_weights,
                                                        weighted, threshold,
                                                        totweight, outlet_seg,
                                                        parallel, quiet)
    # Return calculated DCI values
    DCIs <- structure(DCIs, class = c("dci_results", class(DCIs)))
    return(DCIs)
  }
}

#' Calculate non-thresholded potamodromous DCI
#'
#' @inheritParams calculate_dci
#' @param all_members An integer vector holding all assigned membership labels
#'   in the [`river_net`] object.
#' @param net_nodes An `sf` object of the nodes of the
#'   [`river_net`]
#'   object with river attributes joined.
#' @param seg_weights A data frame of each segments total length. Either
#'   weighted or unweighted depending on parameters.
#'
#' @return A data frame which holds raw and relative DCI scores for each
#' segment.
#'
#' @keywords internal
calculate_dci_pot <- function(all_members, net_nodes, seg_weights, parallel,
                              quiet) {
  # Determine segment pairs
  from_segment <- rep(all_members,
    each = length(all_members)
  )
  to_segment <- rep(all_members,
    times = length(all_members)
  )

  # Calculate passability between each pair of segments
  if (parallel) {
    pass <- furrr::future_pmap_dbl(list(from_segment, to_segment), gather_perm,
                                   nodes = net_nodes)
  } else {
    pass <- mapply(gather_perm, from_segment, to_segment, MoreArgs =
                     list(nodes = net_nodes))
  }

  # Gather DCI inputs and calculate sub-segmental DCI
  DCIs_sub <- data.frame(
    from = from_segment,
    to = to_segment,
    pass
  )
  DCIs_sub <- dplyr::left_join(DCIs_sub, seg_weights,
                               by = c("from" = "member_label"))
  names(DCIs_sub)[names(DCIs_sub) == "segweight"] <- "from_len"
  DCIs_sub <- dplyr::left_join(DCIs_sub, seg_weights,
                               by = c("to" = "member_label"))
  names(DCIs_sub)[names(DCIs_sub) == "segweight"] <- "to_len"
  DCIs_sub$DCIs <- DCIs_sub$from_len * DCIs_sub$to_len * DCIs_sub$pass * 100

  # Group DCI results by from segment to obtain segmental DCI
  DCIs <- DCIs_sub %>%
    dplyr::group_by(.data$from) %>%
    dplyr::summarise(DCI = sum(.data$DCIs))
  names(DCIs)[names(DCIs) == "from"] <- "segment"
  DCI_glob <- sum(DCIs$DCI)
  DCIs$DCI_rel <- DCIs$DCI / DCI_glob * 100
  DCIs <- as.data.frame(DCIs)

  # Print global dci
  if (!quiet) {
    message(paste0("potamodromous DCI: ", DCI_glob))
  }

  # Return DCIs summary
  return(DCIs)
}

#' Calculate non-thresholded diadromous DCI
#'
#' @inheritParams calculate_dci
#' @inheritParams calculate_dci_pot
#' @param outlet_seg An integer indicating the membership label of the outlet
#' segment
#'
#' @return A data frame which holds raw and relative DCI scores for each
#' segment.
#'
#' @keywords internal
calculate_dci_dia <- function(all_members, net_nodes, seg_weights, outlet_seg,
                              parallel, quiet) {
  # Determine segment pairs
  to_segment <- all_members
  from_segment <- rep(outlet_seg, times = length(to_segment))

  # Calculate passability between each pair of segments
  if (parallel) {
    pass <- furrr::future_pmap_dbl(list(from_segment, to_segment), gather_perm,
                                   nodes = net_nodes)
    } else {
    pass <- mapply(gather_perm, from_segment, to_segment,
                   MoreArgs = list(nodes = net_nodes))
  }

  # Gather DCI inputs and calculate sub-segmental DCI
  DCIs_sub <- data.frame(
    from = from_segment,
    to = to_segment,
    pass
  )
  DCIs_sub <- dplyr::left_join(DCIs_sub, seg_weights,
                               by = c("to" = "member_label"))
  names(DCIs_sub)[names(DCIs_sub) == "segweight"] <- "to_len"
  DCIs_sub$DCIs <- DCIs_sub$to_len * DCIs_sub$pass * 100

  # Group DCI results by from segment to obtain segmental DCI
  DCIs <- DCIs_sub %>%
    dplyr::group_by(.data$to) %>%
    dplyr::summarise(DCI = sum(.data$DCIs))
  names(DCIs)[names(DCIs) == "to"] <- "segment"
  DCI_glob <- sum(DCIs$DCI)
  DCIs$DCI_rel <- DCIs$DCI / DCI_glob * 100
  DCIs <- as.data.frame(DCIs)

  # Print global dci
  if (!quiet) {
    message(paste0("diadromous DCI: ", DCI_glob))
  }

  # Return DCIs summary
  return(DCIs)
}

#' Calculate non-thresholded invasive DCI
#'
#' @inheritParams calculate_dci
#' @param all_members An integer vector holding all assigned membership labels
#'   in the [`river_net`] object.
#' @param net_nodes An `sf` object of the nodes of the
#' [`river_net`] object with river attributes joined.
#' @param seg_weights A data frame of each segments total length. Either
#'   weighted or unweighted depending on parameters.
#'
#' @return A data frame which holds raw and relative DCI scores for each
#' segment.
#'
#' @keywords internal
calculate_dci_pot_thresh <- function(net, all_members, net_nodes, seg_weights,
                                     weighted, threshold, totweight, parallel,
                                     quiet) {
  # Determine segment pairs
  from_segment <- rep(all_members,
    each = length(all_members)
  )
  to_segment <- rep(all_members,
    times = length(all_members)
  )

  # Calculate segment-segment distance between each pair
  if (parallel) {
    distances <- furrr::future_pmap_dbl(list(from_segment, to_segment), gather_dist,
                           nodes = net_nodes)
  } else {
    distances <- mapply(gather_dist, from_segment, to_segment,
                        MoreArgs = list(nodes = net_nodes))
  }

  # Remove pairs with distances larger than the threshold
  discard_pairs <- which(distances > threshold)
  if (length(discard_pairs) != 0) {
    from_segment <- from_segment[-discard_pairs]
    to_segment <- to_segment[-discard_pairs]
    distances <- distances[-discard_pairs]
  }

  # Calculate passability between remaining pairs
  if (parallel) {
    perms <- furrr::future_pmap_dbl(list(from_segment, to_segment), gather_perm,
                                    nodes = net_nodes)
  } else {
    perms <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))
  }

  # Calculate DCI
  if (parallel) {
    DCIs <- furrr::future_pmap_dbl(list(from_segment, to_segment, distances,
                                        perms), gather_dci, nodes = net_nodes, seg_weights,
                                        threshold, totweight, weighted,
                                        form = "potamodromous")
  } else {
    DCIs <- mapply(gather_dci, from_segment, to_segment, distances, perms, MoreArgs = list(net = net, nodes = net_nodes, seg_weights, threshold, totweight, weighted, form = "potamodromous"))
  }
  DCI_glob <- sum(DCIs, na.rm = TRUE)

  # Print global dci
  if (!quiet) {
    message(paste0("potamodromous DCI with distance limit of ", threshold, ": ", DCI_glob))
  }

  # Return result
  DCI_res <- data.frame(from_segment, to_segment, DCIs)
  DCI_res <- DCI_res %>%
    dplyr::group_by(.data$from_segment) %>%
    dplyr::summarise(DCI = sum(.data$DCIs, na.rm = TRUE))
  DCI_res$DCI_rel <- DCI_res$DCI / DCI_glob * 100
  names(DCI_res)[names(DCI_res) == "from_segment"] <- "segment"
  DCI_res <- as.data.frame(DCI_res)
  return(DCI_res)
}

#' Calculate thresholded diadromous DCI
#'
#' @inheritParams calculate_dci
#' @inheritParams calculate_dci_pot_thresh
#' @param outlet_seg An integer indicating the membership label of the outlet segment
#'
#' @keywords internal
calculate_dci_dia_thresh <- function(net, all_members, net_nodes, seg_weights, weighted, threshold, totweight, outlet_seg, parallel, quiet) {
  # Determine segment pairs
  to_segment <- all_members
  from_segment <- rep(outlet_seg, times = length(to_segment))

  # Remove pairs of segments further than threshold
  if (parallel) {
    distances <- furrr::future_pmap_dbl(list(from_segment, to_segment), gather_dist,
                                        nodes = net_nodes)
  } else {
    distances <- mapply(gather_dist, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))
  }

  # Remove pairs with distances larger than the threshold
  discard_pairs <- which(distances > threshold)
  if (length(discard_pairs) != 0) {
    from_segment <- from_segment[-discard_pairs]
    to_segment <- to_segment[-discard_pairs]
    distances <- distances[-discard_pairs]
  }

  # Calculate passability between remaining pairs
  if (parallel) {
    perms <- furrr::future_pmap_dbl(list(from_segment, to_segment), gather_perm,
                                    nodes = net_nodes)
  } else {
    perms <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))
  }

  # Calculate DCI
  if (parallel) {
    DCIs <- furrr::future_pmap_dbl(list(from_segment, to_segment, distances,
                                        perms), gather_dci, nodes = net_nodes, seg_weights,
                                   threshold, totweight, weighted,
                                   form = "diadromous")
  } else {
    DCIs <- mapply(gather_dci, from_segment, to_segment, distances, perms, MoreArgs = list(net = net, nodes = net_nodes, seg_weights, threshold, totweight, weighted, form = "diadromous"))
  }
  DCI_glob <- sum(DCIs)

  # Print global dci
  if (!quiet) {
    message(paste0("diadromous DCI with distance limit of ", threshold, ": ", DCI_glob))
  }

  # Gather results
  DCI_res <- data.frame(from_segment, to_segment, DCIs)
  DCI_res <- DCI_res[c("to_segment", "DCIs")]
  names(DCI_res)[names(DCI_res) == "to_segment"] <- "segment"
  names(DCI_res)[names(DCI_res) == "DCIs"] <- "DCI"
  DCI_res$DCI_rel <- DCI_res$DCI / DCI_glob * 100

  # Add missing segments
  missing_segs <- all_members[!(all_members %in% DCI_res$segment)]
  if (length(missing_segs != 0)) {
    missing_rows <- list(missing_segs, rep(0, times = length(missing_segs)), rep(0, times = length(missing_segs)))
    DCI_res <- rbind(missing_rows, DCI_res)
    DCI_res <- dplyr::arrange(DCI_res, "segment")
  }

  return(DCI_res)
}

#' Calculate thresholded invasive DCI
#'
#' @inheritParams calculate_dci
#' @param weighted A logical value indicating whether river lengths in
#'   seg_weights are weighted.
#' @param totweight The total length or weighted length of the whole network.
#'
#' @return A data frame which holds raw and relative DCI scores for each segment.
#'
#' @keywords internal
gather_dci <- function(net, form, from, to, distance, pass, nodes, seg_weights, threshold, totweight, weighted) {

  # Helper to find path between two nodes
  path_between <- function(s1, s2) {
    # Get path from segments to root
    s1_path <- path_to_root(s1)
    s2_path <- path_to_root(s2)

    # Determine non-repeating nodes
    path_sub <- s1_path[!(s1_path %in% s2_path)]
    path <- append(path_sub, s2_path[!(s2_path %in% s1_path)])

    # Return list of nodes across path
    return(path)
  }

  # Helper to find the path from a node to the root node
  path_to_root <- function(seg) {
    # Prepare input vectors
    path <- rep(seg, each = length(unlist(seg)))
    len <- length(unlist(seg)):1
    # Sequentially remove final element from path until 1 left
    path_out <- mapply(function(x, y) unlist(x)[1:y],
                       path, len,
                       SIMPLIFY = TRUE
    )
  }

  # Diadromous case
  if (form == "diadromous") {
    # Case when from and to segment are the same
    if (from == to) {
      seg_length <- seg_weights[seg_weights$member_label == from, ]$segweight
      DCI <- seg_length / totweight * 1 * 100
      return(DCI)
    }

    # Extract sinks and barriers
    sinks_bars <- subset(nodes, nodes$type %in% c("outlet", "barrier"))

    # Get from segment local outlet (always outlet of network)
    from_sink <- sinks_bars[sinks_bars$type == "outlet", ]$node_label

    # Get to segment local outlet
    to_sink <- sinks_bars[sinks_bars$member_label == to, ]$node_label

    # Get path between sinks
    path <- path_between(from_sink, to_sink)

    # Join node attributes to nodes on path
    full_path <- nodes[nodes$node_label %in% path, ]

    # Determine final entrance and exit nodes for pair of segments

    # Select most downstream barrier as entrance, extract row index
    barriers <- full_path[full_path$type == "barrier", ]
    barriers$depth <- unlist(lapply(barriers$node_label, length))
    ent_label <- as.integer(rownames(barriers[which.min(barriers$depth), ]))

    # Calculate length remaining after segment-segment distance
    rem_length <- threshold - distance

    # Gather neighbourhood around exit (from segment)
    weights <- as.data.frame(activate(net, edges))
    weights <- weights$riv_length
    target <- seq_len(igraph::gorder(net))
    source <- ent_label
    dist <- igraph::distances(
      graph = net, v = source, to = target,
      mode = "all", weights = weights, algorithm = "automatic"
    )
    neighb_nodes <- which(dist <= rem_length)
    neighb_nodes <- nodes[nodes$nodeID %in% neighb_nodes, ]
    neighb_nodes <- neighb_nodes[neighb_nodes$member_label == to, ]
    neighb_length <- sum(neighb_nodes$riv_length, na.rm = T)

    if (weighted) {
      # Calculate full length of to neighbourhood
      to_length <- sum(nodes[nodes$member_label == to, ]$riv_length * nodes[nodes$member_label == to, ]$riv_weight, na.rm = TRUE)
    } else {
      # Calculate full length of to neighbourhood
      to_length <- sum(nodes[nodes$member_label == to, ]$riv_length, na.rm = TRUE)
    }

    # Calculate relative neighbourhood length for segment
    neighb_rel <- neighb_length / to_length

    # Calculate sub-segmental DCI for pair of segments
    DCI <- to_length / totweight * neighb_rel * pass * 100
    return(DCI)
  }

  # Potamodromous case
  if (form == "potamodromous") {
    # Case when from and to segment are the same
    if (from == to) {
      seg_length <- seg_weights[seg_weights$member_label == from, ]$segweight
      DCI <- seg_length / totweight * seg_length / totweight * 1 * 100
      return(DCI)
    }

    # Extract sinks and barriers
    sinks_bars <- subset(nodes, nodes$type %in% c("outlet", "barrier"))

    # Get from segment local outlet
    from_sink <- sinks_bars[sinks_bars$member_label == from, ]$node_label

    # Get to segment local outlet
    to_sink <- sinks_bars[sinks_bars$member_label == to, ]$node_label

    # Get path between sinks
    path <- path_between(from_sink, to_sink)

    # Join node attributes to nodes on path
    full_path <- nodes[nodes$node_label %in% path, ]

    # Determine final entrance and exit nodes for pair of segments
    # If from segment is upstream of to segment
    if (length(unlist(from_sink)) > length(unlist(to_sink))) {
      # Set exit to one node upstream of from outlet, extract row index
      exit_label <- list(append(unlist(from_sink), FALSE))
      exit_label <- as.integer(rownames(nodes[nodes$node_label %in% exit_label, ]))

      # If from segment is downstream of to segment
    } else {
      # Select most downstream barrier as exit, extract row index
      barriers <- full_path[full_path$type == "barrier", ]
      barriers$depth <- unlist(lapply(barriers$node_label, length))
      exit_label <- as.integer(rownames(barriers[which.min(barriers$depth), ]))
    }

    # Calculate length remaining after segment-segment distance
    rem_length <- threshold - distance

    # Gather neighbourhood around exit (from segment)
    weights <- as.data.frame(activate(net, edges))
    weights <- weights$riv_length
    target <- seq_len(igraph::gorder(net))
    source <- exit_label
    dist <- igraph::distances(
      graph = net, v = source, to = target,
      mode = "all", weights = weights, algorithm = "automatic"
    )
    neighb_nodes <- which(dist <= rem_length)
    neighb_nodes <- nodes[nodes$nodeID %in% neighb_nodes, ]
    neighb_nodes <- neighb_nodes[neighb_nodes$member_label == from, ]
    neighb_length <- sum(neighb_nodes$riv_length, na.rm = T)

    if (weighted) {
      # Calculate full length of from neighbourhood
      from_length <- sum(nodes[nodes$member_label == from, ]$riv_length * nodes[nodes$member_label == from, ]$riv_weight, na.rm = TRUE)
      # Calculate full length of to neighbourhood
      to_length <- sum(nodes[nodes$member_label == to, ]$riv_length * nodes[nodes$member_label == to, ]$riv_weight, na.rm = TRUE)
    } else {
      # Calculate full length of from neighbourhood
      from_length <- sum(nodes[nodes$member_label == from, ]$riv_length, na.rm = TRUE)
      # Calculate full length of to neighbourhood
      to_length <- sum(nodes[nodes$member_label == to, ]$riv_length, na.rm = TRUE)
    }

    # Calculate relative neighbourhood length for segment
    neighb_rel <- neighb_length / from_length

    # Calculate sub-segmental DCI for pair of segments
    DCI <- from_length / totweight * to_length / totweight * neighb_rel * pass * 100
    return(DCI)
  }
}

#' Gather total distance between two nodes
#'
#' @param from The from node's label
#' @param to The to node's label
#' @param nodes An `sf` object of the nodes of the [`river_net`] object with river attributes joined.
#'
#' @return The distance, in map units, between the two given segments.
#'
#' @keywords internal
gather_dist <- function(from, to, nodes) {

  # Helper to find path between two nodes
  path_between <- function(s1, s2) {
    # Get path from segments to root
    s1_path <- path_to_root(s1)
    s2_path <- path_to_root(s2)

    # Determine non-repeating nodes
    path_sub <- s1_path[!(s1_path %in% s2_path)]
    path <- append(path_sub, s2_path[!(s2_path %in% s1_path)])

    # Return list of nodes across path
    return(path)
  }

  # Helper to find the path from a node to the root node
  path_to_root <- function(seg) {
    # Prepare input vectors
    path <- rep(seg, each = length(unlist(seg)))
    len <- length(unlist(seg)):1
    # Sequentially remove final element from path until 1 left
    path_out <- mapply(function(x, y) unlist(x)[1:y],
                       path, len,
                       SIMPLIFY = TRUE
    )
  }

  # Extract sinks and barriers
  sinks_bars <- subset(nodes, nodes$type %in% c("outlet", "barrier"))

  # Get from segment local outlet
  from_sink <- sinks_bars[sinks_bars$member_label == from, ]$node_label

  # Get to segment local outlet
  to_sink <- sinks_bars[sinks_bars$member_label == to, ]$node_label

  # Get outlet-to-outlet path between segments
  path <- path_between(from_sink, to_sink)

  # Join member labels for nodes on path
  full_path <- nodes %>%
    dplyr::filter(.data$node_label %in% path)
  full_path <- full_path[c("node_label", "member_label")]

  # Case when segments are neighbours
  if (length(unique(full_path$member_label)) == 2) {
    return(0)
  }

  # Trim path to only segment-segment distance
  path_ss <- full_path[!(full_path$member_label %in% c(from, to)), ]$node_label
  #'
  #' @param from The from node's label
  # Get segment-segment path distance
  dist_ss <- sum(nodes %>%
    dplyr::filter(.data$node_label %in% path_ss) %>%
    dplyr::pull(.data$riv_length), na.rm = TRUE)

  # Return segment-segment distance
  return(dist_ss)
}

#' Gather passability between two nodes
#'
#' @inheritParams gather_dist
#'
#' @return The passability from 0 to 1 between the given nodes.
#'
#' @keywords internal
gather_perm <- function(from, to, nodes) {

  # Helper to find path between two nodes
  path_between <- function(s1, s2) {
    # Get path from segments to root
    s1_path <- path_to_root(s1)
    s2_path <- path_to_root(s2)

    # Determine non-repeating nodes
    path_sub <- s1_path[!(s1_path %in% s2_path)]
    path <- append(path_sub, s2_path[!(s2_path %in% s1_path)])

    # Return list of nodes across path
    return(path)
  }

  # Helper to find the path from a node to the root node
  path_to_root <- function(seg) {
    # Prepare input vectors
    path <- rep(seg, each = length(unlist(seg)))
    len <- length(unlist(seg)):1
    # Sequentially remove final element from path until 1 left
    path_out <- mapply(function(x, y) unlist(x)[1:y],
                       path, len,
                       SIMPLIFY = TRUE
    )
    # Return list of nodes to the root
    return(path_out)
  }

  # Condition when from and to are the same
  if (from == to) {
    return(1)
  }

  # Extract sinks and barriers
  sinks_bars <- subset(nodes, nodes$type %in% c("outlet", "barrier"))

  # Get from segment local outlet
  from_sink <- sinks_bars[sinks_bars$member_label == from, ]$node_label

  # Get to segment local outlet
  to_sink <- sinks_bars[sinks_bars$member_label == to, ]$node_label

  # Get path between segments
  path <- path_between(from_sink, to_sink)

  # Gather permeabilities across path
  path_perm <- prod(nodes[nodes$node_label %in% path, ]$pass)

  # Return passability between segments
  return(path_perm)
}

#' Find the path between two nodes
#'
#' @param s1 The label, a logical vector, of the origin node
#' @param s2 The label, a logical vector, of the destination node
#'
#' @return A list of logical vectors in order to move from the origin to the destination node.
#'
#' @keywords internal
path_between <- function(s1, s2) {
  # Get path from segments to root
  s1_path <- path_to_root(s1)
  s2_path <- path_to_root(s2)

  # Determine non-repeating nodes
  path_sub <- s1_path[!(s1_path %in% s2_path)]
  path <- append(path_sub, s2_path[!(s2_path %in% s1_path)])

  # Return list of nodes across path
  return(path)
}

#' Find the path to the root (outlet) of the river network
#'
#' @param seg A node label represented by a logical vector.
#'
#' @return A list of logical vectors in order to move from the origin to the root (outlet).
#'
#' @keywords internal
path_to_root <- function(seg) {
  # Prepare input vectors
  path <- rep(seg, each = length(unlist(seg)))
  len <- length(unlist(seg)):1
  # Sequentially remove final element from path until 1 left
  path_out <- mapply(function(x, y) unlist(x)[1:y],
    path, len,
    SIMPLIFY = TRUE
  )
  # Return list of nodes to the root
  return(path_out)
}
