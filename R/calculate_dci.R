#' Calculate different forms of the DCI for a \code{\link{river_net}} object
#'
#' @details
#' Passability values are probabilities from 0 to 1 where 0 indicates fully impassable and 1 indicates fully passable. If the values in the supplied passability column are not within this range they will be normalized.
#'
#' Similarly weighting values are probability from 0 to 1. Rivers with weights of either 0 or NA will not be considered when calculating the DCI.
#'
#' When DCI results are returned succesfully teh global DCI of the given river network will be printed to the console.
#'
#' @param net A \code{\link{river_net}} object.
#' @param form A string specifying the form of the DCI to calculate: either "potamodromous", "diadromous", or "all".
#' @param pass The name of a column in the nodes table of net which holds the numeric passability of nodes. If none is specified all barriers are automatically considered to have 0 passability.
#' @param weight The name of column in the edges tables of net which holds numeric weights to be applied to river lengths. If none is specified, the DCI is calculated only with river lengths.
#' @param threshold An optional numeric value specifying a dispersal limit in map units. If NULL, the default, no limit is considered.
#'
#' @return A \code{\link{sf}} object of the rivers from the provided \code{\link{river_net}} object with new columns specifying the segmental DCI values at each river location. If sites is not \code{NULL}, a \code{\link{sf}} object of the site points with their associated DCI scores.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_dci(net = net_name, form = "all", pass = "pass", weight = "river_weight", threshold = 1500)
#' calculate_dci(net = net_name, form = "potamodromous")
#' calculate_dci(net = net_name, form = "diadromous", threshold = 2100)
#' }
calculate_dci <- function(net, form, pass = NULL, weight = NULL, threshold = NULL){

  # Check that network is valid
  if(!("river_net" %in% class(net))){
    stop("A valid river_net object is required.")
  }
  # Check that form is valid
  if(!(form %in% c("potamodromous", "diadromous", "all"))){
    stop("A valid form of the DCI must be requested.")
  }

  # Extract edges
  net_edges <- as.data.frame(activate(net, edges))

  # Extract nodes
  net_nodes <- as.data.frame(activate(net, nodes))

  # Check that passability is valid
  if(!is.null(pass)){
    user_perm <- tryCatch(
      as.double(net_nodes[[pass]]),
      error = function(e) {
        stop("Supplied passability field cannot be assigned:", e, call. = FALSE)
      }
    )
    # Set non-barrier node permeabilities to 1
    non_bar <- which(net_nodes$type != "barrier")
    user_perm[non_bar] <- 1
    # Check that passability is between 0 and 1
    if(any(user_perm > 1)){
      user_perm <- (user_perm - min(user_perm)) / (max(user_perm) - min(user_perm))
    }
    # Set active passability column
    net_nodes$pass <- user_perm
  # Set binary passability if pass is left NULL
  } else {
    net_nodes$pass <- 1
    net_nodes[net_nodes$type == "barrier",]$pass <- 0
  }

  # Check that weight is valid
  if(!(is.null(weight))){
    user_weight <- tryCatch(
      as.double(net_edges[[weight]]),
      error = function(e) {
        stop("Supplied weight field cannot be assigned:", e, call. = FALSE)
      }
    )
    # Replace NA values with 0
    if(any(is.na(user_weight))){
      user_weight[is.na(user_weight)] <- 0
    }
    # Check that weight is between 0 and 1
    if(any(user_weight > 1)){
      user_weight <- (user_weight - min(user_weight)) / (max(user_weight) - min(user_weight))
    }
    # Set active weighting column
    net_edges$riv_weight <- user_weight
  }

  # Move edge attributes to nodes
  # Weights from edges associated w/ upstream nodes
  net_nodes <- net_nodes %>%
    dplyr::mutate(nodeID = dplyr::row_number()) %>%
    dplyr::left_join(net_edges, by = c("nodeID" = "from"))
  net_nodes <- sf::st_as_sf(net_nodes, sf_column_name = "geometry.x")
  # Set outlet length to 0
  net_nodes[net_nodes$type == "outlet",]$riv_length <- 0

  if(!(is.null(weight))){
    # Calculate total weighted length of segments
    seg_weights <- as.data.frame(net_nodes) %>%
      dplyr::mutate(weighted_len = .data$riv_length * .data$riv_weight) %>%
      dplyr::group_by(.data$member.label) %>%
      dplyr::summarise(segweight = sum(.data$weighted_len, na.rm = TRUE)) %>%
      # Remove members with 0 length
      dplyr::filter(.data$segweight != 0)
  } else {
    # Calculate segment total lengths
    seg_weights <- as.data.frame(net_nodes) %>%
      dplyr::group_by(.data$member.label) %>%
      dplyr::summarise(segweight = sum(.data$riv_length, na.rm = TRUE)) %>%
      dplyr::filter(.data$segweight != 0)
  }

  # Gather member IDs
  all_members <- seg_weights$member.label

  # Calculate total weight of network
  totweight <- sum(seg_weights$segweight)

  # If no distance threshold is supplied
  if(is.null(threshold)){

    # Calculate relative length of segments
    seg_weights$segweight <- seg_weights$segweight / totweight

    # Potamodromous case
    if(form == "potamodromous") DCIs <- calculate_dci_pot(all_members, net_nodes, seg_weights)

    # Diadromous case
    if(form == "diadromous"){

      # Identify outlet segment
      outlet_seg <- activate(net, nodes) %>%
        dplyr::filter(.data$type == "outlet") %>%
        dplyr::pull(.data$member.label)

      # Calculate DCI
      DCIs <- calculate_dci_dia(all_members, net_nodes, seg_weights, outlet_seg)
    }

    # Both DCI forms
    if(form == "all"){
      DCIs_pot <- calculate_dci_pot(all_members, net_nodes, seg_weights)
      DCIs_dia <- calculate_dci_dia(all_members, net_nodes, seg_weights)
      DCIs <- DCIs_pot %>%
        dplyr::left_join(.data, DCIs_dia, by = "segment", suffix = c("_pot", "_dia"))
    }

    # Return calculated DCI values
    DCIs <- structure(DCIs, class = c("dci.results", class(DCIs)))
    return(DCIs)

  # If distance threshold is supplied
  } else {

    # Check if weighting is requested
    weighted <- FALSE
    if(!is.null(weight)) weighted <- TRUE

    # Potamodromous case
    if(form == "potamodromous") DCIs <- calculate_dci_pot_thresh(net, all_members, net_nodes, seg_weights, weighted, threshold, totweight)

    # Diadromous case
    if(form == "diadromous"){

      # Identify outlet segment
      outlet_seg <- activate(net, nodes) %>%
        dplyr::filter(.data$type == "outlet") %>%
        dplyr::pull(.data$member.label)

      # Calculate DCI
      DCIs <- calculate_dci_dia_thresh(net, all_members, net_nodes, seg_weights, weighted, threshold, totweight, outlet_seg)
    }

    # Return calculated DCI values
    DCIs <- structure(DCIs, class = c("dci.results", class(DCIs)))
    return(DCIs)
  }

}

#' Calculate non-thresholded potamodromous DCI
#'
#' @param all_members An integer vector holding all assigned membership labels in the \code{\link{river_net}} object.
#' @param net_nodes An \code{\link{sf}} object of the nodes of the \code{\link{river_net}} object with river attributes joined.
#' @param seg_weights A data frame of each segments total length. Either weighted or unweighted depending on parameters.
#'
#' @return A data frame which holds raw and relative DCI scores for each segment.
#'
#' @keywords internal
calculate_dci_pot <- function(all_members, net_nodes, seg_weights){

  # Determine segment pairs
  from_segment <- rep(all_members,
                      each = length(all_members))
  to_segment <- rep(all_members,
                    times = length(all_members))

  # Calculate passability between each pair of segments
  pass <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Gather DCI inputs and calculate sub-segmental DCI
  DCIs_sub <- data.frame(from = from_segment,
                         to = to_segment,
                         pass)
  DCIs_sub <- dplyr::left_join(DCIs_sub, seg_weights, by = c("from" = "member.label"))
  names(DCIs_sub)[names(DCIs_sub) == "segweight"] <- "from_len"
  DCIs_sub <- dplyr::left_join(DCIs_sub, seg_weights, by = c("to" = "member.label"))
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
  message(paste0("potamodromous DCI: ", DCI_glob))

  # Return DCIs summary
  return(DCIs)

}

#' Calculate non-thresholded diadromous DCI
#'
#' @inheritParams calculate_dci_pot
#' @param outlet_seg An integer indicating the membership label of the outlet segment
#'
#' @return A data frame which holds raw and relative DCI scores for each segment.
#'
#' @keywords internal
calculate_dci_dia <- function(all_members, net_nodes, seg_weights, outlet_seg){

  # Determine segment pairs
  from_segment <- rep(outlet_seg, times = length(all_members))
  to_segment <- all_members

  # Calculate passability between each pair of segments
  pass <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Gather DCI inputs and calculate sub-segmental DCI
  DCIs_sub <- data.frame(from = from_segment,
                         to = to_segment,
                         pass)
  DCIs_sub <- dplyr::left_join(DCIs_sub, seg_weights, by = c("from" = "member.label"))
  names(DCIs_sub)[names(DCIs_sub) == "segweight"] <- "from_len"
  DCIs_sub <- dplyr::left_join(DCIs_sub, seg_weights, by = c("to" = "member.label"))
  names(DCIs_sub)[names(DCIs_sub) == "segweight"] <- "to_len"
  DCIs_sub$DCIs <- DCIs_sub$from_len * DCIs_sub$to_len * DCIs_sub$pass * 100

  # Group DCI results by from segment to obtain segmental DCI
  DCIs <- DCIs_sub %>%
    dplyr::group_by(.data$to) %>%
    dplyr::summarise(DCI = sum(.data$DCIs))
  names(DCIs)[names(DCIs) == "to"] <- "segment"
  DCI_glob <- sum(DCIs$DCI)
  DCIs$DCI_rel <- DCIs$DCI / DCI_glob * 100
  DCIs <- as.data.frame(DCIs)

  # Print global dci
  message(paste0("diadromous DCI: ", DCI_glob))

  # Return DCIs summary
  return(DCIs)

}

#' Calculate thresholded potamodromous DCI
#'
#' @inheritParams calculate_dci_pot
#' @inheritParams calculate_dci
#' @param weighted A logical value indicating whether river lengths in seg_weights are weighted.
#' @param totweight The total length or weighted length of the whole network.
#'
#' @return A data frame which holds raw and relative DCI scores for each segment.
#'
#' @keywords internal
calculate_dci_pot_thresh <- function(net, all_members, net_nodes, seg_weights, weighted, threshold, totweight){

  # Determine segment pairs
  from_segment <- rep(all_members,
                      each = length(all_members))
  to_segment <- rep(all_members,
                    times = length(all_members))

  # Calculate segment-segment distance between each pair
  distances <- mapply(gather_dist, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Remove pairs with distances larger than the threshold
  discard_pairs <- which(distances > threshold)
  if(length(discard_pairs) != 0){
    from_segment <- from_segment[-discard_pairs]
    to_segment <- to_segment[-discard_pairs]
    distances <- distances[-discard_pairs]
  }

  # Calculate passability between remaining pairs
  perms <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Calculate DCI
  DCIs <- mapply(gather_dci, from_segment, to_segment, distances, perms, MoreArgs = list(net = net, nodes = net_nodes, seg_weights, threshold, totweight, weighted))
  DCI_glob <- sum(DCIs, na.rm = TRUE)

  # Print global dci
  message(paste0("potamodromous DCI with distance limit of ", threshold, ": ", DCI_glob))

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
#' @inheritParams calculate_dci_pot_thresh
#' @param outlet_seg An integer indicating the membership label of the outlet segment
#'
#' @keywords internal
calculate_dci_dia_thresh <- function(net, all_members, net_nodes, seg_weights, weighted, threshold, totweight, outlet_seg){

  # Determine segment pairs
  from_segment <- rep(outlet_seg, times = length(all_members))
  to_segment <- all_members

  # Remove pairs of segments further than threshold
  distances <- mapply(gather_dist, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Remove pairs with distances larger than the threshold
  discard_pairs <- which(distances > threshold)
  if(length(discard_pairs) != 0){
    from_segment <- from_segment[-discard_pairs]
    to_segment <- to_segment[-discard_pairs]
    distances <- distances[-discard_pairs]
  }

  # Calculate passability between remaining pairs
  perms <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Calculate DCI
  DCIs <- mapply(gather_dci, from_segment, to_segment, distances, perms, MoreArgs = list(net = net, nodes = net_nodes, seg_weights, threshold, totweight, weighted))
  DCI_glob <- sum(DCIs)

  # Print global dci
  message(paste0("diadromous DCI with distance limit of ", threshold, ": ", DCI_glob))

  # Return result
  DCI_res <- data.frame(from_segment, to_segment, DCIs)
  DCI_res <- DCI_res[c("to_segment","DCIs")]
  names(DCI_res)[names(DCI_res) == "to_segment"] <- "segment"
  names(DCI_res)[names(DCI_res) == "DCIs"] <- "DCI"
  DCI_res$DCI_rel <- DCI_res$DCI / DCI_glob * 100
  return(DCI_res)
}


#' Calculate sub-segmental DCI component between a single pair of segments
#'
#' @inheritParams calculate_dci_pot_thresh
#' @param from The origin segment's membership label.
#' @param to The destination segment's membership label.
#' @param distance The distance, in map units, between the two segments as calculated by \code{\link{gather_dist}}.
#' @param pass The passability between the two segments as calculated by \code{\link{gather_perm}}.
#' @param nodes An \code{\link{sf}} object of the nodes of the \code{\link{river_net}} object with river attributes joined.
#'
#' @return The sub-segmental DCI component between given pair of segments.
#'
#' @keywords internal
gather_dci <- function(net, from, to, distance, pass, nodes, seg_weights, threshold, totweight, weighted){

  # Case when from and to segment are the same
  if(from == to){

    seg_length <- seg_weights[seg_weights$member.label == from,]$segweight
    DCI <- seg_length/totweight * seg_length/totweight * 1 * 100
    return(DCI)

  }

  # Extract sinks and barriers
  sinks_bars <- subset(nodes, nodes$type %in% c("outlet", "barrier"))

  # Get from segment local outlet
  from_sink <- sinks_bars[sinks_bars$member.label == from,]$node.label

  # Get to segment local outlet
  to_sink <- sinks_bars[sinks_bars$member.label == to,]$node.label

  # Get path between sinks
  path <- path_between(from_sink, to_sink)

  # Join node attributes to nodes on path
  full_path <- nodes[nodes$node.label %in% path,]

  # Determine final entrance and exit nodes for pair of segments
  # If from segment is upstream of to segment
  if(length(unlist(from_sink)) > length(unlist(to_sink))){

    # Set exit to one node upstream of from outlet, extract row index
    exit_label <- list(append(unlist(from_sink), FALSE))
    exit_label <- as.integer(rownames(nodes[nodes$node.label %in% exit_label,]))

    # If from segment is downstream of to segment
  } else {

    # Select most downstream barrier as exit, extract row index
    barriers <- full_path[full_path$type == "barrier",]
    barriers$depth <- unlist(lapply(barriers$node.label, length))
    exit_label <- as.integer(rownames(barriers[which.min(barriers$depth),]))
  }

  # Calculate length remaining after segment-segment distance
  rem_length <- threshold - distance

  # Gather neighbourhood around exit (from segment)
  weights <- as.data.frame(activate(net, edges))
  weights <- weights$riv_length
  target <- seq_len(igraph::gorder(net))
  source <- exit_label
  dist <- igraph::distances(graph = net, v = source, to = target,
                             mode = "all", weights = weights, algorithm = "automatic")
  neighb_length <- sum(dist[dist <= rem_length])

  if(weighted){
    # Calculate length of from neighbourhood
    from_length <- sum(nodes[nodes$member.label == from,]$riv_length * nodes[nodes$member.label == from,]$riv_weight, na.rm = TRUE)
    # Calculate length of to neighbourhood
    to_length <- sum(nodes[nodes$member.label == to,]$riv_length * nodes[nodes$member.label == to,]$riv_weight, na.rm = TRUE)

  } else{
    # Calculate length of from neighbourhood
    from_length <- sum(nodes[nodes$member.label == from,]$riv_length)
    # Calculate length of to neighbourhood
    to_length <- sum(nodes[nodes$member.label == to,]$riv_length)
  }

  # Calculate relative neighbourhood length for segment
  from_length <- sum(nodes[nodes$member.label == from,]$riv_length)
  neighb_rel <- neighb_length / from_length

  # Calculate sub-segmental DCI for pair of segments
  DCI <- from_length/totweight * to_length/totweight * neighb_rel * pass * 100
  return(DCI)

}

#' Gather total distance between two nodes
#'
#' @param from The from node's label
#' @param to The to node's label
#' @param nodes An \code{\link{sf}} object of the nodes of the \code{\link{river_net}} object with river attributes joined.
#'
#' @return The distance, in map units, between the two given segments.
#'
#' @keywords internal
gather_dist <- function(from, to, nodes){

  # Extract sinks and barriers
  sinks_bars <- subset(nodes, nodes$type %in% c("outlet", "barrier"))

  # Get from segment local outlet
  from_sink <- sinks_bars[sinks_bars$member.label == from,]$node.label

  # Get to segment local outlet
  to_sink <- sinks_bars[sinks_bars$member.label == to,]$node.label

  # Get outlet-to-outlet path between segments
  path <- path_between(from_sink, to_sink)

  # Join member labels for nodes on path
  full_path <- nodes %>%
    dplyr::filter(.data$node.label %in% path)
  full_path <- full_path[c("node.label", "member.label")]

  # Case when segments are neighbours
  if(length(unique(full_path$member.label)) == 2){
    return(0)
  }

  # Trim path to only segment-segment distance
  path_ss <- full_path[!(full_path$member.label %in% c(from, to)),]$node.label
  #'
  #' @param from The from node's label
  # Get segment-segment path distance
  dist_ss <- sum(nodes %>%
                   dplyr::filter(.data$node.label %in% path_ss) %>%
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
gather_perm <- function(from, to, nodes){

  # Condition when from and to are the same
  if(from == to){
    return(1)
  }

  # Extract sinks and barriers
  sinks_bars <- subset(nodes, nodes$type %in% c("outlet", "barrier"))

  # Get from segment local outlet
  from_sink <- sinks_bars[sinks_bars$member.label == from,]$node.label

  # Get to segment local outlet
  to_sink <- sinks_bars[sinks_bars$member.label == to,]$node.label

  # Get path between segments
  path <- path_between(from_sink, to_sink)

  # Gather permeabilities across path
  path_perm <- prod(nodes %>%
                      dplyr::filter(.data$node.label %in% path) %>%
                      dplyr::pull(.data$pass))

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
path_between <- function(s1, s2){
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
path_to_root <- function(seg){
  # Prepare input vectors
  path <- rep(seg, each = length(unlist(seg)))
  len <- length(unlist(seg)):1
  # Sequentially remove final element from path until 1 left
  path_out <- mapply(function(x, y) unlist(x)[1:y],
                     path, len, SIMPLIFY = TRUE)
  # Return list of nodes to the root
  return(path_out)
}
