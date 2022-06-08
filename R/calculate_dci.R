# TODO Use mapply from with non-threshold DCI calculations similar to thresholded method

#' Calculate different forms of the DCI for a \code{\link{river_net}} object
#'
#' @param net A \code{\link{river_net}} object.
#' @param form A string specifying the form of the DCI to calculate: either "potamodromous", "catadromous", or "all".
#' @param perm The name of a column in the nodes table of net which holds the numeric permeability of nodes. If none is specified all barriers are automatically considered to have 0 permeability.
#' @param threshold An optional numeric value specifying a dispersal limit in map units. If NULL, the default, no limit is considered.
#' @param weight The name of column in the edges tables of net which holds numeric weights to be applied to river lengths. If none is specified, the DCI is calculated only with river lengths.
#' @param sites The name of a type of nodes in the node table of net. If specified, DCI results will be calculated at these sites and returned. If not specified, the DCI results will be reported on the rivers. See details for more.
#'
#' @return A \code{\link{sf}} object of the rivers from the provided \code{\link{river_net}} object with new columns speciying the segmental DCI values at each river location. If sites is not \code{NULL}, a \code{\link{sf}} object of the site points with their associated DCI scores.
#' @export
#'
#' @examples
calculate_dci <- function(net, form, perm = NULL, threshold = NULL, weight = NULL, sites = NULL){

  # No valid network
  if(!("river_net" %in% class(net))){
    stop("A valid river_net object is required.")
  }
  # No valld form
  if(is.null(form)){
    stop("A valid form of the DCI must be requested.")
  }
  if(!(form %in% c("potamodromous", "diadromous"))){
    stop("A valid form of the DCI must be requested.")
  }

  # Check that weight is valid
  if(!(is.null(weight))){
    if(!(is.numeric(rivers[[weight]]))) stop("Weight values must be numeric.")
    # Check that weight is between 0 and 1
    if(any(abs(rivers[[weight]]) > 1)) stop("Weight values must be between 0 and 1.")
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

  # Extract edges
  net_edges <- net %>%
    activate(edges) %>%
    as.data.frame()
  # No valid weighting
  if(weighted & !("riv_weight" %in% names(net_edges))){
    stop("No valid weighting found in river network.")
  }

  # Extract nodes
  net_nodes <- net %>%
    activate(nodes) %>%
    as.data.frame()
  # Identify sink
  if(form == "diadromous"){
    net_sink <- net_nodes[net_nodes$type == "Sink",]$member.label
    if(length(net_sink) == 0){
      stop("No valid sink found in river network.")
    }
  }
  # Move edge weights to nodes
  # Weights from edges associated w/ upstream nodes
  net_nodes <- net_nodes %>%
    dplyr::left_join(net_edges, by = c("nodeID" = "from")) %>%
    sf::st_as_sf(sf_column_name = "geometry.x")
  # Set sink length to 0
  net_nodes[net_nodes$type == "Sink",]$riv_length <- 0

  if(weighted){
    # Calculate total weighted length of segments
    seg_weights <- net_nodes %>%
      as.data.frame() %>%
      dplyr::mutate(weighted_len = riv_length * riv_weight) %>%
      dplyr::group_by(member.label) %>%
      dplyr::summarise(segweight = sum(weighted_len, na.rm = TRUE)) %>%
      # Remove members with 0 length
      dplyr::filter(segweight != 0)
  } else {
    # Calculate segment total lengths
    seg_weights <- net_nodes %>%
      as.data.frame() %>%
      dplyr::group_by(member.label) %>%
      dplyr::summarise(segweight = sum(riv_length, na.rm = TRUE)) %>%
      dplyr::filter(segweight != 0)
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
    if(form == "diadromous") DCIs <- calculate_dci_dia(all_members, net_nodes, seg_weights, net_sink)

    # If sites are supplied, associate results to them
    if(!is.null(sites)){
      # Isolate site nodes
      site_nodes <- net_nodes[net_nodes$type == sites,]
      # Join results based on segment membership
      DCIs <- dplyr::left_join(site_nodes, DCIs, by = c("member.label" = "segment"))
    }

    # Return calculated DCI values
    DCIs <- structure(DCIs, class = c("dci.results", class(DCIs)))
    return(DCIs)

  # If distance threshold is supplied
  } else {

    # Potamodromous case
    if(form == "potamodromous") DCIs <- calculate_dci_pot_thresh(all_members, net_nodes, seg_weights, weighted, threshold, totweight)

    # Diadromous case
    if(form == "diadromous") DCIs <- calculate_dci_dia_thresh(all_members, net_nodes, seg_weights, weighted, threshold, totweight)

    # If sites are supplied, associate results to them
    if(!is.null(sites)){
      # Isolate site nodes
      site_nodes <- net_nodes[net_nodes$type == sites,]
      # Join results based on segment membership
      DCIs <- dplyr::left_join(site_nodes, DCIs, by = c("member.label" = "segment"))
    }

    # Return calculated DCI values
    DCIs <- structure(DCIs, class = c("dci.results", class(DCIs)))
    return(DCIs)
  }

}

calculate_dci_pot <- function(all_members, net_nodes, seg_weights){

  # Determine segment pairs
  from_segment <- rep(all_members,
                      each = length(all_members))
  to_segment <- rep(all_members,
                    times = length(all_members))

  # Calculate permeability between each pair of segments
  perm <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Gather DCI inputs and calculate sub-segmental DCI
  DCIs_sub <- data.frame(from = from_segment,
                         to = to_segment,
                         perm) %>%
    dplyr::left_join(seg_weights, by = c("from" = "member.label")) %>%
    dplyr::rename(from_len = segweight) %>%
    dplyr::left_join(seg_weights, by = c("to" = "member.label")) %>%
    dplyr::rename(to_len = segweight) %>%
    dplyr::mutate(DCIs = from_len * to_len * perm * 100)

  # Group DCI results by from segment to obtain segmental DCI
  DCIs <- DCIs_sub %>%
    dplyr::group_by(from) %>%
    dplyr::summarise(DCI = sum(DCIs)) %>%
    dplyr::rename(segment = from)
  DCI_glob <- sum(DCIs$DCI)
  DCIs <- DCIs %>%
    dplyr::mutate(DCI_rel = DCI/DCI_glob*100) %>%
    as.data.frame()

  # Return DCIs summary
  return(DCIs)

}

calculate_dci_dia <- function(all_members, net_nodes, seg_weights, net_sink){

  # Identify sink segment
  sink_seg <- net %>%
    activate(nodes) %>%
    dplyr::filter(type == "Sink") %>%
    dplyr::pull(member.label)

  # Determine segment pairs
  from_segment <- rep(sink_seg, times = length(all_members))
  to_segment <- all_members

  # Calculate permeability between each pair of segments
  perm <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Gather DCI inputs and calculate sub-segmental DCI
  DCIs_sub <- data.frame(from = from_segment,
                         to = to_segment,
                         perm) %>%
    dplyr::left_join(seg_weights, by = c("from" = "member.label")) %>%
    dplyr::rename(from_len = segweight) %>%
    dplyr::left_join(seg_weights, by = c("to" = "member.label")) %>%
    dplyr::rename(to_len = segweight) %>%
    dplyr::mutate(DCIs = from_len * to_len * perm * 100)

  # Group DCI results by from segment to obtain segmental DCI
  DCIs <- DCIs_sub %>%
    dplyr::group_by(to) %>%
    dplyr::summarise(DCI = sum(DCIs)) %>%
    dplyr::rename(segment = to)
  DCI_glob <- sum(DCIs$DCI)
  DCIs <- DCIs %>%
    dplyr::mutate(DCI_rel = DCI/DCI_glob*100) %>%
    as.data.frame()

  # Return DCIs summary
  return(DCIs)

}

calculate_dci_pot_thresh <- function(all_members, net_nodes, seg_weights, weighted, threshold, totweight){

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

  # Calculate permeability between remaining pairs
  perms <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Calculate DCI
  DCIs <- mapply(gather_dci, from_segment, to_segment, distances, perms, MoreArgs = list(nodes = net_nodes, seg_weights, threshold, totweight, weighted))
  DCI_glob <- sum(DCIs, na.rm = TRUE)

  # Return result
  DCI_res <- data.frame(from_segment, to_segment, DCIs)
  DCI_res <- DCI_res %>%
    dplyr::group_by(segment = from_segment) %>%
    dplyr::summarise(DCI = sum(DCIs, na.rm = TRUE)) %>%
    dplyr::mutate(DCI_rel = DCI/DCI_glob*100) %>%
    as.data.frame()
  return(DCI_res)
}

calculate_dci_dia_thresh <- function(all_members, net_nodes, seg_weights, weighted, threshold, totweight){

  # Identify sink segment
  sink_seg <- net %>%
    activate(nodes) %>%
    dplyr::filter(type == "Sink") %>%
    dplyr::pull(member.label)

  # Determine segment pairs
  from_segment <- rep(sink_seg, times = length(all_members))
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

  # Calculate permeability between remaining pairs
  perms <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Calculate DCI
  DCIs <- mapply(gather_dci, from_segment, to_segment, distances, perms, MoreArgs = list(nodes = net_nodes, threshold, totweight, weighted))
  DCI_glob <- sum(DCIs)

  # Return result
  DCI_res <- data.frame(from_segment, to_segment, DCIs)
  DCI_res <- DCI_res %>%
    dplyr::select(-from_segment) %>%
    dplyr::rename(segment = to_segment, DCI = DCIs) %>%
    dplyr::mutate(DCI_rel = DCI/DCI_glob*100)
  return(DCI_res)
}

gather_dist <- function(from, to, nodes){

  # Extract sinks and barriers
  sinks_bars <- subset(nodes, nodes$type %in% c("Sink", "Barrier"))

  # Get from segment local sink
  from_sink <- sinks_bars[sinks_bars$member.label == from,]$node.label

  # Get to segment local sink
  to_sink <- sinks_bars[sinks_bars$member.label == to,]$node.label

  # Get sink-to-sink path between segments
  path <- path_between(from_sink, to_sink)

  # Join member labels for nodes on path
  full_path <- nodes %>%
    dplyr::filter(node.label %in% path) %>%
    dplyr::select(node.label, member.label)

  # Case when segments are neighbours
  if(length(unique(full_path$member.label)) == 2){
    return(0)
  }

  # Trim path to only segment-segment distance
  path_ss <- full_path[!(full_path$member.label %in% c(from, to)),]$node.label

  # Get segment-segment path distance
  dist_ss <- sum(nodes %>%
                   dplyr::filter(node.label %in% path_ss) %>%
                   dplyr::pull(riv_length), na.rm = TRUE)

  # Return segment-segment distance
  return(dist_ss)

}

gather_perm <- function(from, to, nodes){

  message(paste0("From ", from, " to ", to))

  # Condition when from and to are the same
  if(from == to){
    return(1)
  }

  # Extract sinks and barriers
  sinks_bars <- subset(nodes, nodes$type %in% c("Sink", "Barrier"))

  # Get from segment local sink
  from_sink <- sinks_bars[sinks_bars$member.label == from,]$node.label

  # Get to segment local sink
  to_sink <- sinks_bars[sinks_bars$member.label == to,]$node.label

  # Get path between segments
  path <- path_between(from_sink, to_sink)

  # Gather permeabilities across path
  path_perm <- prod(nodes %>%
                      dplyr::filter(node.label %in% path) %>%
                      dplyr::pull(perm))

  # Return permeability between segments
  return(path_perm)

}

gather_dci <- function(from, to, distance, perm, nodes, seg_weights, threshold, totweight, weighted){

  # Case when from and to segment are the same
  if(from == to){

    seg_length <- seg_weights[seg_weights$member.label == from,]$segweight
    DCI <- seg_length/totweight * seg_length/totweight * 1 * 100
    return(DCI)

  }

  # Extract sinks and barriers
  sinks_bars <- subset(nodes, nodes$type %in% c("Sink", "Barrier"))

  # Get from segment local sink
  from_sink <- sinks_bars[sinks_bars$member.label == from,]$node.label

  # Get to segment local sink
  to_sink <- sinks_bars[sinks_bars$member.label == to,]$node.label

  # Get path between sinks
  path <- path_between(from_sink, to_sink)

  # Join node attributes to nodes on path
  full_path <- nodes[nodes$node.label %in% path,]

  # Determine final entrance and exit nodes for pair of segments
  # If from segment is upstream of to segment
  if(length(unlist(from_sink)) > length(unlist(to_sink))){

    # Set exit to one node upstream of from sink, extract geometry
    exit_label <- list(append(unlist(from_sink), FALSE))
    exit <- sf::st_geometry(nodes[nodes$node.label %in% exit_label,])

    # Select most downstream barrier as entrance, extract geometry
    barriers <- full_path[full_path$type == "Barrier",]
    barriers$depth <- unlist(lapply(barriers$node.label, length))
    entrance_label <- barriers[which.min(barriers$depth),]$node.label
    entrance <- sf::st_geometry(nodes[nodes$node.label %in% entrance_label,])

  # If from segment is downstream of to segment
  } else {

    # Set entrance to one node upstream of to sink, extract geometry
    entrance_label <- list(append(unlist(to_sink), FALSE))
    entrance <- sf::st_geometry(nodes[nodes$node.label %in% entrance_label,])

    # Select most downstream barrier as exit, extract geometry
    barriers <- full_path[full_path$type == "Barrier",]
    barriers$depth <- unlist(lapply(barriers$node.label, length))
    exit_label <- barriers[which.min(barriers$depth),]$node.label
    exit <- sf::st_geometry(nodes[nodes$node.label %in% exit_label,])
  }

  # Calculate length remaining after segment-segment distance
  rem_length <- threshold - distance

  # Gather neighbourhood around exit (from segment)
  neighb <- net %>%
    dplyr::filter(tidygraph::node_distance_from(sf::st_nearest_feature(exit, net), mode = "all", weights = riv_length) <= threshold) %>%
    dplyr::filter(member.label == from)
  neighb_length <- sum(neighb$riv_length)

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
  DCI <- from_length/totweight * to_length/totweight * neighb_rel * perm * 100
  return(DCI)

}

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

#' @export
# print.dci.results <- function(x, ...){
#   cat("a")
#   invisible(x)
# }
