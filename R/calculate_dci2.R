# Since nodes hold the edge length of their downstream node, barriers should be removed when calculating the DCI since that length belongs to another segment. Might need to reconsider which segment these should be labelled for. Or I just assume the amount of error is well spread out and not significant on DCI results.

# Back to back barriers don't seem to hold any length, should be corrected

calculate_dci <- function(net, form = NULL, threshold = NULL, weighted = FALSE, sites = NULL){

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
    return(DCIs)

  # If distance threshold is supplied
  } else {

    # Potamodromous case
    if(form == "potamodromous") DCIs <- calculate_dci_pot_thresh(all_members, net_nodes, weighted, threshold, totweight)

    # Diadromous case
    if(form == "diadromous") DCIs <- calculate_dci_dia_thresh(all_members, net_nodes, weighted, threshold, totweight)

    # Return calculated DCI values
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
  DCI_glob <- sum(DCIs_sub)
  DCIs <- DCIs_sub %>%
    dplyr::group_by(from) %>%
    dplyr::summarise(DCI = sum(DCIs)) %>%
    dplyr::rename(segment = from) %>%
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
  DCI_glob <- sum(DCIs_sub)
  DCIs <- DCIs_sub %>%
    dplyr::group_by(to) %>%
    dplyr::summarise(DCI = sum(DCIs)) %>%
    dplyr::rename(segment = to) %>%
    dplyr::mutate(DCI_rel = DCI/DCI_glob*100) %>%
    as.data.frame()

  if(!is.null(sites)){

    # Extract site nodes and join DCI values
    DCIs <- net_nodes %>%
      dplyr::filter(type == sites) %>%
      dplyr::select(id, member.label, geometry.x) %>%
      dplyr::rename("geometry" = "geometry.x") %>%
      dplyr::left_join(DCIs, by = c("member.label" = "segment")) %>%
      dplyr::select(-member.label)

    # Convert DCI results to sf object
    DCIs <- sf::st_as_sf(DCIs, sf_column_name = "geometry")
  }

  # Return DCIs summary
  return(DCIs)

}

calculate_dci_pot_thresh <- function(all_members, net_nodes, weighted, threshold, totweight){

  # Determine segment pairs
  from_segment <- rep(all_members,
                      each = length(all_members))
  to_segment <- rep(all_members,
                    times = length(all_members))

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
  DCIs <- mapply(gather_dci, from_segment, to_segment, distances, perms, MoreArgs = list(nodes = net_nodes, threshold = threshold, totweight = totweight))
  DCI_glob <- sum(DCIs)

  # Return result
  DCI_res <- data.frame(from_segment, to_segment, DCIs)
  DCI_res <- DCI_res %>%
    dplyr::group_by(segment = from_segment) %>%
    dplyr::summarise(DCI = sum(DCIs)) %>%
    dplyr::mutate(DCI_rel = DCI/DCI_glob*100) %>%
    as.data.frame()
  return(DCI_res)
}

calculate_dci_dia_thresh <- function(all_members, net_nodes, weighted, threshold, totweight){

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
  DCIs <- mapply(gather_dci, from_segment, to_segment, distances, perms, MoreArgs = list(nodes = net_nodes, threshold = threshold, totweight = totweight))
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

gather_dci <- function(from, to, distance, perm, nodes, threshold, totweight){

  # Case when from and to segment are the same
  if(from == to){

    # Calculate average segment length
    seg_nodes <- nodes[nodes$member.label == from,]
    seg_nodes <- seg_nodes$node.label
    # If there are over 50 nodes, select every 10th node
    if(length(seg_nodes) >= 50){
      seg_nodes <- seg_nodes[seq(0, length(seg_nodes), 10)]
    }
    loc_length <- vector("double", length(seg_nodes))
    for(i in 1:length(seg_nodes)){
      loc_length[i] <- gather_local_length(seg_nodes[i], from, threshold, nodes, net)
    }
    len_ave <- mean(loc_length)

    # Convert to relative from segment length
    from_rel <- len_ave / totweight

    # Convert to relative to segment length
    to_rel <- len_ave / totweight

    # Calculate and return DCI value
    return(from_rel * to_rel * 1 * 100)

  }

  # Extract sinks and barriers
  sinks_bars <- subset(nodes, nodes$type %in% c("Sink", "Barrier"))

  # Get from segment local sink
  from_sink <- sinks_bars[sinks_bars$member.label == from,]$node.label

  # Get to segment local sink
  to_sink <- sinks_bars[sinks_bars$member.label == to,]$node.label

  # Get path between segments
  path <- path_between(from_sink, to_sink)

  # Join member labels for nodes on path
  full_path <- nodes[nodes$node.label %in% path,]
  full_path <- subset(full_path, select = c(node.label, member.label, type))

  # If from segment is upstream of to segment
  if(length(unlist(from_sink)) > length(unlist(to_sink))){

    # Set exit to one node upstream of from sink
    exit_label <- list(append(unlist(from_sink), FALSE))
    exit <- sf::st_geometry(nodes[nodes$node.label %in% exit_label,])

    # Select most downstream barrier as entrance
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

  # Calculate max travelable distance in from & to segment
  max_travel_from <- net %>%
    dplyr::mutate(trav = tidygraph::node_distance_from(sf::st_nearest_feature(exit, net), mode = "all", weights = riv_length)) %>%
    dplyr::filter(member.label == from) %>%
    dplyr::pull(trav) %>%
    max()

  max_travel_to <- net %>%
    dplyr::mutate(trav = tidygraph::node_distance_from(sf::st_nearest_feature(entrance, net), mode = "all", weights = riv_length)) %>%
    dplyr::filter(member.label == to) %>%
    dplyr::pull(trav) %>%
    max()

  # If there isn't enough length in one of the two segments adjust respective travel distances
  rem_length <- threshold - distance
  # If both segments don't have enough length
  if(max_travel_from < rem_length/2 & max_travel_to < rem_length/2){
    thresh_from <- max_travel_from
    thresh_to <- max_travel_to
  }
  # If only from segment doesn't have enough length
  else if(max_travel_from < rem_length/2){
    thresh_from <- max_travel_from
    thresh_to <- rem_length - max_travel_from
  }
  # If to segment doesn't have enough length
  else if(max_travel_to < rem_length/2){
    thresh_to <- max_travel_to
    thresh_from <- rem_length - max_travel_to
  }
  # If both segments have enough length
  else{
    thresh_to <- rem_length/2
    thresh_from <- rem_length/2
  }

  # Calculate length of from neighbourhood
  from_length <- gather_local_length(exit_label, from, threshold, nodes, net)

  # Calculate length of to neighbourhood
  to_length <- gather_local_length(entrance_label, to, threshold, nodes, net)

  # Calculate sub-segmental DCI for pair of segments
  DCI <- from_length/totweight * to_length/totweight * perm * 100
  return(DCI)

}

gather_local_length <- function(label, member, threshold, nodes, net){

  # Subset network
  net <- net %>%
    tidygraph::activate(nodes) %>%
    dplyr::filter(member.label == member)

  # If subset network is empty return 0
  if(nrow(net %>% activate(edges) %>% as.data.frame()) == 0){
    return(0)
  }

  # Identify node associated with label
  node <- sf::st_geometry(nodes[nodes$node.label %in% label,])

  # Identify local neighbourhood around node
  neighb <- net %>%
    dplyr::filter(tidygraph::node_distance_from(sf::st_nearest_feature(node, net), mode = "all", weights = riv_length) <= threshold) %>%
    tidygraph::activate(edges) %>%
    dplyr::pull(riv_length)

  # Calculate and return length
  return(sum(neighb))

}
