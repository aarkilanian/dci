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

  if(weighted){
    # Calculate total weighted length of segments
    seg_weights <- net_nodes %>%
      dplyr::mutate(weighted_len = riv_length * riv_weight) %>%
      dplyr::group_by(member.label) %>%
      dplyr::summarise(segweight = sum(weighted_len, na.rm = TRUE)) %>%
      # Remove members with 0 length
      dplyr::filter(segweight != 0)
  } else {
    # Calculate segment total lengths
    seg_weights <- net_nodes %>%
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
    if(form == "potamodromous") DCIs <- calculate_dci_pot(all_members, net_nodes, seg_weights, sites)

    # Diadromous case
    if(form == "diadromous") DCIs <- calculate_dci_dia(all_members, net_nodes, seg_weights, net_sink, sites)

    # Return calculated DCI values
    return(DCIs)

  # If distance threshold is supplied
  } else {

    # Potamodromous case
    if(form == "potamodromous") DCIs <- calculate_dci_pot_thresh(all_members, net_nodes, weighted, threshold, totweight, sites)

    # Diadromous case
    if(form == "diadromous") DCIs <- calculate_dci_dia_thresh(all_members, weighted, threshold, totweight, net_nodes, sites)

    # Return calculated DCI values
    return(DCIs)
  }

}

calculate_dci_pot <- function(all_members, net_nodes, seg_weights, sites = NULL){

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
    dplyr::summarise(DCIs = sum(DCIs)) %>%
    dplyr::rename(segment = from)

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

calculate_dci_dia <- function(all_members, net_nodes, seg_weights, net_sink, sites = NULL){

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
    dplyr::summarise(DCIs = sum(DCIs)) %>%
    dplyr::rename(segment = to)

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

calculate_dci_pot_thresh <- function(all_members, net_nodes, weighted, threshold, totweight, sites = NULL){

  # Determine segment pairs
  from_segment <- rep(all_members,
                      each = length(all_members))
  to_segment <- rep(all_members,
                    times = length(all_members))

  # Remove pairs of segments further than threshold
  distances <- mapply(gather_dist, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Remove pairs with distances larger than the threshold
  discard_pairs <- which(distances > threshold)
  from_segment <- from_segment[-discard_pairs]
  to_segment <- to_segment[-discard_pairs]
  distances <- distances[-discard_pairs]

  # Calculate permeability between remaining pairs
  perm <- mapply(gather_perm, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

  # Calculate DCI
  DCIs <- mapply(gather_dci, from_segment, to_segment, distances, perm, MoreArgs = list(nodes = net_nodes, threshold = threshold))

  # Measure length for each segment within each pair. Might be easier to calculate DCI pair by pair this way with an apply type implementation
  # However, maybe this would make things more confusing

}

calculate_dci_dia_thresh <- function(all_members, net_nodes, weighted, threshold, totweight, sites = NULL){


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

gather_dci <- function(from, to, distance, perm, nodes, threshold){

  # Calculate remaining length to distribute to each segment
  len_rem <- (threshold - distance) / 2

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
  if(length(from_sink) > length(to_sink)){

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

    # Set exit to one node upstream of to sink
    entrance_label <- list(append(unlist(to_sink), FALSE))
    entrance <- sf::st_geometry(nodes[nodes$node.label %in% entrance_label,])

    # Select most downstream barrier as exit
    barriers <- full_path[full_path$type == "Barrier",]
    barriers$depth <- unlist(lapply(barriers$node.label, length))
    exit_label <- barriers[which.min(barriers$depth),]$node.label
    exit <- sf::st_geometry(nodes[nodes$node.label %in% exit_label,])

  }

  # Identify local neighbourhood around entrance and exit
  iso = net %>%
    dplyr::filter(tidygraph::node_distance_from(sf::st_nearest_feature(entrance, net), mode = "all", weights = weight) <= 50000)

  b <- new_net %>%
    activate(edges) %>%
    dplyr::pull(riv_length)

  a <- net %>%
    dplyr::mutate(distyy = node_distance_from(which(tidygraph::.N()$type == "Sink"), mode = "all", weights = riv_length))

 a <-  net %>%
    tidygraph::convert(sfnetworks::to_spatial_neighborhood(x = net, node = which(tidygraph::.N()$type == "Sink"), 5))

  a <- tidygraph::convert(net, sfnetworks::to_spatial_neighborhood, entrance, threshold = 500)

  # Gather thresholded segment total lengths


}
