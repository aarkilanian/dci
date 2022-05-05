calculate_dci <- function(net, form = NULL, threshold = NULL, weighted = FALSE, points = NULL){

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
    dplyr::left_join(net_edges, by = c("nodeID" = "from"))


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
      dplyr::summarise(segweight = sum(weighted_len, na.rm = TRUE)) %>%
      dplyr::filter(segweight != 0)
  }

  # If no distance threshold is supplied
  if(is.null(threshold)){

    # Gather member IDs
    all_members <- seg_weights$member.label

    # Calculate total weight of network
    totweight <- sum(seg_weights$segweight)

    # Calculate relative length of segments
    seg_weights$segweight <- seg_weights$segweight / totweight

    # Potamodromous case
    if(form == "potamodromous") DCIs <- calculate_dci_pot(all_members, seg_weights, net, net_nodes, net_edges, points)

    # Diadromous case
    if(form == "diadromous") DCIs <- calculate_dci_dia(all_members, seg_weights, net, net_nodes, net_edges, points)

    # Return calculated DCI values
    return(DCIs)

  # If distance threshold is supplied
  } else {

    # Gather member IDs
    all_members <- seg_weights$member.label

    # Calculate total weight of network
    totweight <- sum(seg_weights$segweight)

    # Potamodromous case
    if(form == "potamodromous") DCIs <- calculate_dci_pot_thresh(all_members, weighted, threshold, totweight, net_nodes, points)

    # Diadromous case
    if(form == "diadromous") DCIs <- calculate_dci_dia_thresh(all_members, weighted, threshold, totweight, net_nodes, points)

    # Return calculated DCI values
    return(DCIs)
  }

}

calculate_dci_pot <- function(all_members, net_nodes, seg_weights, points = NULL){

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

  if(!is.null(points)){
    # Join relevent segment's DCI score to the points supplied
  }

  # Return DCIs summary
  return(DCIs)

}

calculate_dci_dia <- function(all_members, net_nodes, seg_weights, points = NULL){

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

  if(!is.null(points)){
    # Join relevent segment's DCI score to the points supplied
  }

  # Return DCIs summary
  return(DCIs)

}

calculate_dci_pot_thresh <- function(all_members, net_nodes, weighted, threshold, totweight, points = NULL){

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

  # Measure length for each segment within each pair. Might be easier to calculate DCI pair by pair this way with an apply type implementation
  # However, maybe this would make things more confusing

}

calculate_dci_dia_thresh <- function(all_members, net_nodes, weighted, threshold, totweight, points = NULL){


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

  # Identify segment-segment path
  ## If first node after from sink is from another segment then from segment is upstream and the sink is the starting point
  ## For other segment find barrier closest to sink and remove last binary digit

  # Calculate distance of path
  path_dist <- sum(nodes %>%
                     dplyr::filter(node.label %in% path) %>%
                     dplyr::pull(riv_length))

  # Return segment-segment distance
  return(path_dist)

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
