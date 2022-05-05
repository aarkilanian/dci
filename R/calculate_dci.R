#' Calculate DCI for a \code{\link{river_net}} object
#'
#' @param net A \code{\link{river_net}} object, the river network over which the dendritic connectivity index will be calculated.
#'
#' @param form A character string, either "potamodromous" or "diadromous" specifying the form of the DCI to be calculated.
#'
#' @param threshold An optional double value, a distance threshold in meters to cosider while calculating the DCI
#'
#' @param weighted A logical value, if \code{FALSE}, the default, weighting will not be considered during DCI calculation. If \code{TRUE} the weighting specified during river import with \code{\link{import_rivers}} will be used when calculating the DCI.
#'
#' @param points A character string, the nodes in the \code{\link{river_net}} for which DCI values should be calculated. Defaults to NULL leading to DCI results by segment.
#'
#' @return A \code{\link{data.frame}} of segment-level DCI values.
#'
#' @export
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

  # If weighted
  if(weighted){

    # Calculate total weighted length of segments
    seg_weights <- net_nodes %>%
      dplyr::mutate(weighted_len = riv_length * riv_weight) %>%
      dplyr::group_by(member.label) %>%
      dplyr::summarise(segweight = sum(weighted_len, na.rm = TRUE)) %>%
      # Remove members with 0 length
      dplyr::filter(segweight != 0)

  # If not weighted
  } else{

    # Calculate total length of segments
    seg_weights <- net_nodes %>%
      dplyr::group_by(member.label) %>%
      dplyr::summarise(segweight = sum(riv_length, na.rm = TRUE)) %>%
      # Remove members with 0 length
      dplyr::filter(segweight != 0)
  }

  # Gather member IDs
  all_members <- seg_weights$member.label

  # Calculate total weight of network
  totweight <- sum(seg_weights$segweight)

  # Calculate relative length of segments
  seg_weights$segweight <- seg_weights$segweight / totweight

  # Potamodromous case
  if(form == "potamodromous") DCIs <- calculate_dci_pot(all_members, seg_weights, net, net_nodes, net_edges)

  # Diadromous case
  if(form == "diadromous") DCIs <- calculate_dci_dia(all_members, seg_weights, net, net_nodes, net_edges)

  # Return calculated DCI values
  return(DCIs)

}

calculate_dci_pot_thresh <- function(all_members, seg_weights, net, net_nodes, net_edges, threshold, points = NULL){

  # Determine segment pairs
  from_segment <- rep(all_members,
                      each = length(all_members))
  to_segment <- rep(all_members,
                    times = length(all_members))

  # Calculate permeability between each pair of segments
  perm <- mapply(gather_perm_thresh, from_segment, to_segment, MoreArgs = list(nodes = net_nodes))

}

calculate_dci_pot <- function(all_members, seg_weights, net, net_nodes, net_edges, threshold = NULL, points = NULL){

  # Determine segment pairs
  from_segment <- rep(all_members,
                      each = length(all_members))
  to_segment <- rep(all_members,
                    times = length(all_members))

  # Gather distance between segments if threshold not NULL
  if(!is.null(threshold)){

    # Identify source nodes
    net_leaves <- net %>%
      dplyr::mutate(type = ifelse(tidygraph::node_is_leaf() & type != "Sink", "Source", type))

    # Extract new nodes
    net_nodes_leaves <- net_leaves %>%
      sfnetworks::activate(nodes) %>%
      as.data.frame() %>%
      dplyr::left_join(net_edges, by = c("nodeID" = "from"))

    # Calculate distance between each pair of segments
    dists <- mapply(gather_dist, from_segment, to_segment, MoreArgs = list(nodes = net_nodes_leaves))

  }

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

  # Return DCIs summary
  return(DCIs)

}

calculate_dci_dia <- function(all_members, seg_weights, net, net_nodes, threshold = NULL, points = NULL){

  # Identify sink segment
  # TODO already calculated in parent function, pass argument
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

  # Return DCIs summary
  return(DCIs)

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

gather_perm_thresh <- function(from, to, nodes, threshold){

  if(from == to){
    #
  }

}

gather_dist <- function(from, to, nodes){

  # Condition when from and to are the same
  if(from == to){
    return(1)
  }

  # Extract sinks and barriers
  sinks_bars <- subset(nodes, nodes$type %in% c("Sink", "Barrier"))
  # Get to segment local sink
  to_sink <- sinks_bars[sinks_bars$member.label == to,]$node.label

  # Extract sources
  sources <- subset(nodes, nodes$type == "Source")
  # Get from segment sources
  from_sources <- sources[sources$member.label == from,]$node.label

  # Extract node labels of from segment
  from_nodes <- nodes[nodes$member.label == from,]$node.label

  # Gather all paths from sources to destination sink
  all_paths <- vector("list", length(from_sources))
  for(i in 1:length(from_sources)){
    paths <- path_between(from_sources[i], to_sink)
  }

  # Get distance of all paths
  all_dists <- list()
  for(i in 1:length(all_paths)){
    all_dists[i] <- nodes %>%
      dplyr::filter(node.label %in% all_paths[[i]]) %>%
      dplyr::summarise(dist = sum(riv_length, na.rm = TRUE)) %>%
      dplyr::pull(dist)
  }

  # Retrieve maximum distance
  max_dist <- max(unlist(all_dists))
  return(max_dist)

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
