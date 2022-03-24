calculate_dci <- function(net){

  # Extract edges
  net_edges <- net %>%
    sfnetworks::activate(edges) %>%
    as.data.frame() %>%
    dplyr::select(from, weight) %>%
    dplyr::mutate(weight = as.double(weight))

  # Extract nodes
  net_nodes <- net %>%
    sfnetworks::activate(nodes) %>%
    as.data.frame()

  # Move edge weights to nodes
  # Weights from edges associated w/ upstream nodes
  net_nodes <- net_nodes %>%
    dplyr::left_join(net_edges, by = c("nodeID" = "from"))

  # Gather member IDs
  all_members <- unique(net_nodes$member.label)

  # Calculate total weight of segments
  seg_weights <- net_nodes %>%
    dplyr::group_by(member.label) %>%
    dplyr::summarise(segweight = sum(weight, na.rm = TRUE)) %>%
    # Remove members with 0 length
    dplyr::filter(segweight != 0)

  # Case when no valid weights in network
  if(nrow(seg_weights) == 0){
    message("The selected network has no valid weights")
  }

  # Update member IDs list
  all_members <- seg_weights$member.label

  # Calculate total weight of network
  totweight <- sum(seg_weights$segweight)

  # Calculate relative length of segments
  seg_weights$segweight <- seg_weights$segweight / totweight

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

  # Return DCIs summa
  return(DCIs)

}

gather_perm <- function(from, to, nodes){

  # Condition when from and to are the same
  if(from == to){
    return(1)
  }

  # Get from segment local sink
  from_sink <- nodes %>%
    dplyr::filter(member.label == from) %>%
    dplyr::filter(type %in% c("Sink", "Barrier")) %>%
    dplyr::pull(node.label)

  # Get to segment local sink
  to_sink <- nodes %>%
    dplyr::filter(member.label == to) %>%
    dplyr::filter(type %in% c("Sink", "Barrier")) %>%
    dplyr::pull(node.label)

  # Get path between segments
  path <- path_between(from_sink, to_sink)

  # Gather permeabilities across path
  path_perm <- prod(nodes %>%
                      dplyr::filter(node.label %in% path) %>%
                      dplyr::pull(perm))

  # Return permeability between segments
  return(path_perm)

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