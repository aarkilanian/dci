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

  # If no distance threshold is supplied
  if(is.null(threshold)){

    # If weighting is supplied
    if(weighted){

      # Calculate total weighted length of segments
      seg_weights <- net_nodes %>%
        dplyr::mutate(weighted_len = riv_length * riv_weight) %>%
        dplyr::group_by(member.label) %>%
        dplyr::summarise(segweight = sum(weighted_len, na.rm = TRUE)) %>%
        # Remove members with 0 length
        dplyr::filter(segweight != 0)

    # If no weighting is supplied
    } else {

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
    if(form == "potamodromous") DCIs <- calculate_dci_pot(all_members, seg_weights, net, net_nodes, net_edges, points)

    # Diadromous case
    if(form == "diadromous") DCIs <- calculate_dci_dia(all_members, seg_weights, net, net_nodes, net_edges, points)

    # Return calculated DCI values
    return(DCIs)

  # If distance threshold is supplied
  } else {

    # If weighting is supplied
    if(weighted){

      # Calculate total weighted length of segments
      seg_weights <- net_nodes %>%
        dplyr::mutate(weighted_len = riv_length * riv_weight) %>%
        dplyr::group_by(member.label) %>%
        dplyr::summarise(segweight = sum(weighted_len, na.rm = TRUE)) %>%
        # Remove members with 0 length
        dplyr::filter(segweight != 0)

      # Calculate total non-weighted length of segments
      seg_lengths <- net_nodes %>%
        dplyr::group_by(member.label) %>%
        dplyr::summarise(segweight = sum(riv_length, na.rm = TRUE)) %>%
        # Remove members with 0 length
        dplyr::filter(segweight != 0)

    # If no weighting is supplied
    } else {
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
  }

}
