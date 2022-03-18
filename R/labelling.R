binary_labeling <- function(rivnet){

  # Store labels during creation
  old_parent <- 0

  # Apply labeling function over network
  rivnet <- rivnet %>%
    sfnetworks::activate(nodes) %>%
    dplyr::mutate(node.label = tidygraph::map_bfs(root = which(.N()$type == "Sink"),
                                    .f = node_labeler, mode = "all"))

  # Return labeled network
  invisible(rivnet)

}

membership_labeling <- function(rivnet){

  # Create member IDs vector
  num_bars <- nrow(rivnet %>% sfnetworks::activate(nodes) %>% dplyr::filter(type == "Barrier") %>% as.data.frame())
  member_IDs <- 1:num_bars

  # Apply membership labeling over network
  rivnet <- rivnet %>%
    sfnetworks::activate(nodes) %>%
    dplyr::mutate(node.membership = tidygraph::map_dfs_int(root = which(.N()$type == "Sink"),
                                                           .f = membership_labeler, mode = "all"))

  # Return labeled network
  invisible(rivnet)

}

node_labeler <- function(node, parent, path, ...){

  cur.type <- .N()$type[node]

  if(cur.type == "Sink"){
    node.label <- c(FALSE)
    return(node.label)
  }

  # Get parent label
  par_label <- as.vector(path$result[[length(path$result)]])

  if(is.na(old_parent)){

    # Store current parent as old parent
    old_parent <<- parent

    # Assign node label based on parent
    node.label <- append(par_label, FALSE)
    return(node.label)

  } else if(old_parent == parent){

    # Store current parent as old parent
    old_parent <<- parent

    # Assign node label based on parent
    node.label <- append(par_label, TRUE)
    return(node.label)

  } else{

    # Store current parent as old parent
    old_parent <<- parent

    # Assign node label based on parent
    node.label <- append(par_label, FALSE)
    return(node.label)

  }

}

membership_labeler <- function(node, parent, path, ...){

  member <- member_IDs[1]

  # If current node is a barrier use new member ID
  if(.N()$type[node] == "Barrier"){

    # Remove used label
    member_IDs <<- member_IDs[-1]

    # Use new label
    member <- member_IDs[1]
    return(member)

  } else{

    # Reuse label
    return(member)

  }

}
