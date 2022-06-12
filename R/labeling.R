#' Label nodes with logical vector binary IDs
#'
#' @inheritParams calculate_dci
#'
#' @return A \code{\link{river_net}} object with nodes assigned binary topological labels.
#'
#' @keywords internal
#' @export
node_labeling <- function(net){
  # Create new env
  labelenv <- new.env(parent = emptyenv())
  # Create variable to keep track of created labels outside loop
  assign('past_label', c(FALSE), labelenv)
  # Determine row index of outlet
  out_index <- as.data.frame(activate(net, nodes))$type
  out_index <- which(out_index == "outlet")
  # Apply labeling function over network
  net <- activate(net, nodes) %>%
    dplyr::mutate(node.label = tidygraph::map_bfs(root = out_index,
                                    .f = node_labeler, env = labelenv, mode = "all"))
  # Return labeled network
  invisible(net)
}

#' Label nodes with integer segment member ID
#'
#' @inheritParams calculate_dci
#'
#' @return A \code{\link{river_net}} object with nodes assigned segment membership labels.
#'
#' @keywords internal
#' @export
membership_labeling <- function(net){
  # Retrieve number of barriers
  num_bar <- as.data.frame(activate(net, nodes)) %>%
    dplyr::filter(.data$type == "barrier") %>%
    nrow()
  # Create new env
  memberenv <- new.env(parent = emptyenv())
  # Create variable in new environment to hold member IDs
  assign("labels", 1:(num_bar*2), envir = memberenv)
  # Determine row index of outlet
  out_index <- as.data.frame(activate(net, nodes))$type
  out_index <- which(out_index == "outlet")
  # Apply labeling function over network
  net <- activate(net, nodes) %>%
    dplyr::mutate(member.label = tidygraph::map_dfs_int(root = out_index,
                                                    .f = membership_labeler, env = memberenv, mode = "all"))
}

#' Node labeling function passed to \code{\link[tidygraph]{map_bfs}}
#'
#' @param node The index of the current node.
#' @param parent The index of the parent node.
#' @param path A list of previous results.
#' @param env A parent environment holding past assigned labels.
#' @param ... other parameters.
#'
#' @return The correct node label. Either TRUE or FALSE.
#'
#' @keywords internal
#' @export
node_labeler <- function(node, parent, path, env, ...){
  cur.type <- tidygraph::.N()$type[node]
  if(cur.type == "outlet"){
    # Create outlet label
    node_label <- c(FALSE)
    # Write to external variable
    assign("past_label", node_label, envir = env)
    # Return label
    return(node_label)
  }
  # Get parent label
  par_label <- path$result[[length(path$result)]]
  # Create new label
  node_label <- append(par_label, FALSE)
  # Retrieve last issued label
  old_label <- get("past_label", envir = env)
  # If same append TRUE to parent label and return
  if(identical(node_label, old_label)){
    node_label <- append(par_label, TRUE)
    # Assign new label to old label environment
    assign("past_label", node_label, envir = env)
    # Return label
    return(node_label)
    # If different return original label
  } else{
    # Assign new label to old label environment
    assign("past_label", node_label, envir = env)
    # Return label
    return(node_label)
  }
}

#' Membership labeling function passed to \code{\link[tidygraph]{map_bfs}}
#'
#' @inheritParams node_labeler
#'
#' @return An integer representing the current node's segment membership.
#'
#' @keywords internal
#' @export
membership_labeler <- function(node, parent, path, env, ...){
  cur.type <- tidygraph::.N()$type[node]
  if(cur.type == "outlet"){
    # Create outlet label
    member_label <- 0
    # Return label
    return(member_label)
  }
  parent_label <- as.integer(as.vector(path$result[length(path$result)]))
  # If current node is a barrier use new member ID
  if(tidygraph::.N()$type[node] == "barrier"){
    # Retrieve label list
    old_labels <- get("labels", envir = env)
    # Choose new label
    member_label <- old_labels[1]
    print(member_label)
    # Remove label from list
    assign("labels", old_labels[-1], envir = env)
    return(member_label)
  } else{
    # Reuse label
    return(parent_label)
  }
}
