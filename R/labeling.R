#' Label nodes with logical vector binary IDs
#'
#' @param rivnet rivnet object
#'
#' @noRd
node_labeling <- function(rivnet){
  # Create new env
  labelenv <- new.env(parent = emptyenv())
  # Create variable to keep track of created labels outside loop
  assign('past_label', c(FALSE), labelenv)
  # Apply labeling function over network
  rivnet <- rivnet %>%
    activate(nodes) %>%
    dplyr::mutate(node.label = tidygraph::map_bfs(root = which(tidygraph::.N()$type == "Sink"),
                                    .f = node_labeler, env = labelenv, mode = "all"))
  # Return labeled network
  invisible(rivnet)
}

#' Label nodes with integer segment member ID
#'
#' @param rivnet rivnet object
#'
#' @noRd
membership_labeling <- function(rivnet){
  # Retrieve number of barriers
  num_bar <- rivnet %>%
    activate(nodes) %>%
    as.data.frame() %>%
    dplyr::filter(type == "Barrier") %>%
    nrow()
  # Create new env
  memberenv <- new.env(parent = emptyenv())
  # Create variable in new environment to hold member IDs
  assign("labels", 1:(num_bar*2), envir = memberenv)
  # Apply labeling function over network
  rivnet <- rivnet %>%
    activate(nodes) %>%
    dplyr::mutate(member.label = tidygraph::map_dfs_int(root = which(tidygraph::.N()$type == "Sink"),
                                                    .f = membership_labeler, env = memberenv, mode = "all"))
}

#' Node labeling function passed to \code{\link[tidygraph]{map_bfs}}
#'
#' @param rivnet rivnet object
#'
#' @noRd
node_labeler <- function(node, parent, path, env, ...){
  cur.type <- tidygraph::.N()$type[node]
  if(cur.type == "Sink"){
    # Create sink label
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
#' @param rivnet rivnet object
#'
#' @noRd
membership_labeler <- function(node, parent, path, env, ...){
  cur.type <- tidygraph::.N()$type[node]
  if(cur.type == "Sink"){
    # Create sink label
    member_label <- 0
    # Return label
    return(member_label)
  }
  parent_label <- as.integer(as.vector(path$result[length(path$result)]))
  # If current node is a barrier use new member ID
  if(tidygraph::.N()$type[node] == "Barrier"){
    # Retrieve label list
    old_labels <- get("labels", envir = env)
    # Choose new label
    member_label <- old_labels[1]
    # Remove label from list
    assign("labels", old_labels[-1], envir = env)
    return(member_label)
  } else{
    # Reuse label
    return(parent_label)
  }
}
