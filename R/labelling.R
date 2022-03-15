binary_labeling <- function(rivnet){

  # Extract node table
  ex.nodes <- rivnet %>%
    sfnetworks::activate(nodes) %>%
    data.frame() %>%
    dplyr::mutate(node.label = "1") %>%
    dplyr::mutate(parent = "0") %>%
    dplyr::mutate(membership = "0")

  # Apply labeling function over network
  rivnet2 <- sub_rivnet %>%
    sfnetworks::activate(nodes) %>%
    dplyr::mutate(node.label = tidygraph::map_bfs_int(root = which(.N()$type == "Sink"),
                                    .f = node_labeler, mode = "all"))

}

node_labeler <- function(node, parent, path, ...){

  cur.type <- .N()$type[node]
  print(length(path$result))

  if(cur.type == "Sink"){
    node.label <- 0
    return(as.integer(node.label))
  }

  res_list <- path$result[[length(path$result)]]
  res_vec <- as.vector(res_list)
  res_last <- res_vec[length(res_vec)]

  # Check for second child
  if(nrow(path) > 2){
    dup_vec <- as.vector(path$result[[length(path$result) - 1]])
    dup_len <- length(dup_vec)
    res_len <- length(res_vec)
    if(length(path$result) == length()){
      print("second baby")
      node.label <- as.integer(res_last) * 10
      return(as.integer(node.label))
    }
  }

  node.label <- as.integer(res_last) + 1
  return(as.integer(node.label))


}
