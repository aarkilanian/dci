binary_labeling <- function(rivnet){

  # Extract node table
  ex.nodes <- rivnet %>%
    sfnetworks::activate(nodes) %>%
    data.frame() %>%
    dplyr::mutate(node.label = "1") %>%
    dplyr::mutate(parent = "0") %>%
    dplyr::mutate(membership = "0")

  # Apply labeling function over network
  rivnet2 <- rivnet %>%
    sfnetworks::activate(nodes) %>%
    dplyr::mutate(node.label = tidygraph::map_bfs(root = which(.N()$type == "Sink"),
                                    .f = node_labeler, mode = "all"))

}

membership_labeling <- function(){

}

node_labeler <- function(node, parent, ...){

  # Get type of current node
  cur.type <- .N()$type[node]

  # Get ID of current node
  cur.ID <- .N()$nodeID[node]

  # Special condition for sink node
  if(cur.type == "Sink"){
    node.label <- "0"

    # Write to external table
    ex.nodes$node.label[which(ex.nodes$nodeID == cur.ID)] <<- node.label

    # Return label
    return(node.label)
  }

  # Get parent node ID
  par.ID <- .N()$nodeID[parent]

  # Get parent node label from external table
  par.label <- ex.nodes[ex.nodes$nodeID == par.ID,]$node.label

  # Check number of other nodes with same parent
  num.parents <- nrow(ex.nodes[ex.nodes$parent == par.ID,])
  print(num.parents)

  # Generate label
  node.label <- paste0(par.label, num.parents)

  # Write parent name to external table
  ex.nodes$parent[which(ex.nodes$nodeID == cur.ID)] <<- par.ID

  # Write label to external table
  ex.nodes$node.label[which(ex.nodes$nodeID == cur.ID)] <<- node.label

  # Return label
  return(as.character(node.label))
}
