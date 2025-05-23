#' Export DCI Results to Spatial Format
#'
#' Exports the output of `calculate_dci()` as a spatial object with DCI values joined
#' to the relevant features in the river network.
#'
#' @inheritParams calculate_dci
#' @param results A `dci_results` object, or a list of such objects, as returned by
#'   [calculate_dci()].
#' @param type A character string specifying which component of the river network
#'   the results should be exported for. Valid options are `"rivers"` (default),
#'   or any of the node layers included in the [river_net] object, such as
#'   `"barriers"` or `"poi"`.
#' @param relative A logical value indicating whether relative DCI values should
#'   be returned in addition to raw values. Defaults to `FALSE`.
#'
#' @return An [sf] object containing the corresponding DCI results joined
#'   to the selected network component. If multiple results are supplied, result
#'   columns are appended by a number corresponding to the index of the
#'   associated results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_dci(net = net_name, results = dci_results, type = "rivers")
#' export_dci(net = net_name, results = dci_results, type = "poi")
#' export_dci(net = net_name, results = dci_results, type = "barriers")
#' export_dci(net = net_name, results = list(result_a, result_b), type = "rivers")
#' }
export_dci <- function(net, results, type = "rivers", relative = FALSE,
                       quiet = TRUE) {
  if (type == "rivers") {
    # Extract nodes
    nodes <- sf::st_as_sf(activate(net, nodes)) %>%
      dplyr::mutate(nodeID = dplyr::row_number())

    # Join nodes to rivers
    rivers <- sf::st_as_sf(activate(net, edges)) %>%
      dplyr::left_join(as.data.frame(nodes), by = c("from" = "nodeID"))

    # Convert single input to list
    if (!(inherits(results, "list"))) {
      results <- list(results)
    }

    # Iterate over list of results
    for (i in 1:length(results)) {
      # Rename result columns
      if (length(results) > 1) {
        names(results[[i]])[names(results[[i]]) == "DCI"] <- paste0("DCI_", i)
        if(relative) names(results[[i]])[names(results[[i]]) == "DCI_rel"] <- paste0("DCI_rel_", i)
      }

      # Join results
      rivers <- rivers %>%
        dplyr::left_join(results[[i]], by = c("member_label" = "segment"))
    }

    # Remove node label column
    rivers <- rivers[, !(names(rivers) == "node_label")]

    # Plot result if only one result joined
    if (length(results) == 1) {
      plot(rivers["DCI"])
    }

    # Return result
    invisible(rivers)
  } else if (type == "barriers") {
    # Extract only barrier and outlet nodes
    barriers <- sf::st_as_sf(activate(net, nodes)) %>%
      dplyr::filter(.data$type %in% c("barrier", "outlet"))

    # Convert single input to list
    if (!(inherits(results, "list"))) {
      results <- list(results)
    }

    # Iterate over list of results
    for (i in 1:length(results)) {
      # Rename result columns
      if (length(results) > 1) {
        names(results[[i]])[names(results[[i]]) == "DCI"] <- paste0("DCI_", i)
        if(relative) names(results[[i]])[names(results[[i]]) == "DCI_rel"] <- paste0("DCI_rel_", i)
      }

      # Join results
      barriers <- barriers %>%
        dplyr::left_join(results[[i]], by = c("member_label" = "segment"))
    }

    # Remove node label column
    barriers <- barriers[, !(names(barriers) == "node_label")]


    # Plot results if only one result joined
    if (length(results) == 1) {
      plot(barriers["DCI"])
    }

    # Return result
    invisible(barriers)
  } else if (type == "poi") {
    # Extract only points of interest
    poi <- sf::st_as_sf(activate(net, nodes)) %>%
      dplyr::filter(.data$type %in% c("barrier", "outlet"))

    # Convert single input to list
    if (!(inherits(results, "list"))) {
      results <- list(results)
    }

    # Iterate over list of results
    for (i in 1:length(results)) {
      # Rename result columns
      if (length(results) > 1) {
        names(results[[i]])[names(results[[i]]) == "DCI"] <- paste0("DCI_", i)
        names(results[[i]])[names(results[[i]]) == "DCI_rel"] <- paste0("DCI_rel_", i)
      }

      # Join results
      barriers <- barriers %>%
        dplyr::left_join(results[[i]], by = c("member_label" = "segment"))
    }

    # Remove node label column
    barriers <- barriers[, !(names(barriers) == "node_label")]


    # Plot results if only one result joined
    if (length(results) == 1) {
      plot(barriers["DCI"])
    }

    # Return result
    invisible(barriers)

    # Join results to others
    others <- sf::st_as_sf(activate(net, nodes)) %>%
      dplyr::filter(.data$type == "poi") %>%
      dplyr::left_join(results, by = c("member_label" = "segment"))
    others <- others[, !(names(others) == "node_label")]

    # Plot results
    plot(others["DCI"])

    # Return result
    invisible(others)
  } else {
    stop("type must be either 'rivers' or 'barriers'.")
  }
}
