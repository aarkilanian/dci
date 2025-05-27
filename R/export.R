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
#'   or `"bars"`.
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
#' res_pot <- calculate_dci(net = yamaska_net, form = "pot", pass = "pass_1",
#' quiet = TRUE)
#' res_dia <- calculate_dci(net = yamaska_net, form = "dia", pass = "pass_1",
#' quiet = TRUE)
#'
#' # Export segment-level potamodromous DCI results to rivers
#' riv_results <- export_dci(net = yamaska_net, results = res_pot,
#' type = "rivers")
#'
#' # Can also be run quietly to keep from plotting results
#' riv_results <- export_dci(net = yamaska_net, results = res_pot,
#' type = "rivers", quiet = TRUE)
#'
#' # Results can also be exported to barrier points
#' bar_results <- export_dci(net = yamaska_net, results = res_pot,
#' type = "bars")
#'
#' # If multiple results are calculated these can be combined together
#' all_res <- export_dci(net = yamaska_net, results = list(res_pot, res_dia),
#' type = "rivers")
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
  } else if (type == "bars") {
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
  } else {
    stop("type must be either 'rivers' or 'bars'.")
  }
}
