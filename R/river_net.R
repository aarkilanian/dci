#' Create a `river_net` Object
#'
#' Constructs a [river_net] object, a geospatial network structure built on top
#' of the [sfnetworks::sfnetwork()] class. This object integrates river lines,
#' barriers and outlets allowing for connectivity analyses with
#' [calculate_dci()] or other network tools.
#'
#' @param rivers A `rivers` object returned by [import_rivers()].
#' @param barriers A `barriers` object returned by [import_points()] with
#' `type = "bars"`.
#' @param outlet An `outlet` object returned by [import_points()] with
#' `type = "out"`.
#' @param check Logical. If `TRUE` (default), dendritic topology is enforced
#' using [enforce_dendritic()].
#' @param tolerance A numeric value specifying the snapping distance
#' (in map units) to align points to the river network. Defaults to `NULL`,
#' meaning no snapping.
#' @param max_iter An integer indicating the maximum number of correction
#' iterations to run. As some topological errors are corrected new ones can
#' can arise requiring multiple passes. In some cases, an automated correction
#' choice can lead to a recursive correction that eliminates most rivers. In
#' this case, some manual corrections may help avoid this.
#'
#' @return An object of class [river_net] representing the river network formed
#' from the provided spatial inputs.
#'
#' @export
#'
#' @examples
#' # Simple case with rivers, barriers, and outlet
#' yam_net <- river_net(rivers = yamaska_rivers, barriers = yamaska_barriers,
#' outlet - yamaska_outlet)
#'
#' # If corrections are done earlier they can be skipped in this constructor
#' riv_cor <- enforce_dendritic(rivers = yamaska_rivers, correct = TRUE,
#' quiet = TRUE)
#' yam_net <- river_net(rivers = yamaska_cor, barriers = yamaska_barriers,
#' outlet - yamaska_outlet, check = FALSE)
#'
#' # For large river networks it may be better to specify a smaller number of
#' # correction sweeps.
#' yam_net <- river_net(rivers = yamaska_rivers, barriers = yamaska_barriers,
#' outlet - yamaska_outlet, max_iter = 3)
river_net <- function(rivers,
                      barriers,
                      outlet,
                      check = TRUE,
                      tolerance = NULL,
                      max_iter = 10) {
  # Check rivers
  if (!("rivers" %in% class(rivers))) {
    stop("Rivers must first be imported with `import_rivers`")
  }

  # Check barriers
  if (!("barriers" %in% class(barriers))) {
    stop("Barriers must first be imported with `import_points`")
  }
  if (sf::st_crs(barriers) != sf::st_crs(rivers)) {
    stop("CRS of barriers does not match rivers")
  }

  # Check sinks
  if (!("outlet" %in% class(outlet))) {
    stop("outlet must first be imported with `import_points`")
  }
  if (sf::st_crs(outlet) != sf::st_crs(rivers)) {
    stop("CRS of outlet does not match rivers")
  }

  # Combine nodes
  user_nodes <- dplyr::bind_rows(barriers, outlet)

  # Ensure user_nodes has geometry column named "geometry"
  if (!("geometry" %in% colnames(user_nodes))) {
    user_nodes <- rename_geometry(user_nodes, "geometry")
  }

  # Clean up topology if requested
  if (check == TRUE) {
    # Perform necessary corrections
    rivers <- enforce_dendritic(rivers, correct = TRUE)
  }

  # Split rivers at user node locations
  rivers <- split_rivers_at_points(rivers, user_nodes, tolerance) %>%
    dplyr::mutate(rivID = seq_len(dplyr::n()))

  # Ensure rivers has geometry column named "geometry"
  if (!("geometry" %in% colnames(rivers))) {
    rivers <- rename_geometry(rivers, "geometry")
  }

  # Create final sfnetwork
  suppressWarnings(
    net <- sfnetworks::as_sfnetwork(rivers)
  )

  # Join special node attributes
  net <- join_attributes(net, user_nodes, tolerance)

  # Apply binary labeling
  net <- node_labeling(net)

  # Apply membership labelling
  net <- membership_labeling(net)

  # Define river_net class
  net <- structure(net, class = c("river_net", class(net)))
  invisible(net)
}
