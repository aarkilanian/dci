test_that("Nodes within tolerance are joined", {
  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(2, 1, 5, 5), 2)),
    sf::st_linestring(matrix(c(3, 2, 5, 5), 2)),
    sf::st_linestring(matrix(c(4, 3, 4, 4), 2)),
    sf::st_linestring(matrix(c(3, 3, 3, 4), 2)),
    sf::st_linestring(matrix(c(3, 3, 2, 3), 2)),
    sf::st_linestring(matrix(c(3, 3, 1, 2), 2)),
    sf::st_linestring(matrix(c(3, 3, 4, 5), 2))
  )) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  rivers <- sf::st_as_sf(rivers, wkt = "x")
  net <- sfnetworks::as_sfnetwork(rivers)

  # Create test nodes
  nodes <- sf::st_as_sf(sf::st_sfc(
    sf::st_point(c(2, 2)),
    sf::st_point(c(2, 7))
  )) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  nodes <- sf::st_as_sf(nodes, wkt = "x") %>%
    dplyr::mutate(type = "Barrier") %>%
    dplyr::mutate(perm = 0)

  # Run test without tolerance
  expect_snapshot(join_attributes(net, nodes))

  # Run test with tolerance
  expect_snapshot(join_attributes(net, nodes, tolerance = 1))
})
