test_that("Single divergences (loops) are corrected", {
  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1, 1, 4, 3), 2)),
    sf::st_linestring(matrix(c(1, 2, 3, 2), 2)),
    sf::st_linestring(matrix(c(1, 1, 3, 1), 2)),
    sf::st_linestring(matrix(c(2, 1, 2, 1), 2)),
    sf::st_linestring(matrix(c(1, 1, 1, 0), 2))
  ))

  # Run test
  new_net <- enforce_dendritic(rivers, quiet = TRUE)
  expect_equal(nrow(new_net), 4)
})

test_that("Multiple divergences (braiding) are corrected", {
  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1, 2, 3, 2), 2)),
    sf::st_linestring(matrix(c(1, 1, 3, 1), 2)),
    sf::st_linestring(matrix(c(2, 1, 2, 1), 2)),
    sf::st_linestring(matrix(c(1, 1, 1, 0), 2)),
    sf::st_linestring(matrix(c(0, 1, 4, 3), 2)),
    sf::st_linestring(matrix(c(1, 1, 5, 3), 2)),
    sf::st_linestring(matrix(c(1, 0, 5, 4), 2)),
    sf::st_linestring(matrix(c(1, 1, 6, 5), 2))
  ))

  # Run test
  new_net <- enforce_dendritic(rivers, quiet = TRUE)
  expect_equal(nrow(new_net), 6)
})

test_that("Single complex nodes are corrected", {
  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1, 2, 1, 3), 2)),
    sf::st_linestring(matrix(c(3, 2, 1, 3), 2)),
    sf::st_linestring(matrix(c(0, 2, 3, 3), 2)),
    sf::st_linestring(matrix(c(2, 4, 3, 3), 2))
  ), crs = 32198) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  rivers <- sf::st_as_sf(rivers, wkt = "x")

  expect_equal(nrow(enforce_dendritic(rivers, quiet = TRUE)), 5)
})

test_that("Multiple complex nodes are corrected", {
  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1,1,0,1), 2)),
    sf::st_linestring(matrix(c(0,1,1,1), 2)),
    sf::st_linestring(matrix(c(2,1,1,1), 2)),
    sf::st_linestring(matrix(c(0,1,4,4), 2)),
    sf::st_linestring(matrix(c(2,1,4,4), 2)),
    sf::st_linestring(matrix(c(1,1,2,3), 2)),
    sf::st_linestring(matrix(c(1,1,1,2), 2)),
    sf::st_linestring(matrix(c(1,1,3,4), 2)),
    sf::st_linestring(matrix(c(1,1,4,5), 2))
  ), crs = 32198) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  rivers <- sf::st_as_sf(rivers, wkt = "x")

  # Correct complex confluences
  net_correct <- sfnetworks::as_sfnetwork(suppressWarnings(enforce_dendritic(rivers, quiet = TRUE))) %>%
    # Get degree
    dplyr::mutate(degree = tidygraph::centrality_degree()) %>%
    sfnetworks::activate(nodes) %>%
    as.data.frame()

  # Check no degree above 3
  expect_equal(sum(net_correct$degree > 3), 0)
})

test_that("Complex confluence and divergence corrected together 1", {
  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1, 2, 1, 3), 2)),
    sf::st_linestring(matrix(c(3, 2, 1, 3), 2)),
    sf::st_linestring(matrix(c(0, 2, 3, 3), 2)),
    sf::st_linestring(matrix(c(2, 4, 3, 3), 2))
  ), crs = 32198) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  rivers <- sf::st_as_sf(rivers, wkt = "x")

  expect_equal(nrow(enforce_dendritic(rivers, quiet = TRUE)), 5)
})

test_that("Error when complex confluence has more than 3 inputs", {
  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1, 2, 1, 3), 2)),
    sf::st_linestring(matrix(c(3, 2, 1, 3), 2)),
    sf::st_linestring(matrix(c(1, 2, 3, 3), 2)),
    sf::st_linestring(matrix(c(3, 2, 3, 3), 2)),
    sf::st_linestring(matrix(c(2, 2, 3, 4), 2))
  ))

  # Run test
  expect_error(enforce_dendritic(rivers, quiet = TRUE), "Complex confluences with over 3 input tributaries have been detected. Use the standalone `enforce_dendritic()` and correct returned errors manually.", fixed = TRUE)
})

test_that("Message is written when no complex", {
  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1, 1, 1, 2), 2)),
    sf::st_linestring(matrix(c(1, 1, 2, 3), 2)),
    sf::st_linestring(matrix(c(2, 1, 2, 2), 2))
  ))
  net <- sfnetworks::as_sfnetwork(rivers)

  # Run test
  expect_message(correct_complex(net), "No complex confluences found.")
})

test_that("Message is written when no divergences", {
  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1, 1, 1, 2), 2)),
    sf::st_linestring(matrix(c(1, 1, 2, 3), 2)),
    sf::st_linestring(matrix(c(2, 1, 2, 2), 2))
  ))
  net <- sfnetworks::as_sfnetwork(rivers, length_as_weight = TRUE)

  # Run test
  expect_message(correct_divergences(net), "No divergences detected.")
})

test_that("Manual correction works", {

  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1, 2, 1, 3), 2)),
    sf::st_linestring(matrix(c(3, 2, 1, 3), 2)),
    sf::st_linestring(matrix(c(0, 2, 3, 3), 2)),
    sf::st_linestring(matrix(c(2, 4, 3, 3), 2))
  ), crs = 32198) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  rivers <- sf::st_as_sf(rivers, wkt = "x")

  rivers_out <- enforce_dendritic(rivers, correct = FALSE)

})
