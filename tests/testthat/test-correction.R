test_that("Divergence correction removes one river per divergence", {

  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(6,6,5,7), 2)),
    sf::st_linestring(matrix(c(6,6,4,5), 2)),
    sf::st_linestring(matrix(c(6,6,3,4), 2)),
    sf::st_linestring(matrix(c(6,6,2,3), 2)),
    sf::st_linestring(matrix(c(6,6,1,2), 2)),
    sf::st_linestring(matrix(c(6,5,7,5), 2)),
    sf::st_linestring(matrix(c(6,5,5,5), 2)),
    sf::st_linestring(matrix(c(5,4,5,5), 2)),
    sf::st_linestring(matrix(c(4,3,5,5), 2)),
    sf::st_linestring(matrix(c(3,2,5,5), 2)),
    sf::st_linestring(matrix(c(2,1,5,5), 2))
    ))
  net <- sfnetworks::as_sfnetwork(rivers, length_as_weight = TRUE)

  # Run test
  new_net <- suppressWarnings(correct_divergences(net, quiet = TRUE))
  new_riv <- sfnetworks::activate(new_net, edges)
  new_riv <- as.data.frame(new_riv)
  expect_equal(nrow(new_riv), 10)

})

test_that("Message is written when no divergences", {

  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1,1,1,2), 2)),
    sf::st_linestring(matrix(c(1,1,2,3), 2)),
    sf::st_linestring(matrix(c(2,1,2,2), 2))
  ))
  net <- sfnetworks::as_sfnetwork(rivers, length_as_weight = TRUE)

  # Run test
  expect_message(suppressWarnings(correct_divergences(net)), "No divergences detected.")

})

test_that("Error when complex confluence has more than 3 inputs", {

  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1,2,1,3), 2)),
    sf::st_linestring(matrix(c(3,2,1,3), 2)),
    sf::st_linestring(matrix(c(1,2,3,3), 2)),
    sf::st_linestring(matrix(c(3,2,3,3), 2)),
    sf::st_linestring(matrix(c(2,2,3,4), 2))
  ))
  net <- sfnetworks::as_sfnetwork(rivers)

  # Run test
  expect_error(correct_complex(net), "Complex confluences with over 3 input tributaries have been detected. Use the standalone `enforce_dendritic()` and correct returned errors manually.", fixed = TRUE)

})

test_that("Message is written when no complex", {

  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1,1,1,2), 2)),
    sf::st_linestring(matrix(c(1,1,2,3), 2)),
    sf::st_linestring(matrix(c(2,1,2,2), 2))
  ))
  net <- sfnetworks::as_sfnetwork(rivers)

  # Run test
  expect_message(correct_complex(net), "No complex confluences found.")

})

test_that("Complex nodes are corrected", {

  # Create test river network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1,2,1,3), 2)),
    sf::st_linestring(matrix(c(3,2,1,3), 2)),
    sf::st_linestring(matrix(c(0,2,3,3), 2)),
    sf::st_linestring(matrix(c(2,4,3,3), 2))
  ), crs = 32198) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  rivers <- sf::st_as_sf(rivers, wkt = "x")
  net <- sfnetworks::as_sfnetwork(rivers)

  expect_snapshot(correct_complex(net))

})
