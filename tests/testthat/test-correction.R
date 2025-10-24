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
  new_net <- enforce_dendritic(rivers, quiet = TRUE, max_loss = 100)
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
  new_net <- enforce_dendritic(rivers, quiet = TRUE, max_loss = 100)
  expect_equal(nrow(new_net), 6)
})

test_that("Divergence randomization works", {

  # Create test network
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(-1,0,1,2), 2)),
    sf::st_linestring(matrix(c(-1,-1,0,1), 2)),
    sf::st_linestring(matrix(c(2,-1,0,1), 2)),
    sf::st_linestring(matrix(c(3,2,-1,0), 2)),
    sf::st_linestring(matrix(c(2,4,0,1), 2)),
    sf::st_linestring(matrix(c(4,4,1,8), 2)),
    sf::st_linestring(matrix(c(5,4,-6,1), 2))
  ))

  # Make file connection for user input
  f <- file()
  options(mypkg.connection = f)
  ans <- paste(c("A", "A", "A", "B", "C"), collapse = "\n")
  write(ans, f)

  # Option A works
  new_net <- suppressMessages(enforce_dendritic(rivers, quiet = TRUE, max_loss = 50))
  expect_equal(round(sum(sf::st_length(new_net)), digits = 2), 17.72)
  # Option A reaches max iterations
  expect_error(suppressMessages(enforce_dendritic(rivers, quiet = TRUE, max_loss = 5, max_div_corr = 1)),
               "Reached maximum number of randomized divergence corrections. Increase 'max_div_corr' or correct manually.")
  # Option A cannot achieve less than max loss
  expect_error(suppressMessages(enforce_dendritic(rivers, quiet = TRUE, max_loss = 10, max_div_corr = 3)),
               "Reached maximum number of randomized divergence corrections. Increase 'max_div_corr' or correct manually.")

  # Option B works
  new_net <- suppressMessages(enforce_dendritic(rivers, quiet = TRUE, max_loss = 50))
  expect_equal(round(sum(sf::st_length(new_net)), digits = 2), 6.99)

  # Option C works
  new_net <- suppressMessages(enforce_dendritic(rivers, quiet = TRUE, max_loss = 50))
  expect_equal(new_net, NULL)

  # Clean up file connection
  options(mypkg.connection = stdin())
  close(f)

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
  net_correct <- suppressWarnings(sfnetworks::as_sfnetwork(enforce_dendritic(rivers, quiet = TRUE), message = FALSE)) %>%
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

  # Perform check for dendritic violations
  rivers_out <- suppressMessages(enforce_dendritic(rivers, correct = FALSE))

  expect_equal(all(c("complexID", "divergent") %in% colnames(rivers_out)), TRUE)

})
