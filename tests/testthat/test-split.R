test_that("Split rivers returns correct number of sections", {

  # Make test river and points
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1,15,1,1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5,1)), sf::st_point(c(10,10)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Single point, single river
  split_riv <- split_rivers_at_points(riv, pnt[1,])
  expect_equal(nrow(split_riv) - nrow(riv), nrow(pnt[1,]))

  # Multiple points, single river
  split_riv <- split_rivers_at_points(riv, pnt)
  expect_equal(nrow(split_riv) - nrow(riv), nrow(pnt))

  # Prepare test multiple rivers
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1,15,1,1), 2)),
                                 sf::st_linestring(matrix(c(1,5,5,1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Single point, multiple rivers
  split_riv <- split_rivers_at_points(riv, pnt[1,])
  expect_equal(nrow(split_riv) - nrow(riv), nrow(pnt[1,]))

  # Multiple points, multiple rivers
  split_riv <- split_rivers_at_points(riv, pnt)
  expect_equal(nrow(split_riv) - nrow(riv), nrow(pnt))

})

test_that("Rivers are split at correct points", {

  # Make test river and points
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1,16,1,1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5,1)), sf::st_point(c(9,10)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Single point, single river
  split_riv <- split_rivers_at_points(riv, pnt[1,])
  expect_equal(as.numeric(sf::st_length(split_riv)[1]), 3.5)

  # Multiple points, single river
  split_riv <- split_rivers_at_points(riv, pnt)
  expect_equal(as.numeric(sf::st_length(split_riv)[3]), 7.1875)

})

test_that("Start and end point are preserved", {

  # Make test river and points
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1,16,1,1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5,1)), sf::st_point(c(10,10)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Mark river start and end points
  riv_start <- sf::st_cast(riv, "POINT")[1,]
  riv_end <- sf::st_cast(riv, "POINT")[2,]

  # Single point, single river
  split_riv <- split_rivers_at_points(riv, pnt[1,])
  new_start <- sf::st_cast(split_riv[1,], "POINT", warn = F)[1,]
  new_end <- sf::st_cast(split_riv[2,], "POINT", warn = F)[12,]
  expect_true(identical(new_end$geometry, riv_end$geometry))

})

test_that("Points outside tolerance do not participate in splitting", {

  # Make test river and points
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1,16,1,1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5,1)), sf::st_point(c(10,10)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Multiple point, single river
  split_riv <- split_rivers_at_points(riv, pnt, tolerance = 5)
  expect_equal(nrow(split_riv) - nrow(riv), 1)

})
