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
