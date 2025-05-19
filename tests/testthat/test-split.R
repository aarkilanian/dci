test_that("Split rivers returns correct number of sections", {
  # Make test river and points
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1, 15, 1, 1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  riv$riv_length <- 10
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(7, 1)), sf::st_point(c(10, 10)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Single point, single river
  split_riv <- split_rivers_at_points(riv, pnt[1, ])
  expect_equal(nrow(split_riv) - nrow(riv), nrow(pnt[1, ]))

  # Multiple points, single river
  split_riv <- split_rivers_at_points(riv, pnt)
  expect_equal(nrow(split_riv) - nrow(riv), nrow(pnt))

  # Prepare test multiple rivers
  riv <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1, 15, 1, 1), 2)),
    sf::st_linestring(matrix(c(1, 5, 5, 1), 2))
  )) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv$riv_length <- c(5, 10)

  # Single point, multiple rivers
  split_riv <- split_rivers_at_points(riv, pnt[1, ])
  expect_equal(nrow(split_riv) - nrow(riv), nrow(pnt[1, ]))

  # Multiple points, multiple rivers
  split_riv <- split_rivers_at_points(riv, pnt)
  expect_equal(nrow(split_riv) - nrow(riv), nrow(pnt))
})

test_that("Rivers are split at correct points", {
  # Make test river and points
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1, 16, 1, 1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  riv$riv_length <- 10
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 1)), sf::st_point(c(9, 10)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Single point, single river
  split_riv <- split_rivers_at_points(riv, pnt[1, ])
  expect_equal(as.numeric(sf::st_length(split_riv)[1]), 3.5)

  # Multiple points, single river
  split_riv <- split_rivers_at_points(riv, pnt)
  expect_equal(as.numeric(sf::st_length(split_riv)[3]), 7.1875)
})

test_that("Start and end point are preserved", {
  # Make test river and points
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1, 16, 1, 1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  riv$riv_length <- 10
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 1)), sf::st_point(c(10, 10)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Mark river start and end points
  riv_start <- suppressWarnings(sf::st_cast(riv, "POINT"))[1, ]
  riv_end <- suppressWarnings(sf::st_cast(riv, "POINT"))[2, ]

  # Single point, single river
  split_riv <- split_rivers_at_points(riv, pnt[1, ])
  new_start <- sf::st_cast(split_riv[1, ], "POINT", warn = F)[1, ]
  new_end <- sf::st_cast(split_riv[2, ], "POINT", warn = F)[12, ]
  expect_true(identical(unname(unlist(new_end$geometry)), unlist(riv_end$geometry)))
})

test_that("Points outside tolerance do not participate in splitting", {
  # Make test river and points
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1, 16, 1, 1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  riv$riv_length <- 10
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 1)), sf::st_point(c(10, 10)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Multiple point, single river
  split_riv <- split_rivers_at_points(riv, pnt, tolerance = 5)
  expect_equal(nrow(split_riv) - nrow(riv), 1)
})

test_that("Outlet excluded in points for splitting", {

  # Make rivers
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1, 16, 1, 1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  riv$riv_length <- 10

  # Make points
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 1)), sf::st_point(c(9, 10)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry") %>%
    dplyr::mutate(type = c("barrier", "outlet"))

  # Run test
  expect_equal(nrow(split_rivers_at_points(riv, pnt)),2)
})

test_that("Skips short rivers", {

  # Make rivers
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1, 1, 1, 2), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  riv$riv_length <- 10

  # Make points
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 1)), sf::st_point(c(9, 10)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry") %>%
    dplyr::mutate(type = c("barrier", "outlet"))

  expect_warning(split_rivers_at_points(riv, pnt), "River too short to perform splitting.")

})

test_that("Rivers split when points are at start or end", {

  # Make rivers
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1, 16, 1, 1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  riv$riv_length <- 10

  # Make points
  pnt_start <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(1,1)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  pnt_end <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(16, 1)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Test start
  splitted <- split_rivers_at_points(riv, pnt_start)
  expect_equal(splitted[1,]$riv_length, 1.5)

  # Test end
  splitted <- split_rivers_at_points(riv, pnt_end)
  expect_equal(splitted[2,]$riv_length, 1.5)
})

test_that("Nodes outside specified tolerance are skipped", {

  # Make rivers
  riv <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1, 16, 1, 1), 2)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")
  riv <- sf::st_as_sf(riv, wkt = "x")
  riv$riv_length <- 10

  # Make points
  pnt <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)))) %>%
    dplyr::rename("geometry" = "x") %>%
    sf::st_as_sf(wkt = "geometry")

  # Test
  expect_equal(nrow(split_rivers_at_points(riv, pnt, tolerance = 3)), 1)

})
