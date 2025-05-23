test_that("River is imported correctly", {

  # Load river
  rivers <- readRDS(test_path("testdata", "riv_uncor.rds")) %>%
    dplyr::select(!c("from", "to"))

  # Test importing
  imported <- import_rivers(rivers = rivers, quiet = TRUE)
  expect_equal(ncol(imported), 7)

})

test_that("Non-projected river data throws error", {

  # Load river
  rivers <- readRDS(test_path("testdata", "riv_uncor.rds"))

  # Remove CRS
  sf::st_crs(rivers) <- NA

  # Test error
  expect_error(import_rivers(rivers), "Provided spatial data is not projected")

})

test_that("non-valid import path returns error", {
  expect_error(import_rivers("invalid_path"), "invalid spatial data provided")
  expect_error(import_points("invalid_path", type = "bars"), "invalid spatial data provided")
})

test_that("Cannot import rivers from file with geometries other than LINESTRING or MULTILINESTRING", {
  # Create river data
  rivers <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(matrix(c(1, 2, 2, 2), 2)),
    sf::st_linestring(matrix(c(2, 2, 4, 2), 2)),
    sf::st_linestring(matrix(c(2, 2, 1, 2), 2)),
    sf::st_linestring(matrix(c(2, 10, 2, 2), 2)),
    sf::st_linestring(matrix(c(10, 10, 2, 1), 2)),
    sf::st_linestring(matrix(c(10, 10, 2, 5), 2))
  ))
  # Create point data
  bars <- sf::st_as_sf(sf::st_sfc(
    sf::st_point(c(1, 1)),
    sf::st_point(c(2, 2)),
    sf::st_point(c(3, 3))
  ))

  # Run test
  expect_error(import_rivers(bars), "Provided data contains geometries other than LINESTRING and MULTILINESTRING")
})


test_that("Points are imported correctly", {
  # Test barrier
  bars <- sf::st_as_sf(sf::st_sfc(
    sf::st_point(c(1, 1)),
    sf::st_point(c(2, 2)),
    sf::st_point(c(3, 3))
  ))
  bars <- sf::st_set_crs(bars, 3347)
  expect_equal(nrow(import_points(bars, type = "bars")), 3)


  # Test sink
  expect_equal(nrow(import_points(bars[1, ], type = "out")), 1)

})

test_that("Points that intersect throw error", {
  # Test barrier
  bars <- sf::st_as_sf(sf::st_sfc(
    sf::st_point(c(1, 1)),
    sf::st_point(c(1, 1)),
    sf::st_point(c(2, 2)),
    sf::st_point(c(3, 3))
  ))
  bars <- sf::st_set_crs(bars, 3347)

  # Run test
  expect_error(import_points(bars, type = "bars"), "There are overlapping geometries in the data provided")
})
