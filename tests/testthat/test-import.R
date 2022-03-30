test_that("non-valid import path returns error", {

  expect_error(import_rivers("invalid_path"), "invalid spatial data provided")
  expect_error(import_points("invalid_path", type = "Barrier"), "invalid spatial data provided")

})

test_that("Invalid river weighting throws error", {

  # Create test rivers
  rivers <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1,2,2,2), 2)),
                                    sf::st_linestring(matrix(c(2,2,4,2), 2)),
                                    sf::st_linestring(matrix(c(2,2,1,2), 2)),
                                    sf::st_linestring(matrix(c(2,10,2,2), 2)),
                                    sf::st_linestring(matrix(c(10,10,2,1), 2)),
                                    sf::st_linestring(matrix(c(10,10,2,5), 2))
  ))
  # Create test weighting field with characters
  rivers$weight_chr <- c("d", "l", "j", "g", "d", "a")
  # Create test weighting field  outisde range
  rivers$weight_rng <- c(1,4,3,5,43,5)

  # Run test
  expect_error(import_rivers(rivers, "weight_chr"), "Weight values must be numeric.")
  expect_error(import_rivers(rivers, "weight_rng"), "Weight values must be between 0 and 1.")

})

test_that("rivers are imported correctly", {

  # Create test rivers
  rivers <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(c(1,2,2,2), 2)),
                                    sf::st_linestring(matrix(c(2,2,4,2), 2)),
                                    sf::st_linestring(matrix(c(2,2,1,2), 2)),
                                    sf::st_linestring(matrix(c(2,10,2,2), 2)),
                                    sf::st_linestring(matrix(c(10,10,2,1), 2)),
                                    sf::st_linestring(matrix(c(10,10,2,5), 2))
  ))

  expect_equal(nrow(import_rivers(rivers)), nrow(rivers))

  # Run test
  expect_equal(nrow(import_rivers(test_path("testdata", "rivers.shp"))), 11)

})

test_that("Invalid permeability throws error", {

  # Create points
  bars <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(1,1)),
                                  sf::st_point(c(2,2)),
                                  sf::st_point(c(3,3))))
  # Create permeability with characters
  test_perm_chr <- c("20 %", "20 %", "40 %")
  # Create permeability outside range
  test_perm_rng <- c(0.2, 1.5, 3)

  # Run character vector test
  expect_error(import_points(path = bars, type = "Barrier", perm = test_perm_chr),
               "Supplied permeability field cannot be assigned because: ")
  # Run outside range test
  expect_error(import_points(path = bars, type = "Barrier", perm = test_perm_rng),
               "^Supplied permeability field cannot be assigned because:")

})

test_that("Points are imported correctly", {

  # Create points
  bars <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(1,1)),
                                  sf::st_point(c(2,2)),
                                  sf::st_point(c(3,3))))
  expect_equal(nrow(import_points(bars, type = "barriers")), 3)

  # Create permeability values
  bars$permeability <- c(0,0.5,1)
  bars_imported <- import_points(bars, perm = "permeability", type = "barriers")
  expect_equal(bars_imported$perm, bars$permeability)

  # Test sink
  expect_equal(nrow(import_points(bars, type = "sinks")), 3)

  # Test other
  expect_equal(nrow(import_points(bars, type = "others")), 3)
})
