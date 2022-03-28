test_that("non-valid import path returns error", {

  expect_error(import_rivers("invalid_path"), "invalid spatial data provided")
  expect_error(import_points("invalid_path", type = "Barrier"), "invalid spatial data provided")

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

  expect_equal(nrow(import_rivers(rivers, sf = TRUE)), nrow(rivers))

  # Write test rivers to shapefile
  sf::st_write(rivers, test_path("testdata", "test_riv.shp"), append = FALSE)
  expect_equal(nrow(import_rivers(test_path("testdata", "test_riv.shp"), sf = FALSE)), nrow(rivers))

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
  expect_error(import_points(path = bars, type = "Barrier", perm = test_perm_chr, sf = TRUE),
               "Supplied permeability field cannot be assigned because: ")
  # Run outside range test
  expect_error(import_points(path = bars, type = "Barrier", perm = test_perm_rng, sf = TRUE),
               "^Supplied permeability field cannot be assigned because:")

})

test_that("Points are imported correctly", {

  # Create points
  bars <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(1,1)),
                                  sf::st_point(c(2,2)),
                                  sf::st_point(c(3,3))))
  expect_equal(nrow(import_points(bars, type = "Barrier", sf = TRUE)), 3)

  # Create permeability values
  bars$permeability <- c(0,0.5,1)
  bars_imported <- import_points(bars, perm = "permeability", type = "Barrier", sf = TRUE)
  expect_equal(bars_imported$perm, bars$permeability)

  # Test sink
  expect_equal(nrow(import_points(bars, type = "Sink", sf = TRUE)), 3)

  # Test other
  expect_equal(nrow(import_points(bars, type = "Other", sf = TRUE)), 3)
})
