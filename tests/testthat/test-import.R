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
