riv_correct <- sf::st_sfc(sf::st_linestring(matrix(c(1,2,1,2), 2)),
                          sf::st_linestring(matrix(c(1,2,3,2), 2)),
                          sf::st_linestring(matrix(c(2,3,2,3), 2)),
                          sf::st_linestring(matrix(c(1,3,4,3), 2)),
                          sf::st_linestring(matrix(c(3,4,3,3), 2)))

riv_unsplit <- sf::st_sfc(sf::st_linestring(matrix(c(1,3,1,3), 2)),
                   sf::st_linestring(matrix(c(3,1,3,4), 2)),
                   sf::st_linestring(matrix(c(3,4,3,3), 2)),
                   sf::st_linestring(matrix(c(1,2,3,2), 2)))

riv_divergent <- st_sfc(sf::st_linestring(matrix(c(1,2,2,1), 2)),
                        sf::st_linestring(matrix(c(1,2,2,3), 2)),
                        sf::st_linestring(matrix(c(2,3,1,2), 2)),
                        sf::st_linestring(matrix(c(2,3,3,2), 2)),
                        sf::st_linestring(matrix(c(3,4,2,3), 2)),
                        sf::st_linestring(matrix(c(3,4,4,3), 2)),
                        sf::st_linestring(matrix(c(4,5,3,3), 2)))

riv_tri <- st_sfc(sf::st_linestring(matrix(c(1,2,1,2), 2)),
                  sf::st_linestring(matrix(c(1,2,3,2), 2)),
                  sf::st_linestring(matrix(c(3,2,1,2), 2)),
                  sf::st_linestring(matrix(c(2,3,2,3), 2)),
                  sf::st_linestring(matrix(c(2,3,4,3), 2)),
                  sf::st_linestring(matrix(c(3,4,3,3), 2)))

riv_multi <- st_sfc(sf::st_linestring(matrix(c(2,3,1,2), 2)),
                    sf::st_linestring(matrix(c(4,3,1,2), 2)),
                    sf::st_linestring(matrix(c(2,3,3,2), 2)),
                    sf::st_linestring(matrix(c(3,4,2,3), 2)),
                    sf::st_linestring(matrix(c(4,5,3,3), 2)),
                    sf::st_linestring(matrix(c(4,3,3,5), 2)),
                    sf::st_linestring(matrix(c(3,2,5,4), 2)),
                    sf::st_linestring(matrix(c(2,1,4,5), 2)),
                    sf::st_linestring(matrix(c(1,2,5,6), 2)),
                    sf::st_linestring(matrix(c(2,3,6,5), 2)))

test_that("rivers are split correctly in river preparation", {
  expect_equal(length(prepare_rivers(riv_unsplit)), 5)
})
