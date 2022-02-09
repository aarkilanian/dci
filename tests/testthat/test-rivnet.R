load("../resources/topologies.rda")

test_that("rivers are split correctly in river preparation for 5 topologies", {
  expect_equal(length(sf::st_geometry(resplit_rivers(riv_correct))), 5)
  expect_equal(length(sf::st_geometry(resplit_rivers(riv_unsplit))), 5)
  expect_equal(length(sf::st_geometry(resplit_rivers(riv_divergent))), 7)
  expect_equal(length(sf::st_geometry(resplit_rivers(riv_tri))), 6)
  expect_equal(length(sf::st_geometry(resplit_rivers(riv_multi))), 10)
})
