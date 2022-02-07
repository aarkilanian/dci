test_riv <- st_sfc(st_linestring(matrix(c(1,3,1,3), 2)),
                   st_linestring(matrix(c(3,1,3,4), 2)),
                   st_linestring(matrix(c(3,4,3,3), 2)),
                   st_linestring(matrix(c(1,2,3,2), 2)))

test_that("number of extracted nodes matches confluence and sources", {
  expect_equal(length(sf::st_cast(extract_nodes(test_riv), "POINT")), 6)
})

test_that("number of split rivers matches expectation", {
  expect_equal(length(prepare_rivers(test_riv)), 5)
})
