test_riv <- st_sfc(st_linestring(matrix(c(1,3,1,3), 2)),
                   st_linestring(matrix(c(3,1,3,4), 2)),
                   st_linestring(matrix(c(3,4,3,3), 2)),
                   st_linestring(matrix(c(1,2,3,2), 2)))

test_that("rivers are split correctly in river preparation", {
  expect_equal(length(prepare_rivers(test_riv)), 5)
})
