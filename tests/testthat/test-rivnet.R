test_that("rivnet captures all points within tolerance", {

  rivers <- import_rivers(path = "local/yam_subset.shp")
  barriers <- import_points("local/yam_bars_subset.shp", type = "Barrier")
  sinks <- import_points("local/yam_sink.shp", type = "Sink")
  others <- import_points("local/yam_extra_subset.shp", type = "Other")
  net <- new_rivnet(rivers, barriers, sinks, others)

  expect_equal(nrow(net %>%
                 sfnetworks::activate(nodes) %>%
                 dplyr::filter(type == "Barrier") %>%
                 as.data.frame()), nrow(barriers))
  expect_equal(nrow(net %>%
                      sfnetworks::activate(nodes) %>%
                      dplyr::filter(type == "Sink") %>%
                      as.data.frame()), nrow(sinks))
  expect_equal(nrow(net %>%
                      sfnetworks::activate(nodes) %>%
                      dplyr::filter(type == "Other") %>%
                      as.data.frame()), nrow(others))
})
