test_that("rivnet captures all points within tolerance", {

  # Import spatial data
  rivers <- import_rivers(path = test_path("testdata", "yam_subset.shp"))
  barriers <- import_points(path = test_path("testdata", "yam_bars_subset.shp"), type = "Barrier")
  sinks <- import_points(path = test_path("testdata", "yam_sink.shp"), type = "Sink")
  others <- import_points(path = test_path("testdata", "yam_extra_subset.shp"), type = "Other")

  # Create rivnet object
  net <- new_rivnet(rivers, barriers, sinks, others=others)

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
