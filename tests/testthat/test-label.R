test_that("Correct node labels created", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  net_label <- suppressWarnings(node_labeling(net))
  labels <- net_label %>%
    sfnetworks::activate(nodes) %>%
    dplyr::pull(node_label)
  expect_snapshot(labels)
})

test_that("Correct member labels created", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  net_label <- suppressWarnings(membership_labeling(net))
  labels <- net_label %>%
    sfnetworks::activate(nodes) %>%
    dplyr::pull(member_label)
  expect_snapshot(labels)
})
