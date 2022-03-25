test_that("Correct node labels created", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "test_net.rds"))

  # Run test
  net_label <- node_labeling(net)
  labels <- net_label %>% sfnetworks::activate(nodes) %>% dplyr::pull(node.label)
  expect_snapshot(labels)

})

test_that("Correct member labels created", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "test_net.rds"))

  # Run test
  net_label <- membership_labeling(net)
  labels <- net_label %>% sfnetworks::activate(nodes) %>% dplyr::pull(member.label)
  expect_snapshot(labels)

})
