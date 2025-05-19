test_that("exporting works correctly for both rivers and barriers", {

  # Import river network
  net <- readRDS(test_path("testdata", "testnet.rds"))
  # Import dci results
  res <- readRDS(test_path("testdata", "dci_res.rds"))

  # Export to rivers
  riv_exp <- export_dci(net = net, results = res, type = "rivers")
  # Export to barriers
  bar_exp <- export_dci(net = net, results = res, type = "barriers")

  # Test
  expect_equal(all(c("DCI", "DCI_rel") %in% colnames(riv_exp)), TRUE)
  expect_equal(all(c("DCI", "DCI_rel") %in% colnames(bar_exp)), TRUE)

})
