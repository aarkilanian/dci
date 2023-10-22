test_that("Potamodromous DCI", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "potamodromous")
  expect_snapshot(dci$DCIs)

})

test_that("Potamodromous DCI with weight", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "potamodromous", weight = "riv_weight")
  expect_snapshot(dci$DCIs)

})

test_that("Potamodromous DCI with threshold", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "potamodromous", threshold = 3)
  expect_snapshot(dci$DCIs)

})

test_that("Potamodromous DCI with threshold and weight", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "potamodromous", weight = "riv_weight", threshold = 3)
  expect_snapshot(dci$DCIs)

})

test_that("Diadromous DCI", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "diadromous")
  expect_snapshot(dci$DCIs)

})

test_that("Diadromous DCI with weight", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "diadromous", weight = "riv_weight")
  expect_snapshot(dci$DCIs)

})

test_that("Diadromous DCI with threshold", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "diadromous", threshold = 3)
  expect_snapshot(dci$DCIs)

})

test_that("Diadromous DCI with threshold and weight", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "diadromous", weight = "riv_weight", threshold = 3)
  expect_snapshot(dci$DCIs)

})

test_that("Invasive DCI", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "invasive")
  expect_snapshot(dci$DCIs)

})

test_that("Invasive DCI with weight", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "invasive", weight = "riv_weight")
  expect_snapshot(dci$DCIs)

})

test_that("Invasive DCI with threshold", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "invasive", threshold = 3)
  expect_snapshot(dci$DCIs)

})

test_that("Invasive DCI with threshold and weight", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "invasive", weight = "riv_weight", threshold = 3)
  expect_snapshot(dci$DCIs)

})
