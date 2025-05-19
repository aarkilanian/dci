test_that("Potamodromous DCI", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "potamodromous", pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 93.72)
})

test_that("Potamodromous DCI with weight", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "potamodromous", weight = "riv_weight", pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 95.60)
})

test_that("Potamodromous DCI with threshold", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "potamodromous", threshold = 3, pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 56.71)
})

test_that("Potamodromous DCI with threshold and weight", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "potamodromous", weight = "riv_weight", threshold = 3, pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 87.87)
})

test_that("Diadromous DCI", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "diadromous", pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 68.46)
})

test_that("Diadromous DCI with weight", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "diadromous", weight = "riv_weight", pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 75.61)
})

test_that("Diadromous DCI with threshold", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "diadromous", threshold = 3, pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 10.52)
})

test_that("Diadromous DCI with threshold and weight", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "diadromous", weight = "riv_weight", threshold = 3, pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 22.18)
})
