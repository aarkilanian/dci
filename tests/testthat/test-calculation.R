test_that("Potamodromous DCI", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "pot", pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 93.72)
})

test_that("Potamodromous DCI (parallel)", {

  # Skip on Github Actions
  skip_on_ci()

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Set future
  future::plan("multisession", workers = 2)

  # Run test
  dci <- calculate_dci(net, form = "pot", pass = "pass_1", quiet = TRUE,
                       parallel = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 93.72)
})

test_that("Potamodromous DCI with weight", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "pot", weight = "riv_weight", pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 95.60)
})

test_that("Potamodromous DCI with threshold", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "pot", threshold = 3, pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 56.71)
})

test_that("Potamodromous DCI with threshold and weight", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "pot", weight = "riv_weight", threshold = 3, pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 87.87)
})

test_that("Diadromous DCI", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "dia", pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 90.63)
})

test_that("Diadromous DCI with weight", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "dia", weight = "riv_weight", pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 90.54)
})

test_that("Diadromous DCI with threshold", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "dia", threshold = 3, pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 32.69)
})

test_that("Diadromous DCI with threshold and weight", {
  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "dia", weight = "riv_weight", threshold = 3, pass = "pass_1", quiet = TRUE)
  expect_equal(round(sum(dci$DCI), 2), 37.11)
})
