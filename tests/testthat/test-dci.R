test_that("Invalid network returns error", {

  expect_error(calculate_dci(1), "A valid river_net object is required.")

})

test_that("Invalid DCI form request returns error", {

  ex <- 1
  class(ex) <- "river_net"
  expect_error(calculate_dci(ex, form = "x"), "A valid form of the DCI must be requested.")

})

test_that("Can find path to root", {

  # Create test node label
  testlabel <- list(c(TRUE, TRUE, FALSE))

  # Run test
  path <- path_to_root(testlabel)
  expect_true(identical(path, list(c(TRUE, TRUE, FALSE), c(TRUE, TRUE), c(TRUE))))

})

test_that("Can find path between segments", {

  # Create test node labels
  testlabel1 <- list(c(TRUE, TRUE))
  testlabel2 <- list(c(TRUE, FALSE, FALSE))

  # Run test
  path <- path_between(testlabel1, testlabel2)
  expect_true(identical(path, list(c(TRUE, TRUE), c(TRUE, FALSE, FALSE), c(TRUE, FALSE))))

})

test_that("Can gather permeabilities", {

  # Import test river and nodes
  net <- readRDS(test_path("testdata", "testnet.rds"))
  nodes <- net %>%
    sfnetworks::activate(nodes) %>%
    as.data.frame()

  # Run test
  expect_equal(gather_perm(0, 2, nodes), 1)

})

test_that("Potamodromous DCI works", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "potamodromous")
  expect_snapshot(dci$DCIs)

})

test_that("Diadromous DCI works", {

  # Load rivnet
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Run test
  dci <- calculate_dci(net, form = "diadromous")
  expect_snapshot(dci$DCIs)

})

