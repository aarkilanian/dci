test_that("Simple river network is created", {

  # Get network
  net <- readRDS(test_path("testdata", "testnet.rds"))

  # Get rivers
  rivs <- net %>% sfnetworks::activate(edges) %>% sf::st_as_sf()
  rivs <- structure(rivs, class = c("rivers", class(rivs)))
  # Get barriers
  bars <- net %>% sfnetworks::activate(nodes) %>% dplyr::filter(type == "barrier") %>% sf::st_as_sf()
  bars <- structure(bars, class = c("barriers", class(bars)))
  # Get outlet
  outlet <- net %>% sfnetworks::activate(nodes) %>% dplyr::filter(type == "outlet") %>% sf::st_as_sf()
  outlet <- structure(outlet, class = c("outlet", class(outlet)))


  # Create river network
  net2 <- suppressWarnings(river_net(rivers = rivs, barriers = bars, outlet = outlet))

  expect_equal(any(class(net2) == "river_net"), TRUE)

})
