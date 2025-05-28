## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(dci)
library(dplyr)

## ----riverData, fig.width=8, fig.height=4-------------------------------------
# Load Yamaska river data
data("yamaska_rivers", package = "dci")
# Import rivers
rivers_in <- import_rivers(rivers = yamaska_rivers)

## ----pointData----------------------------------------------------------------
# Load Yamaska barrier and outlet data
data("yamaska_barriers", package = "dci")
data("yamaska_outlet", package = "dci")
# Import barriers
barriers_in <- import_points(pts = yamaska_barriers, type = "bars")
outlet_in <- import_points(pts = yamaska_outlet, type = "out")
barriers_in

## ----river_net, fig.width=8, fig.height=4-------------------------------------
# Combine rivers, barriers, and outlet
# Warnings about river splitting suppressed since rivers are already split at barrier node locations
net <- suppressWarnings(
  river_net(rivers = rivers_in, barriers = barriers_in, outlet = outlet_in) 
)
plot(net)

## ----findErrors---------------------------------------------------------------
# Identify topological errors in rivers
river_errors <- enforce_dendritic(rivers = rivers_in, correct = FALSE)
river_errors

## ----plotErrors, fig.width=8, fig.height=4------------------------------------
# Visualize divergence locations
plot(sf::st_geometry(rivers_in), col = "grey75")
plot(river_errors["divergent"], add = TRUE)

# Visualize complex confluence locations
plot(sf::st_geometry(rivers_in), col = "grey75")
plot(river_errors["complexID"], add = TRUE)

## ----calculateDCI-------------------------------------------------------------
yamaska_res <- calculate_dci(net = net,
                             form = "pot",
                             pass = "pass_1",
                             weight = NULL,
                             threshold = NULL)
yamaska_res

## ----thresholdDCI-------------------------------------------------------------
yamaska_res_thr <- calculate_dci(net = net,
                            form = "pot",
                            pass = "pass_1",
                            weight = NULL,
                            threshold = 1500)
yamaska_res_thr

## ----weightedDCI--------------------------------------------------------------
yamaska_res_w <- calculate_dci(net = net,
                             form = "pot",
                             pass = "pass_1",
                             weight = "qual",
                             threshold = NULL)
yamaska_res_w

## ----export, fig.width=8, fig.height=4----------------------------------------
# Results can be rejoined to the river lines
res_riv <- export_dci(net = net, results = yamaska_res, type = "rivers")

# Results can also be rejoined to the barriers
res_bar <- export_dci(net = net, results = yamaska_res, type = "bars")

## ----ggplot, eval=FALSE-------------------------------------------------------
# # Visualize river network with nodes coloured according to their type
# ggplot() +
#   geom_sf(data = net %>% tidygraph::activate(edges) %>% sf::st_as_sf()) +
#   geom_sf(data = net %>% tidygraph::activate(nodes) %>% sf::st_as_sf(), aes(col = type))
# 
# # Visualize the DCI color coded river network with the barriers
# ggplot() +
#   geom_sf(data = res_riv, aes(col = DCI)) +
#   geom_sf(data = net %>% tidygraph::activate(nodes) %>% sf::st_as_sf() %>% dplyr::filter(type == "barrier"))

