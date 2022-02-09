enforce_dendritic <- function(river_net){

  # Identify diverging streams
  div_riv <- river_net %>%
    sfnetworks::activate(edges) %>%
    data.frame() %>%
    dplyr::mutate(from_grp = forcats::fct_lump_min(as.factor(from), min = 2)) %>%
    dplyr::filter(from_grp != "Other") %>%
    dplyr::select(rivID, from)

}
