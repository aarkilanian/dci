enforce_dendritic <- function(river_net){

  # Identify diverging streams
  div_riv <- river_net %>%
    sfnetworks::activate(edges) %>%
    data.frame() %>%
    dplyr::mutate(rivID = 1:dplyr::n()) %>%
    dplyr::mutate(from_grp = forcats::fct_lump_min(as.factor(from), min = 2)) %>%
    dplyr::filter(from_grp != "Other") %>%
    dplyr::select(rivID, from)

  # Skip correction if no divergences are found
  if(nrow(div.riv) = 0){
    # Issue message to user if no divergences found
    message("No divergences found.")
    break()
  }

}
