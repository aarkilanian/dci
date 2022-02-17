enforce_dendritic <- function(river_net, output_errors){

  # Identify diverging streams
  div_riv <- river_net %>%
    sfnetworks::activate(edges) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(rivID = 1:dplyr::n()) %>%
    dplyr::mutate(from_grp = forcats::fct_lump_min(as.factor(from), min = 2)) %>%
    dplyr::filter(from_grp != "Other") %>%
    dplyr::select(rivID, from) %>%
    dplyr::group_by(from) %>%
    dplyr::mutate(divID = dplyr::cur_group_id())

  # Skip correction if no divergences are found
  if(nrow(div_riv) == 0){
    # Issue message to user if no divergences found
    message("No divergences found.")
    # Return unchanged rivers
    rivers <-  data.frame(sfnetworks::activate(river_net, edges))
    rivers <- rivers[, !names(rivers) %in% c("from", "to")]
    invisible(rivers)
  } else {
    # Return divergent errors in spatial format
    if(output_errors){
      div_join <- river_net %>%
        sfnetworks::activate(edges) %>%
        data.frame() %>%
        dplyr::left_join(div_riv %>% dplyr::select(rivID, divID), by = rivID) %>%
        sf::st_as_sf()
      # Correct divergences automatically
    } else {

    }
  }
}
