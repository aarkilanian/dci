set_perm <- function(pts, new_perm){
  if(!("barriers" %in% class(pts))) stop("Permeability can only be modified in barrier point objects imported by 'import_points'")
  else{
    pts$perm <- new_perm
  }
  return(pts)
}

set_weight <- function(riv, new_weight){
  if(!("rivers" %in% class(riv))) stop("Weighting can only be modified in river lines objects imported by 'import_rivers'")
  else{
    riv$riv_weight <- new_weight
  }
  return(riv)
}
