set_perm <- function(pts, new_perm){
  if(!("barriers" %in% class(pts))) stop("Permeability can only be modified in barrier point objects imported by 'import_points'.")
  else{
    # Check that permeability is numeric
    if(!(is.numeric(new_perm))) stop("Permeability values must be numeric.")
    # Check that permeability values are between 0 and 1
    if(!(new_perm >= 0 & new_perm <= 1)) stop("Permeability values must be between 0 and 1.")
    pts$perm <- new_perm
  }
  return(pts)
}

set_weight <- function(riv, new_weight){
  if(!("rivers" %in% class(riv))) stop("Weighting can only be modified in river lines objects imported by 'import_rivers'")
  else{
    # Check that weighting is numeric
    if(!(is.numeric(new_weight))) stop("Weighting values must be numeric.")
    # Correct weighting range if needed (between 0 and 1)
    if(!(new_weight >= 0 & new_weight <= 1)){
      message("Weighting values are not between 0 and 1, normalizing values.")
      new_weight <- (new_weight - min(new_weight)) / (max(new_weight) - min(new_weight))
    }
    riv$riv_weight <- new_weight
  }
  return(riv)
}
