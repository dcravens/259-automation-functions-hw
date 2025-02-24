
limit_replace <- function(vec, lower_bound = NULL, upper_bound = NULL) {
  stopifnot(is.numeric(vec))  # Ensure input is numeric
  stopifnot(length(na.omit(vec)) > 0)  # Ensure there’s at least one non-NA value
  
  if (is.null(lower_bound) | is.null(upper_bound)) {
    bounds <- plus_minus_SD(vec)
    lower_bound <- bounds[1]
    upper_bound <- bounds[2]
  }
  
  vec[vec < lower_bound | vec > upper_bound] <- NA
  return(vec)
}

