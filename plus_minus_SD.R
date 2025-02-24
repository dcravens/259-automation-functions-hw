
plus_minus_SD <- function(vec, num_of_SDs = 1) {
  mean_val <- mean(vec, na.rm = TRUE)
  sd_val <- sd(vec, na.rm = TRUE)
  
  lower_bound <- mean_val - (num_of_SDs * sd_val)
  upper_bound <- mean_val + (num_of_SDs * sd_val)
  
  return(c(lower_bound, upper_bound))
}

