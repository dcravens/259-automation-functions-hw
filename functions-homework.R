#PSYC 259 Homework 4 - Writing functions
#Optional, for extra credit if all questions are answered

#List names of students collaborating with: NA

### SETUP: RUN THIS BEFORE STARTING ----------
library(tidyverse)
set.seed(1)
id <- rep("name", 30)
x <- runif(30, 0, 10)
y <- runif(30, 0, 10)
z <- runif(30, 0, 10)
ds <- tibble(id, x, y, z)

### Question 1 ---------- 

#Vectors x, y, and z contain random numbers between 1 and 10. 
#Write a function called "limit_replace" that will replace values less than 2 or greater than 8 with NA
#Then, run the function on x and save the results to a new vector "x_replace" to show it worked

limit_replace <- function(vec) {
  vec[vec < 2 | vec > 8] <- NA
  return(vec)
}

x_replace <- limit_replace(x)
print(x_replace)

### Question 2 ---------- 

#Make a new version of limit_replace that asks for arguments for a lower bounary and an upper boundary
  #so that they can be customized (instead of being hard coded as 2 and 8)
#Run the function on vector y with boundaries 4 and 6, saving the results to a new vector "y_replace"

limit_replace <- function(vec, lower_bound, upper_bound) {
  vec[vec < lower_bound | vec > upper_bound] <- NA
  return(vec)
}

y_replace <- limit_replace(y, 4, 6)
print(y_replace)

### Question 3 ----------

#Write a function called "plus_minus_SD" that can take one of the vectors (x/y/z) as input
  #and "num_of_SDs" as an input and returns the boundaries +/- num_of_SDs around the mean. 
#plus_minus_SD(x, 1) would give +/- 1SD around the mean of x, plus_minus_SD(y, 2) would give +/- 2SDs around the mean 
#Make num_of_SDs default to 1
#run the new function on x, y, and z with 1 SD

plus_minus_SD <- function(vec, num_of_SDs = 1) {
  mean_val <- mean(vec, na.rm = TRUE)
  sd_val <- sd(vec, na.rm = TRUE)
  
  lower_bound <- mean_val - (num_of_SDs * sd_val)
  upper_bound <- mean_val + (num_of_SDs * sd_val)
  
  return(c(lower_bound, upper_bound))
}

# Running the function on x, y, and z with 1 SD
x_bounds <- plus_minus_SD(x)
y_bounds <- plus_minus_SD(y)
z_bounds <- plus_minus_SD(z)

print(x_bounds)
print(y_bounds)
print(z_bounds)

### Question 4 ----------

#Write an another new version of limit_replace
#This time, make the upper and lower boundaries optional arguments
#If they are not given, use +/- 1 SD as the boundaries (from your plus_minus_SD function)
#Apply the function to each column in ds, and save the results to a new tibble called "ds_replace"

limit_replace <- function(vec, lower_bound = NULL, upper_bound = NULL) {
  if (is.null(lower_bound) | is.null(upper_bound)) {
    bounds <- plus_minus_SD(vec)
    lower_bound <- bounds[1]
    upper_bound <- bounds[2]
  }
  
  vec[vec < lower_bound | vec > upper_bound] <- NA
  return(vec)
}

# Apply the function to each numeric column in ds
ds_replace <- ds %>%
  mutate(across(where(is.numeric), limit_replace))

print(ds_replace)

### Question 5 ----------

#Add a "stopifnot" command to your limit_replace function to make sure it only runs on numeric variables
#Try running it on a non-numeric input (like "id") to make sure it gives you an error

limit_replace <- function(vec, lower_bound = NULL, upper_bound = NULL) {
  stopifnot(is.numeric(vec))  # Ensure the input is numeric
  
  if (is.null(lower_bound) | is.null(upper_bound)) {
    bounds <- plus_minus_SD(vec)
    lower_bound <- bounds[1]
    upper_bound <- bounds[2]
  }
  
  vec[vec < lower_bound | vec > upper_bound] <- NA
  return(vec)
}

# Testing on a non-numeric variable (should give an error)
limit_replace(ds$id)  # This should trigger an error

### Question 6 ----------

#What other requirements on the input do you need to make the function work correctly?
#Add another stopifnot to enforce one more requirement
limit_replace <- function(vec, lower_bound = NULL, upper_bound = NULL) {
  stopifnot(is.numeric(vec))  # Ensure input is numeric
  stopifnot(length(na.omit(vec)) > 0)  # Ensure there's at least one non-NA value
  stopifnot(sd(na.omit(vec)) > 0)  # Ensure there is variability in the data
  
  if (is.null(lower_bound) | is.null(upper_bound)) {
    bounds <- plus_minus_SD(vec)
    lower_bound <- bounds[1]
    upper_bound <- bounds[2]
  }
  
  vec[vec < lower_bound | vec > upper_bound] <- NA
  return(vec)
}

# Testing on an all-NA vector (should give an error)
empty_vec <- c(NA, NA, NA)
limit_replace(empty_vec)  # Should trigger an error

# Testing on a constant vector (should also trigger an error)
constant_vec <- c(5, 5, 5, 5, 5)
limit_replace(constant_vec)  # Should trigger an error

### Question 7 ----------

#Clear out your workspace and load the built-in diamonds dataset by running the lines below
#RUN THIS CODE
rm(list = ls())
library(tidyverse)
ds_diamonds <- diamonds

#Save your two functions to an external file (or files) 
#Then, load your functions from the external files(s)
#Next, run your limit_replace function on all of the numeric columns in the new data set
#and drop any rows with NA, saving it to a new tibble named "ds_trimmed"

# Save plus_minus_SD function
writeLines('
plus_minus_SD <- function(vec, num_of_SDs = 1) {
  mean_val <- mean(vec, na.rm = TRUE)
  sd_val <- sd(vec, na.rm = TRUE)
  
  lower_bound <- mean_val - (num_of_SDs * sd_val)
  upper_bound <- mean_val + (num_of_SDs * sd_val)
  
  return(c(lower_bound, upper_bound))
}
', "plus_minus_SD.R")

# Save limit_replace function
writeLines('
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
', "limit_replace.R")

source("plus_minus_SD.R")
source("limit_replace.R")

ds_trimmed <- ds_diamonds %>%
  mutate(across(where(is.numeric), limit_replace)) %>% 
  drop_na()

print(ds_trimmed)

### Question 8 ----------

#The code below makes graphs of diamond price grouped by different variables
#Refactor it to make it more efficient using functions and/or iteration
#Don't worry about the order of the plots, just find a more efficient way to make all 6 plots
#Each cut (Premium/Ideal/Good) should have a plot with trimmed and untrimmed data
#The title of each plot should indicate which cut and whether it's all vs. trimmed data

ds_diamonds %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, all") + 
  theme_minimal()

ds_diamonds %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, all") +
  theme_minimal()

ds_diamonds %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, all") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, trimmed") + 
  theme_minimal()

ds_trimmed %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, trimmed") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, trimmed") +
  theme_minimal()

# Define the cuts and datasets
cuts <- c("Premium", "Ideal", "Good")
datasets <- list("All" = ds_diamonds, "Trimmed" = ds_trimmed)

# Function to generate boxplots
plot_price_vs_clarity <- function(data, cut_name, dataset_name) {
  data %>%
    filter(cut == cut_name) %>%
    ggplot(aes(x = clarity, y = price)) +
    geom_boxplot() +
    ggtitle(paste(cut_name, dataset_name)) +
    theme_minimal()
}

# Use map() to generate all plots
plots <- expand_grid(cut = cuts, dataset = names(datasets)) %>%
  mutate(plot = map2(dataset, cut, ~plot_price_vs_clarity(datasets[[.x]], .y, .x)))

# Display the plots
plots$plot
