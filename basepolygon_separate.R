# Load required libraries
library(ggplot2)
library(dplyr)

generate_circle <- function(radius) {
  num_points <- 100  # Number of points along the circle
  angles <- seq(0, 2 * pi, length.out = num_points)
  
  circle <- data.frame(
    x = radius * cos(angles),
    y = radius * sin(angles)
  )
  
  return(circle)
}

add_deviation_to_circle <- function(circle, deviation_factor = 0.05) {
  deviation_x <- runif(nrow(circle), -deviation_factor, deviation_factor)
  deviation_y <- runif(nrow(circle), -deviation_factor, deviation_factor)
  circle$x <- circle$x + deviation_x
  circle$y <- circle$y + deviation_y
  return(circle)
}

adjust_perimeter <- function(circle, target_perimeter) {
  perimeter <- sum(sqrt(diff(circle$x)^2 + diff(circle$y)^2))
  scale_factor <- target_perimeter / perimeter
  circle$x <- circle$x * scale_factor
  circle$y <- circle$y * scale_factor
  return(circle)
}

base_circle <- function(radius = 5, target_perimeter = 20, deviation_factor = 0.1) {
  circle <- generate_circle(radius)
  circle <- add_deviation_to_circle(circle, deviation_factor)
  circle <- adjust_perimeter(circle, target_perimeter)
  return(circle)
}
