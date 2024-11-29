# Load required libraries
library(ggplot2)
library(dplyr)

# Function to create a square with equidistant vertices
generate_square <- function(side_length) {
  # Define the four corners of the square
  square <- data.frame(
    x = c(0, side_length, side_length, 0, 0),  # X-coordinates
    y = c(0, 0, side_length, side_length, 0)   # Y-coordinates
  )
  return(square)
}

# Function to add deviations along the edges of the square
add_edge_deviations <- function(square, deviation_factor = 0.05) {
  # For each side of the square, add deviations
  set.seed(42)  # For reproducibility
  num_points_per_side <- 20  # Number of points per side
  
  # Function to add deviations to a single edge (line)
  add_deviation_to_edge <- function(x_start, y_start, x_end, y_end) {
    # Generate points along the edge
    x_vals <- seq(x_start, x_end, length.out = num_points_per_side)
    y_vals <- seq(y_start, y_end, length.out = num_points_per_side)
    
    # Add random deviations to these points
    deviation_x <- runif(length(x_vals), -deviation_factor, deviation_factor)
    deviation_y <- runif(length(y_vals), -deviation_factor, deviation_factor)
    
    # Apply deviations
    x_vals <- x_vals + deviation_x
    y_vals <- y_vals + deviation_y
    
    return(data.frame(x = x_vals, y = y_vals))
  }
  
  # Apply deviations to each side of the square
  deviations <- list()
  deviations[[1]] <- add_deviation_to_edge(square$x[1], square$y[1], square$x[2], square$y[2])  # Bottom
  deviations[[2]] <- add_deviation_to_edge(square$x[2], square$y[2], square$x[3], square$y[3])  # Right
  deviations[[3]] <- add_deviation_to_edge(square$x[3], square$y[3], square$x[4], square$y[4])  # Top
  deviations[[4]] <- add_deviation_to_edge(square$x[4], square$y[4], square$x[1], square$y[1])  # Left
  
  # Combine all the deviations
  polygon <- do.call(rbind, deviations)
  return(polygon)
}

# Function to adjust the perimeter of the polygon
adjust_perimeter <- function(polygon, target_perimeter) {
  # Calculate the current perimeter of the polygon
  perimeter <- sum(sqrt(diff(polygon$x)^2 + diff(polygon$y)^2))
  
  # Calculate scaling factor to match the target perimeter
  scale_factor <- target_perimeter / perimeter
  
  # Scale the polygon
  polygon$x <- polygon$x * scale_factor
  polygon$y <- polygon$y * scale_factor
  return(polygon)
}

# Main function to generate the square with deviations and a specified perimeter
generate_square_with_deviations <- function(side_length = 5, target_perimeter = 20, deviation_factor = 0.1) {
  # Step 1: Generate a regular square
  square <- generate_square(side_length)
  
  # Step 2: Add deviations to the edges
  polygon <- add_edge_deviations(square, deviation_factor)
  
  # Step 3: Adjust the perimeter to match the target value
  polygon <- adjust_perimeter(polygon, target_perimeter)
  
  return(polygon)
}

# Set desired parameters
side_length <- 1  # Initial side length of the square
target_perimeter <- 4  # Desired perimeter length
deviation_factor <- 0.03  # Deviation factor to introduce irregularity

# Generate the polygon with deviations
polygon <- generate_square_with_deviations(side_length, target_perimeter, deviation_factor)

# Plot the polygon using ggplot2
ggplot(polygon, aes(x=x, y=y)) +
  geom_polygon(fill = "lightblue", color = "blue") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Square with Deviations (Approx. Desired Perimeter)")

