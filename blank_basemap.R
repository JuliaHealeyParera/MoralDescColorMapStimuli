library(tidyverse)
library(RColorBrewer)
library(viridis)
library(here)

###CIRCLE GENERATION###
#Helper functions
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

#Call this function to generate circle!
base_circle <- function(radius, target_perimeter, deviation_factor) {
  circle <- generate_circle(radius)
  circle <- add_deviation_to_circle(circle, deviation_factor)
  circle <- adjust_perimeter(circle, target_perimeter)
  return(circle)
}

#Polygon rotation helper function
rot <- function(df, angle_deg) {
  angle_rad <- angle_deg * (pi / 180)
  
  df_rotated <- df |>
    mutate(
      x_rot = x * cos(angle_rad) - y * sin(angle_rad),
      y_rot = x * sin(angle_rad) + y * cos(angle_rad),
      x = x_rot, 
      y = y_rot
    )
  
  return(df_rotated)
}

###MAP CREATION###
create_map = function(starting_angle) {
  #Declare polygon objects
  obj1 <- base_circle(.7, 3.4, .055)
  obj2 <- base_circle(.7, 3.4, .055)
  obj3 <- base_circle(.7, 3.4, .055)
  obj4 <- base_circle(.7, 3.4, .055)
  
  #Establish map-wise rotation
  first_obj_rad = starting_angle * pi/180
  second_obj_rad = first_obj_rad + pi/2 
  third_obj_rad = first_obj_rad + pi
  fourth_obj_rad = first_obj_rad + 3*pi/2
  
  #Choose angle of internal rotation
  rot_angle <- sample(0:360, size=1)
  rot_rad = rot_angle * pi/180
  
  #Calculate end-goal center coordinates of each polygon
  cntr1_x = cos(first_obj_rad)
  cntr1_y = sin(first_obj_rad)
  cntr2_x = cos(second_obj_rad)
  cntr2_y = sin(second_obj_rad)
  cntr3_x = cos(third_obj_rad)
  cntr3_y = sin(third_obj_rad)
  cntr4_x = cos(fourth_obj_rad)
  cntr4_y = sin(fourth_obj_rad)
  
  #Translating polygons to formerly-calculated center coordinates
  coords1 <- obj1 |>
    mutate(x = x + cntr1_x,
           y = y + cntr1_y)
  coords2 <- obj2 |>
    mutate(x = x + cntr2_x,
           y = y + cntr2_y)
  coords3 <- obj3 |>
    mutate(x = x + cntr3_x,
           y = y + cntr3_y)
  coords4 <- obj4 |>
    mutate(x = x + cntr4_x,
           y = y + cntr4_y)
  
  #Rotate polygons by same random amount because of odd rot() functionality
  coords1 <- rot(coords1, rot_angle)
  coords2 <- rot(coords2, rot_angle)
  coords3 <- rot(coords3, rot_angle)
  coords4 <- rot(coords4, rot_angle)
  
  #Create label overlay with polygon center coordinates
  #Adding rotation from rot() function to label coordinates
  cntr1_x_rot <- cntr1_x * cos(rot_rad) - cntr1_y * sin(rot_rad)
  cntr1_y_rot <- cntr1_x * sin(rot_rad) + cntr1_y * cos(rot_rad)
  cntr2_x_rot <- cntr2_x * cos(rot_rad) - cntr2_y * sin(rot_rad)
  cntr2_y_rot <- cntr2_x * sin(rot_rad) + cntr2_y * cos(rot_rad)
  cntr3_x_rot <- cntr3_x * cos(rot_rad) - cntr3_y * sin(rot_rad)
  cntr3_y_rot <- cntr3_x * sin(rot_rad) + cntr3_y * cos(rot_rad)
  cntr4_x_rot <- cntr4_x * cos(rot_rad) - cntr4_y * sin(rot_rad)
  cntr4_y_rot <- cntr4_x * sin(rot_rad) + cntr4_y * cos(rot_rad)
  
  labels <- data.frame(
    text = c("A", "B", "C", "D"),
    x = c(cntr1_x_rot, cntr2_x_rot, cntr3_x_rot, cntr4_x_rot),
    y = c(cntr1_y_rot, cntr2_y_rot, cntr3_y_rot, cntr4_y_rot))
  
  #Circle qualities pre-selected based on visual salience
  circle <- base_circle(1.6, 8, .055) 
  
  #Plot, store to object
  map <- ggplot() +
    geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
    geom_polygon(data = coords1, aes(x = x, y = y), fill = "white", color  = "grey70") +
    geom_polygon(data = coords2, aes(x = x, y = y), fill = "white", color = "grey70") +
    geom_polygon(data = coords3, aes(x = x, y = y), fill = "white", color = "grey70") +
    geom_polygon(data = coords4, aes(x = x, y = y), fill = "white", color = "grey70") +
    geom_text(data = labels, aes(x = x, y = y, label = text), color = "grey10", size = 12) +
    labs(fill = "Damage") +
    coord_fixed() 
  
  #Add void theme to eliminate grid lines, axes, etc. 
  map <- map + 
    theme_void() +  
    theme(plot.background = element_rect(fill = "white", color = NA),
          legend.key.size = unit(.9, 'cm'), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 20))
  return(map)
}

blank_base <- create_map(0)

path <- here("map_plots", "blank_base", "blank_example_basemap.png") 
ggsave(path, plot = map, width = 10, height = 8, dpi = 300)
