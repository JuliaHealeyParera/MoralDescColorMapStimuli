library(tidyverse)
library(RColorBrewer)
library(viridis)
library(here)
library(cowplot)

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
create_map = function(starting_angle, idx) {
  #Declare polygon objects
  obj1 <- base_circle(.85, 3.8, .045)
  obj2 <- base_circle(.85, 3.8, .045)
  obj3 <- base_circle(.85, 3.8, .045)
  obj4 <- base_circle(.85, 3.8, .045)
  
  #Establish map-wise rotation
  first_obj_rad = (starting_angle * pi/180) + (9*pi)/2
  second_obj_rad = first_obj_rad + (3*pi)/2 
  third_obj_rad = first_obj_rad + (2*pi)/2
  fourth_obj_rad = first_obj_rad + (pi)/2
  
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
           y = y + cntr1_y,
           damage = 0,
           yes_no = 0)
  coords2 <- obj2 |>
    mutate(x = x + cntr2_x,
           y = y + cntr2_y,
           damage = 5, 
           yes_no = 1)
  coords3 <- obj3 |>
    mutate(x = x + cntr3_x,
           y = y + cntr3_y, 
           damage = 5, 
           yes_no = 0)
  coords4 <- obj4 |>
    mutate(x = x + cntr4_x,
           y = y + cntr4_y, 
           damage = 10, 
           yes_no = 1)
  
  #Create label overlay with polygon center coordinates
  #Adding rotation from rot() function to label coordinates
  labels <- data.frame(
    text = c("A", "B", "C", "D"),
    x = c(cntr1_x, cntr2_x, cntr3_x, cntr4_x),
    y = c(cntr1_y, cntr2_y, cntr3_y, cntr4_y))
  #For base map, extra Province label
  province_lab <- data.frame(
    text = c("Province", "Province", "Province", "Province"),
    x = c(cntr1_x, cntr2_x, cntr3_x, cntr4_x),
    y = c(cntr1_y +.075, cntr2_y + .075, cntr3_y + .075, cntr4_y + .075)
  )
  
  #Circle qualities pre-selected based on visual salience
  circle <- base_circle(1.7, 9, .055) 
  
  uninhabited <- data.frame(text = c("Uninhabited", "Area"), x = c(0, 0), y = c(0.1, -0.1))
  yes_no <- data.frame(
    text = c("yes", "no", 'yes', 'no'), 
    x =  c(cntr1_x, cntr2_x, cntr3_x, cntr4_x), 
    y = c(cntr1_y - .15, cntr2_y - .15, cntr3_y - .15, cntr4_y - .15))
  
  #Plot, store to object
  if (idx == 0) {
    labels['y'] <- c(cntr1_y - .075, cntr2_y - 0.075, cntr3_y - 0.075, cntr4_y - 0.07)
    
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y), fill = "white", color  = "grey70") +
      geom_polygon(data = coords2, aes(x = x, y = y), fill = "white", color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y), fill = "white", color = "grey70") +
      geom_polygon(data = coords4, aes(x = x, y = y), fill = "white", color = "grey70") +
      geom_text(data = labels, aes(x = x, y = y, label = text), color = "grey10", size = 11) +
      geom_text(data = province_lab, aes(x = x, y = y, label = text), color = "grey10", size = 8) +
      geom_text(data = uninhabited, aes(x = x, y = y, label = text), color = "grey10", size = 10) +
      coord_fixed()
  } else if (idx == 1) {
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "black") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "black") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_text(data = labels, aes(x = x, y = y, label = text), color = "grey10", size = 12) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) + 
      coord_fixed()
  } else if (idx == 2) {
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), alpha = 1, color  = "grey70") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), alpha = .5, color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), alpha = 1, color = "grey70") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), alpha = .5, color = "grey70") +
      geom_text(data = labels, aes(x = x, y = y, label = text), color = "grey10", size = 12) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
      guides(alpha = "none") +
      coord_fixed()
  } else if (idx == 3) {
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), alpha = 1, color  = "grey70") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), alpha = .5, color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), alpha = 1, color = "grey70") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), alpha = .5, color = "grey70") +
      geom_text(data = labels, aes(x = x, y = y, label = text), color = "grey10", size = 12) +
      geom_text(data = yes_no, aes(x = x, y = y, label = text), color = "grey10", size = 7) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
      guides(alpha = "none") +
      coord_fixed()
  } else if (idx == 5) {
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "black") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "black") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_text(data = labels, aes(x = x, y = y, label = text), color = "grey10", size = 12) +
      geom_text(data = yes_no, aes(x = x, y = y, label = text), color = "grey10", size = 7) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
      coord_fixed() 
  } else if (idx == 6) {
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), alpha = 1, color  = "black") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), alpha = .5, color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), alpha = 1, color = "black") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), alpha = .5, color = "grey70") +
      geom_text(data = labels, aes(x = x, y = y, label = text), color = "grey10", size = 12) +
      geom_text(data = yes_no, aes(x = x, y = y, label = text), color = "grey10", size = 7) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
      coord_fixed() 
  } else {
      map <- ggplot() +
        geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
        geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "grey70") +
        geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
        geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "grey70") +
        geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
        geom_text(data = labels, aes(x = x, y = y, label = text), color = "grey10", size = 12) +
        geom_text(data = yes_no, aes(x = x, y = y, label = text), color = "grey10", size = 7) +
        scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
        coord_fixed() 
  }

  #Add void theme to eliminate grid lines, axes, etc. 
  map <- map +
    theme_void() +  
    theme(plot.background = element_rect(fill = "white", color = NA),
          legend.key.size = unit(.9, 'cm'), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 20)) +
    labs(fill = "Damage Level")
  return(map)
}

blank_base <- create_map(0, 0) 
blank_base
grayed_map <- create_map(0, 2) 
grayed_and_labeled_map <- create_map(0,3)
labeled_map <- create_map(0, 7)
outlined_map <- create_map(0,1)
outlined_and_labeled_map <- create_map(0,5)
outline_gray_label_map <- create_map(0,6)

path <- here("map_plots", "blank_base", "blank_example_basemap.png") 
ggsave(path, plot = blank_base, width = 10, height = 8, dpi = 300)

grayed_path <- here("map_plots", "vote_prototypes", "grayed_votes.png") 
ggsave(grayed_path, plot = grayed_map, width = 10, height = 8, dpi = 300)

gray_label_path <- here("map_plots", "vote_prototypes", "grayed_labeled_votes.png") 
ggsave(gray_label_path, plot = grayed_and_labeled_map, width = 10, height = 8, dpi = 300)

label_path <- here("map_plots", "vote_prototypes", "labeled_votes.png") 
ggsave(label_path, plot = labeled_map, width = 10, height = 8, dpi = 300)

outline_path <- here("map_plots", "vote_prototypes", "outlined_votes.png") 
ggsave(outline_path, plot = outlined_map, width = 10, height = 8, dpi = 300)

outline_label_path <- here("map_plots", "vote_prototypes", "outlined_labeled_votes.png") 
ggsave(outline_label_path, plot = outlined_and_labeled_map, width = 10, height = 8, dpi = 300)

outline_gray_label_path <- here("map_plots", "vote_prototypes", "outlined_labeled_grayed_votes.png") 
ggsave(outline_gray_label_path, plot = outline_gray_label_map, width = 10, height = 8, dpi = 300)
