library(tidyverse)
library(RColorBrewer)
library(viridis)
library(here)
library(cowplot)

##TO DO: 
  #Add symbol as argument to create_map 
  #Find symbols that properly render after being pasted over 

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
           damage = 10,
           yes_no = 0)
  coords2 <- obj2 |>
    mutate(x = x + cntr2_x,
           y = y + cntr2_y,
           damage = 3, 
           yes_no = 1)
  coords3 <- obj3 |>
    mutate(x = x + cntr3_x,
           y = y + cntr3_y, 
           damage = 3, 
           yes_no = 0)
  coords4 <- obj4 |>
    mutate(x = x + cntr4_x,
           y = y + cntr4_y, 
           damage = 2, 
           yes_no = 1)
  
  #Create label overlay with polygon center coordinates
  #Adding rotation from rot() function to label coordinates
  labels <- data.frame(
    text = c("A", "B", "C", "D"),
    x = c(cntr1_x, cntr2_x, cntr3_x, cntr4_x),
    y = c(cntr1_y, cntr2_y, cntr3_y, cntr4_y))
  
  #Circle qualities pre-selected based on visual salience
  circle <- base_circle(1.7, 9, .055) 
  
  uninhabited <- data.frame(text = c("Uninhabited", "Area"), x = c(0, 0), y = c(0.1, -0.1))
  
  yes_no <- data.frame(
    state = c('A', 'B', 'C', 'D'),
    text = c("no", "yes", 'no', 'yes'))
    
  labels <- labels |>
    left_join(yes_no, by = join_by(text == state)) |>
    mutate(textplot = ifelse(text.y == "yes", paste0(text, '*'), text)) 
  
  #Plot, store to object
  if (idx == 0) {
    #Outside provinces, within base polygon
    labels <- labels |> 
      mutate(y = case_when(text == "A" ~ y-.61,
                           text == "C" ~ y+.625,
                           TRUE ~ y),
             x = case_when(text == "D" ~ x+.615,
                           text == "B" ~ x-.6,
                           TRUE ~ x))
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "grey70") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_text(data = labels, aes(x = x, y = y, label = textplot), color = "grey10", size = 12) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
      coord_fixed() 
  } else if (idx == 1) {
    #Below provinces
    labels <- labels |> 
      mutate(y = y - .625)
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "grey70") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_text(data = labels, aes(x = x, y = y, label = textplot), color = "grey10", size = 12) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
      coord_fixed()  
  } else if (idx == 2) {
    #Outside polygons, outside base polygon -- asterisk
    labels <- labels |> 
      mutate(y = case_when(text == "A" ~ y+.635,
                           text == "C" ~ y-.635,
                           TRUE ~ y),
             x = case_when(text == "D" ~ x-.635,
                           text == "B" ~ x+.635,
                           TRUE ~ x))
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "grey70") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_segment(aes(x = cntr2_x - .075, xend = cntr2_x + .075, y = cntr2_y - .125, yend = cntr2_y - .13)) + 
      geom_segment(aes(x = cntr4_x - .075, xend = cntr4_x + .075, y = cntr4_y - .125, yend = cntr4_y - .13)) + 
      geom_text(data = labels, aes(x = x, y = y, label = textplot), color = "grey10", size = 12) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
      coord_fixed()  
  } else if (idx == 3) {
    #Outside polygons, outside base polygon -- underline
    a_y_adj <- .635
    c_y_adj <- -.635
    d_x_adj <- -.632
    b_x_adj <- .6325
    
    labels <- labels |> 
      mutate(y = case_when(text == "A" ~ y + a_y_adj,
                           text == "C" ~ y + c_y_adj,
                           TRUE ~ y),
             x = case_when(text == "D" ~ x + d_x_adj,
                           text == "B" ~ x + b_x_adj,
                           TRUE ~ x))
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "grey70") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
      #A - geom_segment(aes(x = cntr1_x - .075, xend = cntr1_x + .075, y = cntr1_y + a_y_adj - .125, yend = cntr1_y + a_y_adj - .13)) + 
      geom_segment(aes(x = cntr2_x + b_x_adj - .08, xend = cntr2_x + b_x_adj + .08, y = cntr2_y - .15, yend = cntr2_y - .15)) + 
      #C - geom_segment(aes(x = cntr3_x - .075, xend = cntr3_x + .075, y = cntr3_y + c_y_adj - .125, yend = cntr3_y + c_y_adj - .13)) + 
      geom_segment(aes(x = cntr4_x + d_x_adj - .08, xend = cntr4_x + d_x_adj + .08, y = cntr4_y - .15, yend = cntr4_y - .15)) + 
      geom_text(data = labels, aes(x = x, y = y, label = text), color = "grey10", size = 12) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
      coord_fixed() 
  }
  else if (idx == 4) {
    label_circle_a <- generate_circle(.155) |>
      mutate(x = x + cntr1_x, 
             y = y + cntr1_y)
    label_circle_b <- generate_circle(.185) |>
      mutate(x = x + cntr2_x,
             y = y + cntr2_y)
    label_circle_c <- generate_circle(.15) |> 
      mutate(x = x + cntr3_x, 
             y = y + cntr3_y)
    label_circle_d <- generate_circle(.185) |>
      mutate(x = x + cntr4_x, 
             y = y + cntr4_y)
    
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "grey70") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = label_circle_a, aes(x = x, y = y), fill = "white", color = "white") +
      geom_polygon(data = label_circle_b, aes(x = x, y = y), fill = "white", color = "white") +
      geom_polygon(data = label_circle_c, aes(x = x, y = y), fill = "white", color = "white") +
      geom_polygon(data = label_circle_d, aes(x = x, y = y), fill = "white", color = "white") +
      geom_text(data = labels, aes(x = x, y = y, label = textplot), color = "grey10", size = 12) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
      coord_fixed() 
    } else if (idx == 5) {
    label_circle_a <- generate_circle(.155) |>
      mutate(x = x + cntr1_x, 
             y = y + cntr1_y)
    label_circle_b <- generate_circle(.185) |>
      mutate(x = x + cntr2_x,
             y = y + cntr2_y)
    label_circle_c <- generate_circle(.15) |> 
      mutate(x = x + cntr3_x, 
             y = y + cntr3_y)
    label_circle_d <- generate_circle(.185) |>
      mutate(x = x + cntr4_x, 
             y = y + cntr4_y)
    
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "grey70") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = label_circle_a, aes(x = x, y = y), fill = "white", color = "white") +
      geom_polygon(data = label_circle_b, aes(x = x, y = y), fill = "white", color = "black") +
      geom_polygon(data = label_circle_c, aes(x = x, y = y), fill = "white", color = "white") +
      geom_polygon(data = label_circle_d, aes(x = x, y = y), fill = "white", color = "black") +
      geom_text(data = labels, aes(x = x, y = y, label = textplot), color = "grey10", size = 12) +
      scale_fill_distiller(direction = 1, palette = "Blues", limits = c(0, 10)) +
      coord_fixed() 
  }
  else {
    #Underline
    #To include symbol, switch label = text in geom_text to label = textplot
    vote_yes <- labels |> 
      filter(text.y == "yes")
    vote_no <- labels |> 
      filter(text.y == "no")
    
    map <- ggplot() +
      geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
      geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "grey70") +
      geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
      geom_text(data = vote_yes, aes(x = x, y = y, label = text), fontface = "bold.italic", color = "black", size = 12) +
      geom_text(data = vote_no, aes(x = x, y = y, label = text), color = "grey15", size = 12) +
      scale_fill_viridis_c(option = "rocket", begin = .2, end = 1, direction = -1, limits = c(0, 10)) + 
      #A - geom_segment(aes(x = cntr1_x - .09, xend = cntr1_x + .09, y = cntr1_y - .125, yend = cntr1_y - .13), linewidth = 1.2) + 
      geom_segment(aes(x = cntr2_x - .075, xend = cntr2_x + .075, y = cntr2_y - .09, yend = cntr2_y - .09), linewidth = 1.2) + 
      #C - geom_segment(aes(x = cntr3_x - .09, xend = cntr3_x + .09, y = cntr3_y - .125, yend = cntr3_y - .13), linewidth = 1.2) + 
      geom_segment(aes(x = cntr4_x - .075, xend = cntr4_x + .075, y = cntr4_y - .09, yend = cntr4_y - .09), linewidth  = 1.2) + 
      coord_fixed() 
  }
  
  #Add void theme to eliminate grid lines, axes, etc. 
  map <- map +
    theme_void() +  
    theme(plot.background = element_rect(fill = "white", color = NA),
          legend.key.size = unit(.9, 'cm'), 
          legend.text = element_text(size = 15), 
          legend.title = element_text(size = 20),
          legend.title.align = 0.5,
          legend.margin = margin(t = 15)) +
    labs(fill = "Damage Level")
  return(map)
}

#Ast. above or below 
#thicker underline, try agian

labels_inside_base <- create_map(0, 0)
labels_below_prov <- create_map(0, 1)
labels_outside_base <- create_map(0, 2)
underline <- create_map(0, 3)
white_circle_labels <- create_map(0, 4)
white_circle_outline <- create_map(0,5)
bold_underline_italic <- create_map(0,6) ##CHOSEN 

lev_10_3_3_2 <- create_map(0,6)
lev_8_6_4_2 <- create_map(0,6)

path <- here("map_plots", "vote_prototypes", "labels_inside_base.png") 
ggsave(path, plot = labels_inside_base, width = 10, height = 8, dpi = 300)

path <- here("map_plots", "vote_prototypes", "labels_below_prov.png") 
ggsave(path, plot = labels_below_prov, width = 10, height = 8, dpi = 300)

path <- here("map_plots", "vote_prototypes", "labels_outside_base.png") 
ggsave(path, plot = labels_outside_base, width = 10, height = 8, dpi = 300)

path <- here("map_plots", "vote_prototypes", "underline.png") 
ggsave(path, plot = underline, width = 10, height = 8, dpi = 300)

path <- here("map_plots", "vote_prototypes", "white_circle_labels.png") 
ggsave(path, plot = white_circle_labels, width = 10, height = 8, dpi = 300)

path <- here("map_plots", "vote_prototypes", "white_circle_outline.png") 
ggsave(path, plot = white_circle_outline, width = 10, height = 8, dpi = 300)

path <- here("map_plots", "vote_prototypes", "bold_italic.png") 
ggsave(path, plot = bold_underline_italic, width = 10, height = 8, dpi = 300)

path <- here("map_plots", "vote_prototypes", "lev_10_3_3_2.png") 
ggsave(path, plot = lev_10_3_3_2, width = 10, height = 8, dpi = 300)

path <- here("map_plots", "vote_prototypes", "lev_8_6_4_2.png") 
ggsave(path, plot = lev_8_6_4_2, width = 10, height = 8, dpi = 300)
