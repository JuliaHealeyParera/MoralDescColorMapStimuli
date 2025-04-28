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

#Call this function to do all circle generation
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

adj_coord <- function(df, a_x_adj, b_x_adj, c_x_adj, d_x_adj, a_y_adj, b_y_adj, c_y_adj, d_y_adj) {
  df <- df |> 
    mutate(x = case_when(text == "A" ~ x + a_x_adj,
                         text == "B" ~ x + b_x_adj,
                         text == "C" ~ x + c_x_adj,
                         text == "D" ~ x + d_x_adj), 
           y = case_when(text == "A" ~ y + a_y_adj,
                         text == "B" ~ y + b_y_adj,
                         text == "C" ~ y + c_y_adj,
                         text == "D" ~ y + d_y_adj))
  return(df)
}

###MAP CREATION###
create_map = function(starting_angle, idx, input_palette, dir, dmg_lvls, voting_dec) {
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
           damage = dmg_lvls[1],
           yes_no = voting_dec[1])
  coords2 <- obj2 |>
    mutate(x = x + cntr2_x,
           y = y + cntr2_y,
           damage = dmg_lvls[2], 
           yes_no = voting_dec[2])
  coords3 <- obj3 |>
    mutate(x = x + cntr3_x,
           y = y + cntr3_y, 
           damage = dmg_lvls[3], 
           yes_no = voting_dec[3])
  coords4 <- obj4 |>
    mutate(x = x + cntr4_x,
           y = y + cntr4_y, 
           damage = dmg_lvls[4], 
           yes_no = voting_dec[4])
  
  #Create label overlay with polygon center coordinates
  labels <- data.frame(
    text = c("A", "B", "C", "D"),
    x = c(cntr1_x, cntr2_x, cntr3_x, cntr4_x),
    y = c(cntr1_y, cntr2_y, cntr3_y, cntr4_y))
  
  #Circle qualities pre-selected based on visual salience
  circle <- base_circle(1.7, 9, .055) 
  
  #label set for blank version of basemap
  uninhabited <- data.frame(text = c("Uninhabited", "Area"), x = c(0, 0), y = c(0.1, -0.1))
  
  #Label creation
  voting_dec[voting_dec == 1] = "yes"
  voting_dec[voting_dec == 0] = "no"
  yes_no <- data.frame(
    state = c('A', 'B', 'C', 'D'),
    text = voting_dec)
  labels <- labels |>
    left_join(yes_no, by = join_by(text == state)) |>
    mutate(textplot = ifelse(text.y == "yes", paste0(text, '*'), text)) 
  
  #Base map object
  map <- ggplot() +
    geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
    geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "grey70") +
    geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
    geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "grey70") +
    geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
    coord_fixed() 
  
  #Color scheme 
  if (input_palette == "blues") {
    map <- map + 
      scale_fill_distiller(direction = dir, palette = "Blues", limits = c(0, 10))}
  else {
    map <- map + 
      scale_fill_viridis_c(option = "rocket", begin = .2, end = 1, direction = dir, limits = c(0, 10)) 
  }
  
  #Customize according to style
  if (idx == 0) { #Outside provinces, within base polygon
    labels <- adj_coord(df, -.61, 0, .625, 0, 0, -.6, 0, .615) 
      
    map <- map +
      geom_text(data = labels, aes(x = x, y = y, label = textplot), color = "grey10", size = 12) 
  
  } else if (idx == 1) { #Labels below provinces
    labels <- labels |> 
      mutate(y = y - .625) #Could have also done adj_coord
    
    map <- map +
      geom_text(data = labels, aes(x = x, y = y, label = textplot), color = "grey10", size = 12) 
  
  } else if (idx == 2) { #Labels outside provinces and outside base polygon (voting as asterisk)
    labels <- adj_coord(labels, .635, 0, -.635, 0, 0, .635, 0 , -.635)
    
    map <- map +
      geom_segment(aes(x = cntr2_x - .075, xend = cntr2_x + .075, y = cntr2_y - .125, yend = cntr2_y - .13)) + 
      geom_segment(aes(x = cntr4_x - .075, xend = cntr4_x + .075, y = cntr4_y - .125, yend = cntr4_y - .13)) + 
      geom_text(data = labels, aes(x = x, y = y, label = textplot), color = "grey10", size = 12) 
    
  } else if (idx == 3) { #Labels outside provinces and outside base polygon (voting as underline)
    a_x_adj <- 0
    b_x_adj <- .6325
    c_x_adj <- 0
    d_x_adj <- -.632
    
    a_y_adj <- .635
    b_y_adj <- 0
    c_y_adj <- -.635
    d_y_adj <- 0
    
    labels <- adj_coord(labels, 0, b_x_adj, 0, d_x_adj, a_y_adj, 0, c_y_adj, 0)
    
    #Comment out/uncomment line segments according to voting decision
    #Probably some way to automate this, but not worth the hassle
    map <- map +
      #A - geom_segment(aes(x = cntr1_x - .075, xend = cntr1_x + .075, y = cntr1_y + a_y_adj - .125, yend = cntr1_y + a_y_adj - .13)) + 
      geom_segment(aes(x = cntr2_x + b_x_adj - .08, xend = cntr2_x + b_x_adj + .08, y = cntr2_y - .15, yend = cntr2_y - .15)) + 
      #C - geom_segment(aes(x = cntr3_x - .075, xend = cntr3_x + .075, y = cntr3_y + c_y_adj - .125, yend = cntr3_y + c_y_adj - .13)) + 
      geom_segment(aes(x = cntr4_x + d_x_adj - .08, xend = cntr4_x + d_x_adj + .08, y = cntr4_y - .15, yend = cntr4_y - .15)) + 
      geom_text(data = labels, aes(x = x, y = y, label = text), color = "grey10", size = 12) 
  }
  else if (idx == 4) { #White circle labels
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
    
    map <- map +
      geom_polygon(data = label_circle_a, aes(x = x, y = y), fill = "white", color = "white") +
      geom_polygon(data = label_circle_b, aes(x = x, y = y), fill = "white", color = "white") +
      geom_polygon(data = label_circle_c, aes(x = x, y = y), fill = "white", color = "white") +
      geom_polygon(data = label_circle_d, aes(x = x, y = y), fill = "white", color = "white") +
      geom_text(data = labels, aes(x = x, y = y, label = textplot), color = "grey10", size = 12) 
  } else if (idx == 5) { #White circle labels, voting decision dependent
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
    
    #Change label circle outline according to voting decision
    map <- map +
      geom_polygon(data = label_circle_a, aes(x = x, y = y), fill = "white", color = "white") +
      geom_polygon(data = label_circle_b, aes(x = x, y = y), fill = "white", color = "black") +
      geom_polygon(data = label_circle_c, aes(x = x, y = y), fill = "white", color = "white") +
      geom_polygon(data = label_circle_d, aes(x = x, y = y), fill = "white", color = "black") +
      geom_text(data = labels, aes(x = x, y = y, label = textplot), color = "grey10", size = 12) +
      scale_fill_distiller(direction = 1, palette = input_palette, limits = c(0, 10)) +
      coord_fixed() 
  }
  else { #Underline, bold, italic
    #To include symbol, switch label = text in geom_text to label = textplot
    vote_yes <- labels |> 
      filter(text.y == "yes")
    vote_no <- labels |> 
      filter(text.y == "no")
    
    #Comment and uncomment line segements according to voting decision
    map <- map +
      geom_text(data = vote_yes, aes(x = x, y = y, label = text), fontface = "bold.italic", color = "black", size = 12) +
      geom_text(data = vote_no, aes(x = x, y = y, label = text), color = "grey15", size = 12) +
      geom_segment(aes(x = cntr1_x - .075, xend = cntr1_x + .075, y = cntr1_y - .09, yend = cntr1_y - .09), linewidth = 1.2) + 
      geom_segment(aes(x = cntr2_x - .075, xend = cntr2_x + .075, y = cntr2_y - .09, yend = cntr2_y - .09), linewidth = 1.2) + 
      geom_segment(aes(x = cntr3_x - .075, xend = cntr3_x + .075, y = cntr3_y - .09, yend = cntr3_y - .09), linewidth = 1.2) + 
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

#Input vectors 
#always formatted as c(a,b,c,d)
dmg_8642 <- c(8,6,4,2)
vot_8642 <- c(0,1,1,1)
dmg_6211 <- c(6,2,1,1)
vot_6211 <- c(1,1,1,1)
dmg_4321 <- c(4,3,2,1)
vot_4321 <- c(0,1,1,1)

#Different map/label versions
labels_inside_base <- create_map(0, 0, "blues", 1, dmg_8642, vot_8642)
labels_below_prov <- create_map(0, 1, "blues", 1, dmg_8642, vot_8642)
labels_outside_base <- create_map(0, 2, "blues", 1, dmg_8642, vot_8642)
underline <- create_map(0, 3, "blues", 1, dmg_8642, vot_8642)
white_circle_labels <- create_map(0, 4, "blues", 1, dmg_8642, vot_8642)
white_circle_outline <- create_map(0,5, "blues", 1, dmg_8642, vot_8642)


###FINAL MAPS### -- creation and download
#8642s
rocket_unint_8642 <- create_map(0,6, "rocket", 1, dmg_8642, vot_8642) 
rocket_int_8642 <- create_map(0,6, "rocket", -1, dmg_8642, vot_8642) 
blues_unint_8642 <- create_map(0,6, "blues", -1, dmg_8642, vot_8642) 
blues_int_8642 <- create_map(0,6, "blues", 1, dmg_8642, vot_8642) 

path_ir_8642 <- here("map_plots", "final", "intuitive_rocket_8642.png") 
path_ur_8642 <- here("map_plots", "final", "unintuitive_rocket_8642_v2.png")
path_ib_8642 <- here("map_plots", "final", "intuitive_blues_8642.png") 
path_ub_8642 <- here("map_plots", "final", "unintuitive_blues_8642.png") 

ggsave(path_ir_8642, plot = rocket_int_8642, width = 10, height = 8, dpi = 300)
ggsave(path_ur_8642, plot = rocket_unint_8642, width = 10, height = 8, dpi = 300)
ggsave(path_ib_8642, plot = blues_int_8642, width = 10, height = 8, dpi = 300)
ggsave(path_ub_8642, plot = blues_unint_8642, width = 10, height = 8, dpi = 300)

#6211s
rocket_unint_6211 <- create_map(0,6, "rocket", 1, dmg_6211, vot_6211) 
rocket_int_6211 <- create_map(0,6, "rocket", -1, dmg_6211, vot_6211) 
blues_unint_6211 <- create_map(0,6, "blues", -1, dmg_6211, vot_6211) 
blues_int_6211 <- create_map(0,6, "blues", 1, dmg_6211, vot_6211) 

path_ir_6211 <- here("map_plots", "final", "intuitive_rocket_6211.png") 
path_ur_6211 <- here("map_plots", "final", "unintuitive_rocket_6211.png")
path_ib_6211 <- here("map_plots", "final", "intuitive_blues_6211.png") 
path_ub_6211 <- here("map_plots", "final", "unintuitive_blues_6211.png") 

ggsave(path_ir_6211, plot = rocket_int_6211, width = 10, height = 8, dpi = 300)
ggsave(path_ur_6211, plot = rocket_unint_6211, width = 10, height = 8, dpi = 300)
ggsave(path_ib_6211, plot = blues_int_6211, width = 10, height = 8, dpi = 300)
ggsave(path_ub_6211, plot = blues_unint_6211, width = 10, height = 8, dpi = 300)

#4321s
rocket_unint_4321 <- create_map(0,6, "rocket", 1, dmg_4321, vot_4321) 
rocket_int_4321 <- create_map(0,6, "rocket", -1, dmg_4321, vot_4321) 
blues_unint_4321 <- create_map(0,6, "blues", -1, dmg_4321, vot_4321) 
blues_int_4321 <- create_map(0,6, "blues", 1, dmg_4321, vot_4321) 

path_ir_4321 <- here("map_plots", "final", "intuitive_rocket_4321.png") 
path_ur_4321 <- here("map_plots", "final", "unintuitive_rocket_4321.png")
path_ib_4321 <- here("map_plots", "final", "intuitive_blues_4321.png") 
path_ub_4321 <- here("map_plots", "final", "unintuitive_blues_4321.png") 

ggsave(path_ir_4321, plot = rocket_int_4321, width = 10, height = 8, dpi = 300)
ggsave(path_ur_4321, plot = rocket_unint_4321, width = 10, height = 8, dpi = 300)
ggsave(path_ib_4321, plot = blues_int_4321, width = 10, height = 8, dpi = 300)
ggsave(path_ub_4321, plot = blues_unint_4321, width = 10, height = 8, dpi = 300)

