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
create_map = function(a_idx, col, starting_angle, intuition) {
  #Index for D country is 3 after index for A country -- select damage levels
  d_idx <- a_idx + 3
  props <- scenarios[a_idx:d_idx]
  
  #Declare polygon objects
  obj1 <- base_circle(.85, 3.8, .045)
  obj2 <- base_circle(.85, 3.8, .045)
  obj3 <- base_circle(.85, 3.8, .045)
  obj4 <- base_circle(.85, 3.8, .045)
  
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
           y = y + cntr1_y,
           damage = props[1])
  coords2 <- obj2 |>
    mutate(x = x + cntr2_x,
           y = y + cntr2_y,
           damage = props[2])
  coords3 <- obj3 |>
    mutate(x = x + cntr3_x,
           y = y + cntr3_y,
           damage = props[3])
  coords4 <- obj4 |>
    mutate(x = x + cntr4_x,
           y = y + cntr4_y,
           damage = props[4])
  
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
  circle <- base_circle(1.7, 9, .055) 
  
  #Plot, store to object
  map <- ggplot() +
    geom_polygon(circle, mapping = aes(x = x, y = y), fill = "grey90", color = "grey80") +
    geom_polygon(data = coords1, aes(x = x, y = y, fill = damage), color  = "grey70") +
    geom_polygon(data = coords2, aes(x = x, y = y, fill = damage), color = "grey70") +
    geom_polygon(data = coords3, aes(x = x, y = y, fill = damage), color = "grey70") +
    geom_polygon(data = coords4, aes(x = x, y = y, fill = damage), color = "grey70") +
    geom_text(data = labels, aes(x = x, y = y, label = text), color = "white", size = 12) +
    labs(fill = "Damage") +
    coord_fixed() 
  
  if (intuition == "int") {
    rev = 1
  } else {
    rev = -1
  }
  
  if (col == "blues") { #Blue color map
    map_col <- map +
      scale_fill_distiller(direction = rev, palette = "Blues", limits = c(0, 10))}
  else { #Rocket color map
    map_col <- map +
      scale_fill_viridis_c(option = "rocket", begin = .2, end = 1, direction = -rev, limits = c(0, 10))
  }
  
  #Add void theme to eliminate grid lines, axes, etc. 
  map_col <- map_col + 
    theme_void() +  
    theme(plot.background = element_rect(fill = "white", color = NA),
          legend.key.size = unit(.9, 'cm'), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 20))
  return(map_col)
}


#Save map to repository
download_map = function(map, a_idx, intuition, col, i) {
  #Create scenario prefix, rename intuition level, add version number
  scenario = case_when(a_idx == 1 ~ "scen1",
                       a_idx == 5 ~ "scen2",
                       a_idx == 9 ~ "scen3",
                       a_idx == 13 ~ "scen4",
                       a_idx == 17 ~ "scen5",
                       a_idx == 21 ~ "scen6")
  intuition_level = if_else(intuition == "int", "intuitive", "nonintuitive")
  i = paste("ver", i)
  
  #Create name, folder name, and set path
  download_name <- paste(scenario, col, intuition_level, i, ".png", sep = "_")
  folder_name <- paste(scenario, col, intuition_level, "maps", sep = "_")
  path <- here("map_plots", folder_name, download_name) 
  
  #Save and return name for storage in attribute table
  ggsave(path, plot = map, width = 10, height = 8, dpi = 300)
  return(download_name)
}

#Scenario damage levels
scenarios <- c(c(2,2,2,2),c(8,8,4,4),c(4,4,8,8),c(8,5,5,2),c(2,5,5,8),c(5,5,2,8))
#Possible indices of country A for each scenario 
scenario_a_indx <- c(1,5,9,13,17,21)
#Differentiating between intuititon levels and color schemes 
int_nonint <- c("int", "nonint")
col_scheme <- c("rocket", "blues")
#Version number
iter <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
#Possible map rotations
#Any given two maps within an attribute combination will be at least 15 degrees apart
map_rot <- c(0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,240,255,270,285,300,315,330,345)

#Initialize attributes dataframe
map_attributes <- data.frame(
  a_idx = c(),
  intuition = c(),
  color = c(),
  version = c(),
  starting_angle = c(),
  filename = c()
)

for (a_idx in scenario_a_indx) {
  for (col in col_scheme){
    for (intuition in int_nonint) {
      #Randomly choose 8 starting angles for 8 versions of a given combination
      starting_angle_arr <- sample(map_rot, size = 16, replace = FALSE)
      for (i in iter) {
        starting_angle = starting_angle_arr[i]
        
        map <- create_map(a_idx, col, starting_angle, intuition)
        
        filename <- download_map(map, a_idx, intuition, col, i)
        
        attr <- c(a_idx, intuition, col, i, starting_angle, filename)
        map_attributes <- map_attributes |> rbind(attr)
      }
    }
  }
}


map_attributes_edited <- map_attributes |>
  mutate(scenario_props = case_when(X.1. == 1 ~ '2_2_2_2',
                                    X.1. == 5 ~ '8_8_4_4',
                                    X.1. == 9 ~ '4_4_8_8',
                                    X.1. == 13 ~ '8_5_5_2',
                                    X.1. == 17 ~ '2_5_5_8',
                                    X.1. == 21 ~ '5_5_2_8')) |>
  select(-X.1.)

colnames(map_attributes_edited) <- c('intuition', 'colormap', 'version', 'rotation', 'file_name', 'scenario_props')

write.csv(map_attributes_edited, 'map_attributes.csv')
