library(sp)
library(tidyverse)
library(ggplot2)
library(ggforce)
library(sf)
library(tidycensus)
library(units)
library(pliman)
library(RColorBrewer)
library(viridis)
library(here)

### IDENTIFYING REGIONS
#Find similar area distributions using US Counties
us_county_sf = get_acs(geography = "county", variables = c("totalpop" = "B01001A_001"),
                       geometry= T) # from census, use load_variables() to find names
us_county_sf = us_county_sf |> select(-variable, -moe) |> rename(total_pop = estimate, place_name = NAME)
us_county_sf = us_county_sf |> mutate(state_name = place_name |> str_extract("[A-Za-z ]+$") |> str_trim())
us_county_sf |> st_set_geometry(NULL) |> count(state_name)

#Example US
us_cont_county_sf = us_county_sf |> filter(!(state_name %in% c("Puerto Rico", "Hawaii", "Alaska"))) 
us_cont_county_sf|> st_geometry() |> plot()


#Selecting most circular states via maximum radius calculation
max_dist = function(geo, cntr){
  points_list <- NULL
  points_list <- data.frame(st_cast(geo, "POINT"))
  points_list <- points_list |>
    mutate(dist = st_distance(geometry, cntr))
  maximum = max(points_list$dist)
  return(maximum)
}

#Calculate: 
# 1) Actual polygon area
# 2) Area of smallest possible circle that can cover polygon
# 3) Ratio of actual/ideal area to identify most circular regions
#Generating this for entire US takes too long--select subset of states
#Kentucky
us_cont_county_sf_ky <- us_cont_county_sf |>
  filter(str_detect(state_name, "Kentucky")) |>
  rowwise() |>
  mutate(center = st_centroid(geometry),
         max_dist = max_dist(geometry, center)) |>
  ungroup()

us_cont_county_sf_ky <- us_cont_county_sf_ky |>
  rowwise() |>
  mutate(area = st_area(geometry),
         possible_area = pi*max_dist*max_dist,
         area_ratio = area / possible_area)

#West Virginia
us_cont_county_sf_wv <- us_cont_county_sf |>
  filter(str_detect(state_name, "West Virginia")) |>
  rowwise() |>
  mutate(center = st_centroid(geometry),
         max_dist = max_dist(geometry, center)) |>
  ungroup()

us_cont_county_sf_wv <- us_cont_county_sf_wv |>
  rowwise() |>
  mutate(area = st_area(geometry),
         possible_area = pi*max_dist*max_dist,
         area_ratio = area / possible_area)

#Virginia
us_cont_county_sf_va <- us_cont_county_sf |>
  filter(str_detect(state_name, "Virginia")) |>
  rowwise() |>
  mutate(center = st_centroid(geometry),
         max_dist = max_dist(geometry, center)) |>
  ungroup()

us_cont_county_sf_va <- us_cont_county_sf_va |>
  rowwise() |>
  mutate(area = st_area(geometry),
         possible_area = pi*max_dist*max_dist,
         area_ratio = area / possible_area)

#Georgia
us_cont_county_sf_ga <- us_cont_county_sf |>
  filter(str_detect(state_name, "Georgia")) |>
  rowwise() |>
  mutate(center = st_centroid(geometry),
         max_dist = max_dist(geometry, center)) |>
  ungroup()

us_cont_county_sf_ga <- us_cont_county_sf_ga |>
  rowwise() |>
  mutate(area = st_area(geometry),
         possible_area = pi*max_dist*max_dist,
         area_ratio = area / possible_area)

#Tennessee
us_cont_county_sf_tn <- us_cont_county_sf |>
  filter(str_detect(state_name, "Tennessee")) |>
  rowwise() |>
  mutate(center = st_centroid(geometry),
         max_dist = max_dist(geometry, center)) |>
  ungroup()

us_cont_county_sf_tn <- us_cont_county_sf_tn |>
  rowwise() |>
  mutate(area = st_area(geometry),
         possible_area = pi*max_dist*max_dist,
         area_ratio = area / possible_area)

#North Carolina
us_cont_county_sf_nc <- us_cont_county_sf |>
  filter(str_detect(state_name, "North Carolina")) |>
  rowwise() |>
  mutate(center = st_centroid(geometry),
         max_dist = max_dist(geometry, center)) |>
  ungroup()

us_cont_county_sf_nc <- us_cont_county_sf_nc |>
  rowwise() |>
  mutate(area = st_area(geometry),
         possible_area = pi*max_dist*max_dist,
         area_ratio = area / possible_area)

#Combined into one df
all <- bind_rows(us_cont_county_sf_ky, 
                 us_cont_county_sf_wv, 
                 us_cont_county_sf_va, 
                 us_cont_county_sf_ga, 
                 us_cont_county_sf_tn, 
                 us_cont_county_sf_nc)

#Best four states
all_slice_12 <- all |>
  arrange(desc(area_ratio)) |>
  slice_head(n=25) |>
  filter((GEOID %in% 
             c(47087, 
               37145,
               47051,
               37033)))

#Area scaling
all_slice_12 <- all_slice_12 |>
  rowwise() |>
  mutate(
    geometry = st_transform(geometry, 32618),
    area = st_area(geometry),
    area_coef = 4500 / area,
    area_coef = drop_units(area_coef),
    center = st_centroid(geometry),
    geometry = geometry - center,
    center = st_centroid(geometry),
    geometry = geometry * sqrt(area_coef) / 100)

###BASE POLYGON CODE - separated for readability in basepolygon_separate.R
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

base_circle <- function(radius, target_perimeter, deviation_factor) {
  circle <- generate_circle(radius)
  circle <- add_deviation_to_circle(circle, deviation_factor)
  circle <- adjust_perimeter(circle, target_perimeter)
  return(circle)
}

###MAP CREATION LOOP

#Reprocess coordinates for plotting
reprocess_coords = function(country) {
  coords_poly1 <- as.data.frame(st_coordinates(country$geometry)[, 1:2])
  coords_cntr <- poly_center(coords_poly1, plot = FALSE)
  coords_cntr <- as.data.frame(poly_smooth(coords_cntr, plot = FALSE))
  return(coords_cntr)
}

#Rotation function
rot <- function(df, angle_deg) {
  angle_rad <- angle_deg * (pi / 180)
  
  df_rotated <- df |>
    mutate(
      x_rot = V1 * cos(angle_rad) - V2 * sin(angle_rad),
      y_rot = V1 * sin(angle_rad) + V2 * cos(angle_rad),
      V1 = x_rot, 
      V2 = y_rot
    )
  
  return(df_rotated)
}


#Map creation function
create_map = function(a_idx, col, starting_angle, intuition) {
  #Index for D country is 3 after index for A country -- select damage levels
  d_idx <- a_idx + 3
  props <- scenarios[a_idx:d_idx]
  
  #Select four random polygons
  polys <- all_slice_12 |> 
    ungroup() |>
    slice_sample(n = 4, replace = FALSE) 

  #Declare polygon objects
  obj1 <- polys[1, ] 
  obj2 <- polys[2, ]
  obj3 <- polys[3, ]
  obj4 <- polys[4, ]
  
  #Establish map-wise rotation
  first_obj_rad = starting_angle * pi/180
  second_obj_rad = first_obj_rad + pi/2 
  third_obj_rad = first_obj_rad + pi
  fourth_obj_rad = first_obj_rad + 3*pi/2
  
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
  coords1 <- reprocess_coords(obj1) |>
    mutate(V1 = V1 + cntr1_x,
           V2 = V2 + cntr1_y,
           damage = props[1])
  coords2 <- reprocess_coords(obj2) |>
    mutate(V1 = V1 + cntr2_x,
           V2 = V2 + cntr2_y,
           damage = props[2])
  coords3 <- reprocess_coords(obj3) |>
    mutate(V1 = V1 + cntr3_x,
           V2 = V2 + cntr3_y,
           damage = props[3])
  coords4 <- reprocess_coords(obj4) |>
    mutate(V1 =V1 + cntr4_x,
           V2 = V2 + cntr4_y,
           damage = props[4])
  
  #Rotate polygons by random amount
  rot_angles <- sample(0:360, size=4)
  coords1 <- rot(coords1, rot_angles[1])
  coords2 <- rot(coords2, rot_angles[2])
  coords3 <- rot(coords3, rot_angles[3])
  coords4 <- rot(coords4, rot_angles[4])
  
  #Create label overlay with polygon center coordinates
  labels <- data.frame(
    text = c("A", "B", "C", "D"),
    x = c(cntr1_x, cntr2_x, cntr3_x, cntr4_x),
    y = c(cntr1_y, cntr2_y, cntr3_y, cntr4_y))
  
  #Circle qualities pre-selected based on visual salience
  circle <- base_circle(1.8, 9, .1) 
  
  #Plot, store to object
  map <- ggplot() +
    geom_polygon(circle, mapping = aes(x=x, y=y), fill = "grey90") +
    geom_polygon(data = coords1, aes(x = V1, y = V2, fill = damage)) +
    geom_polygon(data = coords2, aes(x = V1, y = V2, fill = damage)) +
    geom_polygon(data = coords3, aes(x = V1, y = V2, fill = damage)) +
    geom_polygon(data = coords4, aes(x = V1, y = V2, fill = damage)) +
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
      scale_fill_distiller(direction = rev, palette = "Blues", limits = c(0, 100))}
  else { #Rocket color map
    map_col <- map +
      scale_fill_viridis_c(option = "rocket", direction = 1, limits = c(0, 100))
  }

  #Add void theme to eliminate grid lines, axes, etc. 
  #Q - add white background? (Currently transparent background)
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
scenarios <- c(c(25,25,25,25),c(40,40,10,10),c(10,10,40,40),c(40,25,25,10),c(10,25,25,40),c(25,25,10,40))
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
  mutate(scenario_props = case_when(X.1. == 1 ~ '25_25_25_25',
                                    X.1. == 5 ~ '40_40_10_10',
                                    X.1. == 9 ~ '10_10_40_40',
                                    X.1. == 13 ~ '40_25_25_10',
                                    X.1. == 17 ~ '10_25_25_40',
                                    X.1. == 21 ~ '25_25_10_40')) |>
  select(-X.1.)

colnames(map_attributes_edited) <- c('intuition', 'colormap', 'version', 'rotation', 'file_name', 'scenario_props')

write.csv(map_attributes_edited, 'map_attributes.csv')


