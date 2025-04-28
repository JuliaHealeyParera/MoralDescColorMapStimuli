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

idx <- sample(c(1,2,3,4), size = 1)
radius <- c(1.9, 1.8, 1.6, 1.9)
perimeter <- c(8, 9, 9, 8)
deviation <- c(.08, .1, .1, .06)
circle <- base_circle(1.6, 9, .1) 
ggplot() +
  geom_polygon(circle, mapping = aes(x=x, y=y), fill = "gray") +
  coord_fixed()

a_idx=1
starting_angle = 30
col = "blues"
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

#Four possibilities for base region size and deviation amount
#Pre-selected based on visual salience
circle <- base_circle(1.9, 8, .08) 

#Plot, store to object
ggplot() +
  geom_polygon(circle, mapping = aes(x=x, y=y), fill = "gray") +
  geom_polygon(data = coords1, aes(x = V1, y = V2, fill = damage)) +
  geom_polygon(data = coords2, aes(x = V1, y = V2, fill = damage)) +
  geom_polygon(data = coords3, aes(x = V1, y = V2, fill = damage)) +
  geom_polygon(data = coords4, aes(x = V1, y = V2, fill = damage)) +
  geom_text(data = labels, aes(x = x, y = y, label = text), color = "white", size = 12) +
  labs(fill = "Damage") +
  coord_fixed() 

if (col == "blues") { #Blue color map
  map_col <- map +
    scale_fill_continuous(limits = c(0,100))} 
else { #Rocket color map
  map_col <- map +
    scale_fill_viridis(option = "rocket", limits = c(0,100))}

#Add void theme to eliminate grid lines, axes, etc. 
#Q - add white background? (Currently transparent background)
map_col <- map_col + theme_void()
return(map_col)





