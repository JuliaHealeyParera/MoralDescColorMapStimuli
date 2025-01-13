obj1 <- polys[1, ] 
coords1 <- reprocess_coords(obj1) |>
  mutate(damage = 25)

obj2 <- polys[2, ] 
coords2 <- reprocess_coords(obj2) |>
  mutate(damage = 75, 
         V1 = V1 + .3,
         V2 = V2 + .3)

circle <- base_circle(1.8, 9, .1) 

add_deviation_to_circle <- function(circle, deviation_factor) {
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

ggplot() +
  geom_polygon(circle, mapping = aes(x=x, y=y), fill = "grey90") +
  geom_polygon(data = coords1, aes(x = V1, y = V2, fill = damage)) +
  geom_polygon(data = coords2, aes(x = V1, y = V2, fill = damage)) +
  scale_fill_distiller(direction = -1, palette = "Blues", limits = c(0, 100)) +
  labs(fill = "Damage") +
  theme_void() +
  theme(legend.key.size = unit(.7, 'cm'), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 15))
  
map_col <- map_col + 
  theme_void() +  
  theme(plot.background = element_rect(fill = "white", color = NA),
        theme(legend.key.size = unit(2, 'cm'), 
              legend.text 

ggplot() + 
  geom_polygon(data = coords1, aes(x = V1, y = V2, fill = damage)) +
  geom_polygon(data = coords2, aes(x = V1, y = V2, fill = damage)) +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 100))
