obj1 <- polys[1, ] 
coords1 <- reprocess_coords(obj1) |>
  mutate(damage = 25)

obj2 <- polys[2, ] 
coords2 <- reprocess_coords(obj2) |>
  mutate(damage = 75, 
         V1 = V1 + .3,
         V2 = V2 + .3)

ggplot() + 
  geom_polygon(data = coords1, aes(x = V1, y = V2, fill = damage)) +
  geom_polygon(data = coords2, aes(x = V1, y = V2, fill = damage)) +
  scale_fill_distiller(direction = -1, palette = "Blues", limits = c(0, 100)) +
  theme_void()

ggplot() + 
  geom_polygon(data = coords1, aes(x = V1, y = V2, fill = damage)) +
  geom_polygon(data = coords2, aes(x = V1, y = V2, fill = damage)) +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 100))
