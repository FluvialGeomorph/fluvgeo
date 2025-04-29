# Define pour point

pour_point <- st_point(c(-89.362239, 43.090266)) %>%
  st_sfc(crs = 4326) %>%
  st_as_sf(crs = 4326) %>%
  mutate(PointID = 1)
