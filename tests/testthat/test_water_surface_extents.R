test_that("paired inundation keeps nested outer extents and original holes", {
  r <- terra::rast(nrows = 7, ncols = 7, xmin = 0, xmax = 7,
                   ymin = 0, ymax = 7, crs = "EPSG:3857")
  grid <- matrix(9, 7, 7)
  grid[2:6, 2:6] <- 2
  grid[3:5, 3:5] <- 1
  grid[4, 4] <- 9
  terra::values(r) <- as.vector(t(grid))
  flow <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(2, 4.5), c(5, 4.5))), crs = 3857))
  result <- water_surface_extents(r, flow, 1.5, 3)
  expect_equal(names(result), c("channel", "floodplain", "channel_outer", "floodplain_outer"))
  expect_equal(as.numeric(sum(sf::st_area(result$channel_outer))),
               as.numeric(sum(sf::st_area(result$channel))) + 1)
  outside <- sf::st_difference(sf::st_union(result$channel_outer),
                               sf::st_union(result$floodplain_outer))
  expect_equal(sum(as.numeric(sf::st_area(outside))), 0)
  expect_error(water_surface_extents(r, flow, 3, 3), "strictly less")
  expect_error(water_surface_extents(r, flow, 4, 3), "strictly less")
  expect_error(water_surface_extents(r, flow, NA_real_, 3), "strictly less")
})

test_that("paired inundation rejects missing or mismatched spatial context", {
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2,
                   ymin = 0, ymax = 2, crs = "EPSG:3857")
  terra::values(r) <- 1
  flow <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(0, 1), c(2, 1))), crs = 4326))
  expect_error(water_surface_extents(r, flow, 1, 2), "same known CRS")
})

test_that("disconnected inundation is excluded and absent inundation is reported", {
  r <- terra::rast(nrows = 5, ncols = 7, xmin = 0, xmax = 7,
                   ymin = 0, ymax = 5, crs = "EPSG:3857")
  grid <- matrix(9, 5, 7)
  grid[2:4, 2:3] <- 1
  grid[2:4, 6] <- 1
  terra::values(r) <- as.vector(t(grid))
  flow <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(2.5, 1.5), c(2.5, 3.5))), crs = 3857))
  result <- water_surface_extents(r, flow, 1.5, 2)
  expect_equal(as.numeric(sum(sf::st_area(result$channel_outer))), 6)
  expect_error(water_surface_extents(r, flow, 0, 2), "No inundation")
})
