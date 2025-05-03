watershed_plot <- function(watershed_pt) {
  plot(watershed_pt$drainage_basin$geometry, col = "blue")
  plot(watershed_pt$pt$geometry, add = TRUE, col = "red")
}

test_that("get watershed area, crs:3857", {
  point_sfc <- sf::st_sfc(sf::st_point(x = c(-10449821, 4503124)), crs = 3857)
  point_sf <- sf::st_as_sf(point_sfc)
  watershed_pt <- pt_watershed_area(point_sf)
  watershed_plot(watershed_pt)
  expect_true("sf" %in% class(watershed_pt$pt))
  expect_true("sf" %in% class(watershed_pt$drainage_basin))
  expect_equal(nrow(point_sf),
               nrow(watershed_pt$pt),
               nrow(watershed_pt$drainage_basin))
})
test_that("get watershed area, crs:4326", {
  point_sfc <- sf::st_sfc(sf::st_point(x = c(-89.2158, 42.9561)), crs = 4326)
  point_sf <- sf::st_as_sf(point_sfc)
  watershed_pt <- pt_watershed_area(point_sf)
  watershed_plot(watershed_pt)
  expect_true("sf" %in% class(watershed_pt$pt))
  expect_true("sf" %in% class(watershed_pt$drainage_basin))
  expect_equal(nrow(point_sf),
               nrow(watershed_pt$pt),
               nrow(watershed_pt$drainage_basin))
})
