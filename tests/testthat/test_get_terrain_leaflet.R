# R 4.4.0 has bug that cannot display js-based htmlwidgets in viewer

test_that("leaflet from shapefile", {
  xs <- sf::st_read(system.file("extdata", "shiny", "xs.shp",
                        package = "fluvgeodata"), quiet = TRUE)
  dem <- get_dem(xs)
  leaf <- get_terrain_leaflet(xs, dem)
  leaf
  expect_true("leaflet" %in% class(leaf))
})
