# R 4.4.0 has bug that cannot display js-based htmlwidgets in viewer

test_that("leaflet from geojson", {
  fl_shp <- sf::st_read(system.file("extdata", "fl.shp", 
                                    package = "tieredassessment"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_shp)
  fl <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  xs_shp <- sf::st_read(system.file("extdata", "xs.shp", 
                                    package = "tieredassessment"), quiet = TRUE)
  xs_fix <- sf_fix_crs(xs_shp)
  xs <- sf::st_transform(xs_fix, crs = 3857) # Web Mercator
  xs$Seq <- row.names(xs)
  dem <- get_dem(xs)
  leaf <- get_results_leaflet(fl, xs, dem)
  leaf
  expect_true("leaflet" %in% class(leaf))
})
