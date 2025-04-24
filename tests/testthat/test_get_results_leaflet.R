# R 4.4.0 has bug that cannot display js-based htmlwidgets in viewer

test_that("channel and floodplain polys", {
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  xs_fix <- sf_fix_crs(xs_mapedit)
  xs <- sf::st_transform(xs_fix, crs = 3857) # Web Mercator
  fl_mapedit <- sf::st_read(system.file("extdata", "shiny", "fl_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream"
  dem <- get_dem(xs)
  fl <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(fl, dem, station_distance)
  buffer_distance <- 300
  detrend <- detrend(dem, fl, flowline_points, buffer_distance)
  rem <- detrend$rem
  trend <- detrend$trend
  xs <- cross_section(xs, flowline_points)
  station_distance = 5
  xs_pts <- cross_section_points(xs, dem, rem, station_distance)
  channel_wse <- 103
  channel_poly <- water_surface_poly(rem, channel_wse, fl)
  floodplain_wse <- 112
  floodplain_poly <- water_surface_poly(rem, floodplain_wse, fl)
  leaf <- get_results_leaflet(fl, xs, dem, channel_poly, floodplain_poly)
  leaf
  expect_true("leaflet" %in% class(leaf))
})
