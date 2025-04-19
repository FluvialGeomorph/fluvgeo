channel_plot <- function(flowline, rem, water_surface_elevation) {
  terra::plot(rem < water_surface_elevation)
  terra::lines(flowline, col = "blue")
}

test_that("check channel volume", {
  fl_mapedit <- sf::st_read(system.file("extdata", "shiny", "fl_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream"
  dem <- get_dem(fl_3857)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(flowline, dem, station_distance)
  buffer_distance <- 300
  detrend <- detrend(dem, flowline, flowline_points, buffer_distance)
  rem <- detrend$rem
  trend <- detrend$trend
  watersurface <- trend + 2                     # water depth = baseline + 2 ft
  #channel_plot(flowline, rem, 102)
  fpv <- floodplain_volume(dem, watersurface)
  expect_true(is.numeric(fpv))
})

test_that("check floodplain volume", {
  fl_mapedit <- sf::st_read(system.file("extdata", "shiny", "fl_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream"
  dem <- get_dem(fl_3857)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(flowline, dem, station_distance)
  buffer_distance <- 300
  detrend <- detrend(dem, flowline, flowline_points, buffer_distance)
  rem <- detrend$rem
  trend <- detrend$trend
  watersurface <- trend + 18                   # water depth = baseline + 18 ft
  #channel_plot(flowline, rem, 118)
  fpv <- floodplain_volume(dem, watersurface)
  expect_true(is.numeric(fpv))
})

test_that("check volume difference between channel and floodplain surfaces", {
  fl_mapedit <- sf::st_read(system.file("extdata", "shiny", "fl_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream"
  dem <- get_dem(fl_3857)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(flowline, dem, station_distance)
  buffer_distance <- 300
  detrend <- detrend(dem, flowline, flowline_points, buffer_distance)
  rem <- detrend$rem
  trend <- detrend$trend
  channel_dem <- hydroflatten_dem(dem, trend, 2)
  floodplain_dem <- hydroflatten_dem(dem, trend, 18)
  #channel_plot(flowline, rem, 118)
  cv  <- floodplain_volume(dem, channel_dem)
  fpv <- floodplain_volume(dem, floodplain_dem)
  dif <- floodplain_volume(channel_dem, floodplain_dem)
  expect_true(cv < fpv)
})
