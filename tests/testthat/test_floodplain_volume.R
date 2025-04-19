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
  channel_plot(flowline, rem, 102)
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
  channel_plot(flowline, rem, 118)
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
  channel_watersurface <- trend + 2             # water depth = baseline +2 ft
  floodplain_watersurface <- trend + 18         # water depth = baseline +18 ft
  channel_surface    <- ifel(channel_watersurface > dem,
                             channel_watersurface, dem)
  floodplain_surface <- ifel(floodplain_watersurface > dem,
                             floodplain_watersurface, dem)
  channel_plot(flowline, rem, 118)
  cv  <- floodplain_volume(dem, channel_surface)
  fpv <- floodplain_volume(dem, floodplain_surface)
  dif <- floodplain_volume(channel_surface, floodplain_surface)
  expect_true(cv < fpv)
})
