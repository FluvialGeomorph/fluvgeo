test_that("WGS84 input - dem is a terra::SpatRaster", {
  xs_mapedit <- sf::st_read(system.file("extdata", "xs_mapedit.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  xs <- sf::st_transform(xs_mapedit, crs = 3857) # Web Mercator
  dem <- get_dem(xs)
  #terra::plot(dem)
  expect_true("SpatRaster" %in% class(dem))
})

test_that("web mercator input - dem is a terra::SpatRaster", {
  xs <- sf::st_read(system.file("extdata", "xs.shp", 
                       package = "tieredassessment"), quiet = TRUE)
  dem <- get_dem(xs)
  #terra::plot(dem)
  expect_true("SpatRaster" %in% class(dem))
})

test_that("expect error - WGS84 input", {
  xs_mapedit <- sf::st_read(system.file("extdata", "xs_mapedit.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  dem <- get_dem(xs_mapedit)
  #terra::plot(dem)
  expect_error()
})
