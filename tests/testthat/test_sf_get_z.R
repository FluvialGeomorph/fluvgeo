test_that("test sf_get_z function", {
  fl_pts <- sf::st_read(system.file("extdata", "shiny", "fl_pts.shp",
                                package = "fluvgeodata"), quiet = TRUE)
  points <- fl_pts %>% select(ID)
  dem <- get_dem(points)
  pts_z <- sf_get_z(points, dem)
  expect_true("data.frame" %in% class(pts_z))
  expect_true("x" %in% colnames(pts_z))
  expect_true("y" %in% colnames(pts_z))
  expect_true("z" %in% colnames(pts_z))
})
