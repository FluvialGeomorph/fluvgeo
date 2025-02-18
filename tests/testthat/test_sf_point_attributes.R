test_that("", {
  flowline_points <- file.path(system.file("extdata", "y2016_R1.gdb",
                                             package = "fluvgeo"),
                                 "feature_dataset/flowline_points")
  points <- fc2sf(flowline_points)
  field_names = c("x_coord","y_coord", "m")
  pts <- sf_point_attributes(points, field_names)
  expect_true("sf" %in% class(pts))
  expect_true("x_coord" %in% colnames(pts))
  expect_true("y_coord" %in% colnames(pts))
  expect_true("m" %in% colnames(pts))
})

test_that("", {
  flowline_points <- file.path(system.file("extdata", "y2016_R1.gdb",
                                           package = "fluvgeo"),
                               "feature_dataset/flowline_points")
  points <- fc2sf(flowline_points)
  field_names = c("x_coord","y_coord")
  pts <- sf_point_attributes(points, field_names)
  expect_true("sf" %in% class(pts))
  expect_true("x_coord" %in% colnames(pts))
  expect_true("y_coord" %in% colnames(pts))
})
