xs_pts_fc  <- file.path(system.file("extdata", "y2016_R1.gdb",
                                    package = "fluvgeo"),
                        "feature_dataset/xs_50_points")
xs_number   <- 8
bf_estimate <- 103.5
regions     <- c("USA", "Eastern United States")
xs_pts_sf   <- fluvgeo::fc2sf(xs_pts_fc)

t1 <- table_xs_dimensions(xs_pts_sf = xs_pts_sf,
                          xs_number = xs_number,
                          bf_estimate = bf_estimate,
                          regions = regions)
#grid.newpage()
#grid::grid.draw(t1)

test_that("check that table_xs_dimensions returns a gtable object", {
  expect_true("gtable" %in% class(t1))
  expect_error(print(t1), NA)
})
