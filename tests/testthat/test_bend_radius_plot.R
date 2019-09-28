library(fgm)
context("bend_radius_plot")

br_plot <- bend_raduis_plot(fgm::sin_bankline_points_sp,
                            loop = 1,
                            bend = 1,
                            coord_system = "UTM")

test_that("check bend_radius data structure", {
  expect_true("ggplot" %in% class(br_plot))
  expect_error(print(br_plot), NA)
})
