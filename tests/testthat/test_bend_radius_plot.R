library(fluvgeo)
context("bend_radius_plot")

br_plot_1 <- bend_raduis_plot(fluvgeo::sin_bankline_points_sp, 1, 1, "UTM 16N")
br_plot_2 <- bend_raduis_plot(fluvgeo::sin_bankline_points_sp, 2, 1, "UTM 16N")

test_that("check bend_radius_plot 1", {
  expect_true("ggplot" %in% class(br_plot_1))
  expect_error(print(br_plot_1), NA)
})

test_that("check bend_radius_plot 2", {
  expect_true("ggplot" %in% class(br_plot_2))
  expect_error(print(br_plot_2), NA)
})
