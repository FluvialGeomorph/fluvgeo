library(fluvgeo)
context("xs_profile_plot")

# Create cross section profile plot
profile_plot <- xs_profile_plot(reach_xs_dims_sp = fluvgeo::sin_riffle_floodplain_dims_planform_sp,
                                features_sp = fluvgeo::sin_features_sp,
                                label_xs = TRUE)


test_that("Check the plot object", {
  expect_true(ggplot2::is.ggplot(profile_plot))
  expect_equal(profile_plot$labels$title,
               "Sinsinawa")
  expect_error(print(profile_plot), NA)
})
