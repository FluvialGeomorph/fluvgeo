library(fluvgeo)
context("xs_profile_plot")

# Get test data
reach_xs_dims_sp <- fluvgeo::sin_riffle_floodplain_dims_L3_sp
features_sp      <- fluvgeo::sin_features_sp
label_xs = TRUE
xs_label_freq = 2
profile_units = "kilometers"

# Create cross section profile plot
profile_plot <- xs_profile_plot(reach_xs_dims_sp = reach_xs_dims_sp,
                                features_sp = features_sp,
                                label_xs = label_xs,
                                xs_label_freq = xs_label_freq,
                                profile_units = profile_units)

print(profile_plot)

test_that("Check the plot object", {
  expect_true(ggplot2::is.ggplot(profile_plot))
  expect_equal(profile_plot$labels$title,
               "Sinsinawa")
  expect_error(print(profile_plot), NA)
})
