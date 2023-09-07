library(fluvgeo)
context("xs_profile_plot")

# Get test data
reach_xs_dims_sf <- fluvgeo::sin_riffle_floodplain_dims_L3_sf
features_sf      <- fluvgeo::sin_features_sf
label_xs = TRUE
xs_label_freq = 2
profile_units = "kilometers"

# Create cross section profile plot
profile_plot <- xs_profile_plot(reach_xs_dims_sf = reach_xs_dims_sf,
                                features_sf = features_sf,
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

