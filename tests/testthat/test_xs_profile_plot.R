library(fgm)
context("xs_profile_plot")

# Extract data from spatial features
reach_xs_dims_df <- fgm::sin_riffle_floodplain_dims_planform_sp@data
sin_features_df <- fgm::sin_features_sp@data

# Create cross section profile plot
sin_xs_profile_plot <- xs_profile_plot(reach_xs_dims = reach_xs_dims_df,
                                       features = sin_features_df,
                                       label_xs = TRUE)

# Print the plot
print(sin_xs_profile_plot)

test_that("Check the plot object", {
  expect_true(ggplot2::is.ggplot(sin_xs_profile_plot))
  expect_equal(sin_xs_profile_plot$labels$title,
               "Sinsinawa")
  expect_error(print(sin_xs_profile_plot), NA)
})
