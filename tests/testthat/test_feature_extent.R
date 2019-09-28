library(fgm)
context("feature_extent")

# Calculate extents
fp_extent <- feature_extent(fgm::sin_flowline_points_sp)
rc_extent <- feature_extent(fgm::sin_riffle_channel_sp)

test_that("Check feature extents", {
  expect_true(class(fp_extent)[1] == "Extent")
  expect_true(class(rc_extent)[1] == "Extent")
})
