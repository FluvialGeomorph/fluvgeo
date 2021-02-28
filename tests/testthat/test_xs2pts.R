library(fluvgeo)
context("xs2pts")

# Retrieve a cross section data structure
xs_sp <- fluvgeo::sin_riffle_floodplain_dims_L3_sp

# Call the xs2pts function for a cross section
xs_pts <- xs2pts(xs_sp)

test_that("check xs_pts", {
  expect_equal(class(xs_pts)[1], "SpatialPointsDataFrame")
  expect_equal(xs_sp@proj4string, xs_pts@proj4string)
  expect_equal(length(xs_sp$Seq), length(xs_pts$Seq))
})
