library(fluvgeo)
context("xs2pts")

# Retrieve a cross section data structure
xs_sf <- fluvgeo::sin_riffle_floodplain_dims_L3_sf


# Call the xs2pts function for a cross section
xs_pts <- xs2pts(xs_sf)

test_that("check xs_pts", {
  expect_equal(class(xs_pts)[1], "sf")
  expect_equal(sf::st_crs(xs_sf), sf::st_crs(xs_pts))
  expect_equal(length(xs_sf$Seq), length(xs_pts$Seq))
})
