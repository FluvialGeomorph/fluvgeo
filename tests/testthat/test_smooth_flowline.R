test_that("Flowline smoothing uses the bounded legacy default", {
  raw <- sf::st_sf(route_id = "P00001", length_m = sqrt(2) * 4,
    geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 1, 2, 0, 3, 1, 4, 0),
      ncol = 2, byrow = TRUE)), crs = 26915))

  smoothed <- smooth_flowline(raw)

  expect_s3_class(smoothed, "sf")
  expect_identical(smoothed$smoothing_method, "Gaussian kernel regression")
  expect_equal(smoothed$smoothing_bandwidth, 2)
  expect_true(smoothed$smoothing_valid)
  expect_equal(smoothed$raw_length_m, raw$length_m)
  expect_lt(smoothed$length_m, smoothed$raw_length_m)
  expect_lte(smoothed$maximum_displacement, 2)
  expect_true(sf::st_is_simple(smoothed))
  raw_xy <- sf::st_coordinates(raw)[, c("X", "Y"), drop = FALSE]
  smooth_xy <- sf::st_coordinates(smoothed)[, c("X", "Y"), drop = FALSE]
  expect_equal(smooth_xy[c(1, nrow(smooth_xy)), , drop = FALSE],
    raw_xy[c(1, nrow(raw_xy)), , drop = FALSE])
  expect_false(isTRUE(all.equal(sf::st_geometry(smoothed), sf::st_geometry(raw))))
})

test_that("Flowline smoothing validates input and displacement", {
  raw <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(
    matrix(c(0, 0, 1, 1, 2, 0, 3, 1, 4, 0), ncol = 2, byrow = TRUE)),
    crs = 26915))

  expect_error(smooth_flowline(sf::st_transform(raw, 4326)), "projected")
  expect_error(smooth_flowline(raw, bandwidth = 0), "positive finite")
  expect_error(smooth_flowline(raw, max_displacement = 0.01), "exceeds")
})
