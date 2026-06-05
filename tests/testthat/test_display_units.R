test_that("validate_unit_system accepts supported values", {
  expect_identical(validate_unit_system("USCS"), "USCS")
  expect_identical(validate_unit_system("SI"), "SI")
})

test_that("validate_unit_system rejects unsupported values", {
  expect_error(validate_unit_system("Metric"))
  expect_error(validate_unit_system("")
  )
  expect_error(validate_unit_system(NA_character_))
})

test_that("unit_system_spec returns expected fields for USCS", {
  spec <- unit_system_spec("USCS")

  expect_type(spec, "list")
  expect_identical(spec$unit_system, "USCS")
  expect_identical(spec$length_unit, "ft")
  expect_identical(spec$area_unit, "sq ft")
  expect_identical(spec$elevation_unit, "ft")
  expect_identical(spec$distance_axis_label, "Distance (ft)")
  expect_identical(spec$elevation_axis_label, "Elevation (ft)")
  expect_identical(spec$area_label, "Area (sq ft)")
  expect_true(is.function(spec$profile_distance_to_display))
})

test_that("unit_system_spec returns expected fields for SI", {
  spec <- unit_system_spec("SI")

  expect_type(spec, "list")
  expect_identical(spec$unit_system, "SI")
  expect_identical(spec$length_unit, "m")
  expect_identical(spec$area_unit, "sq m")
  expect_identical(spec$elevation_unit, "m")
  expect_identical(spec$distance_axis_label, "Distance (m)")
  expect_identical(spec$elevation_axis_label, "Elevation (m)")
  expect_identical(spec$area_label, "Area (sq m)")
  expect_true(is.function(spec$profile_distance_to_display))
})

test_that("convert_profile_distance converts kilometers for display", {
  expect_equal(convert_profile_distance(1, "USCS"), 3280.84)
  expect_equal(convert_profile_distance(1, "SI"), 1000)
  expect_equal(convert_profile_distance(c(0, 1, 2), "USCS"), c(0, 3280.84, 6561.68))
})
