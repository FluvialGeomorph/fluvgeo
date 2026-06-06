test_that("validate_unit_system accepts supported values", {
  expect_identical(validate_unit_system("USCS"), "USCS")
  expect_identical(validate_unit_system("SI"), "SI")
})

test_that("validate_unit_system rejects unsupported values", {
  expect_error(validate_unit_system("Metric"))
  expect_error(validate_unit_system(""))
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
  expect_identical(spec$vertical_reference_label, "NAVD88 ft")
  expect_identical(spec$profile_distance_units, "ft")
  expect_identical(spec$area_units, "ft^2")
  expect_identical(spec$elevation_units, "ft")
  expect_identical(spec$vertical_reference_units, "ft")
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
  expect_identical(spec$vertical_reference_label, "m")
  expect_identical(spec$profile_distance_units, "m")
  expect_identical(spec$area_units, "m^2")
  expect_identical(spec$elevation_units, "m")
  expect_identical(spec$vertical_reference_units, "m")
  expect_true(is.function(spec$profile_distance_to_display))
})

test_that("render_unit_symbol returns expected unit strings", {
  expect_identical(render_unit_symbol("distance", "USCS", "plain"), "ft")
  expect_identical(render_unit_symbol("distance", "SI", "plain"), "m")
  expect_identical(render_unit_symbol("area", "USCS", "plain"), "ft^2")
  expect_identical(render_unit_symbol("area", "SI", "plain"), "m^2")
  expect_identical(render_unit_symbol("elevation", "USCS", "plain"), "ft")
  expect_identical(render_unit_symbol("elevation", "SI", "plain"), "m")
  expect_identical(
    render_unit_symbol("vertical_reference", "USCS", "plain"),
    "ft"
  )
  expect_identical(
    render_unit_symbol("vertical_reference", "SI", "plain"),
    "m"
  )
})

test_that("render_unit_label returns expected labels", {
  expect_identical(
    render_unit_label("distance", "USCS", "plain"),
    "Distance (ft)"
  )
  expect_identical(render_unit_label("distance", "SI", "plain"), "Distance (m)")
  expect_identical(
    render_unit_label("elevation", "USCS", "plain"),
    "Elevation (ft)"
  )
  expect_identical(
    render_unit_label("elevation", "SI", "plain"),
    "Elevation (m)"
  )
  expect_identical(render_unit_label("area", "USCS", "plain"), "Area (ft^2)")
  expect_identical(render_unit_label("area", "SI", "plain"), "Area (m^2)")
  expect_identical(
    render_unit_label("vertical_reference", "USCS", "plain"),
    "Vertical reference (ft)"
  )
  expect_identical(
    render_unit_label("vertical_reference", "SI", "plain"),
    "Vertical reference (m)"
  )
})

test_that("backwards-compatible helpers return expected outputs", {
  expect_identical(format_display_label("distance", "USCS"), "Distance (ft)")
  expect_identical(format_display_label("area", "SI"), "Area (m^2)")
  expect_identical(format_display_units("area", "USCS"), "ft^2")
  expect_identical(format_display_units("elevation", "SI"), "m")
})

test_that("convert_profile_distance converts kilometers for display", {
  expect_equal(convert_profile_distance(1, "USCS"), 3280.84)
  expect_equal(convert_profile_distance(1, "SI"), 1000)
  expect_equal(
    convert_profile_distance(c(0, 1, 2), "USCS"),
    c(0, 3280.84, 6561.68)
  )
})

test_that("convert_distance_value converts kilometers for display", {
  expect_equal(convert_distance_value(1, "USCS"), 3280.84)
  expect_equal(convert_distance_value(1, "SI"), 1000)
  expect_equal(
    convert_distance_value(c(0, 1, 2), "SI"),
    c(0, 1000, 2000)
  )
})

test_that("as_display_units returns units objects with expected units", {
  uscs_distance <- as_display_units(1, "distance", "USCS")
  si_distance <- as_display_units(1, "distance", "SI")
  uscs_area <- as_display_units(1, "area", "USCS")
  si_elevation <- as_display_units(1, "elevation", "SI")

  expect_s3_class(uscs_distance, "units")
  expect_s3_class(si_distance, "units")
  expect_s3_class(uscs_area, "units")
  expect_s3_class(si_elevation, "units")

  expect_identical(units::deparse_unit(uscs_distance), "ft")
  expect_identical(units::deparse_unit(si_distance), "m")
  expect_identical(units::deparse_unit(uscs_area), "ft2")
  expect_identical(units::deparse_unit(si_elevation), "m")

  expect_equal(as.numeric(uscs_distance), 1)
  expect_equal(as.numeric(si_distance), 1)
  expect_equal(as.numeric(uscs_area), 1)
  expect_equal(as.numeric(si_elevation), 1)
})

test_that("display unit helpers reject unknown quantities", {
  expect_error(render_unit_symbol("unknown", "USCS", "plain"))
  expect_error(render_unit_label("unknown", "USCS", "plain"))
  expect_error(format_display_label("unknown", "USCS"))
  expect_error(format_display_units("unknown", "USCS"))
  expect_error(as_display_units(1, "unknown", "USCS"))
})
