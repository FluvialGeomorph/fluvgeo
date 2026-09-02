test_that("cross section watershed lookup can be skipped", {
  called <- FALSE
  lookup <- function(point) {
    called <<- TRUE
    stop("lookup should not run")
  }

  area <- cross_section_watershed_area(
    point = structure(list(), class = "sf"),
    watershed = "skip",
    watershed_area_fn = lookup
  )

  expect_false(called)
  expect_true(is.na(area))
})

test_that("optional cross section watershed lookup warns and returns missing", {
  lookup <- function(point) {
    stop("service unavailable")
  }

  expect_warning(
    area <- cross_section_watershed_area(
      point = structure(list(), class = "sf"),
      watershed = "optional",
      watershed_area_fn = lookup
    ),
    "Continuing with missing watershed area"
  )
  expect_true(is.na(area))
})

test_that("required cross section watershed lookup retains strict behavior", {
  lookup <- function(point) {
    stop("service unavailable")
  }

  expect_error(
    cross_section_watershed_area(
      point = structure(list(), class = "sf"),
      watershed = "required",
      watershed_area_fn = lookup
    ),
    "Unable to calculate watershed area"
  )
})

test_that("cross section watershed lookup extracts a valid area", {
  lookup <- function(point) {
    list(
      drainage_basin = data.frame(area = 2564102.5641026)
    )
  }

  area <- cross_section_watershed_area(
    point = structure(list(), class = "sf"),
    watershed = "required",
    watershed_area_fn = lookup
  )

  expect_equal(area, 2564102.5641026)
})

test_that("cross sections can be processed without a watershed service call", {
  skip_if_not_installed("fluvgeodata")
  xs <- sf::st_read(
    system.file("extdata", "shiny", "xs.shp", package = "fluvgeodata"),
    quiet = TRUE
  )
  flowline_points <- sf::st_read(
    system.file("extdata", "shiny", "fl_pts.shp", package = "fluvgeodata"),
    quiet = TRUE
  )

  result <- cross_section(
    xs = xs,
    flowline_points = flowline_points,
    watershed = "skip"
  )

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(xs))
  expect_true(is.numeric(result$Watershed_Area_SqMile))
  expect_true(all(is.na(result$Watershed_Area_SqMile)))
  expect_true(check_cross_section(
    result,
    step = "station_points",
    watershed = "skip"
  ))
  expect_error(
    check_cross_section(result, step = "station_points"),
    "contains missing values"
  )
})
