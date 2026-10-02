test_that("NAIP image dimensions preserve shape within service limits", {
  square <- .fg_naip_dimensions(c(xmin = 0, ymin = 0, xmax = 100, ymax = 100))
  wide <- .fg_naip_dimensions(c(xmin = 0, ymin = 0, xmax = 1000, ymax = 100))
  tall <- .fg_naip_dimensions(c(xmin = 0, ymin = 0, xmax = 100, ymax = 1000))

  expect_identical(unname(square), c(2000L, 2000L))
  expect_lte(wide[["columns"]], 15000L)
  expect_lte(wide[["rows"]], 4100L)
  expect_equal(wide[["columns"]] / wide[["rows"]], 10, tolerance = 0.01)
  expect_equal(tall[["columns"]] / tall[["rows"]], 0.1, tolerance = 0.01)
  expect_error(.fg_naip_dimensions(c(xmin = 0, ymin = 0, xmax = 1, ymax = 1),
                                   max_cells = 0), "positive")
})

test_that("NAIP retrieval produces a cached RGB SpatRaster", {
  location <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
    c(xmin = -93.66, ymin = 42.08, xmax = -93.62, ymax = 42.12),
    crs = sf::st_crs(4326)
  )))
  calls <- 0L
  transfer <- function(request, path) {
    calls <<- calls + 1L
    fixture <- terra::rast(nrows = 10, ncols = 10, nlyrs = 4,
                           xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                           crs = "EPSG:3857")
    terra::values(fixture) <- 1
    terra::writeRaster(fixture, path, overwrite = TRUE)
    invisible(NULL)
  }

  first <- .fg_naip_image(location, transfer = transfer)
  second <- .fg_naip_image(location, transfer = transfer)

  expect_s4_class(first, "SpatRaster")
  expect_equal(terra::nlyr(first), 3L)
  expect_identical(names(first), c("red", "green", "blue"))
  expect_equal(terra::values(first), terra::values(second))
  expect_identical(calls, 1L)
})
