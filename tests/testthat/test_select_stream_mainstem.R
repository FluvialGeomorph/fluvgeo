mainstem_fixture <- function() {
  geometry <- sf::st_sfc(
    sf::st_linestring(matrix(c(-2, 3, 1, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 1, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 1, 3, 1), ncol = 2, byrow = TRUE)),
    crs = 26915
  )
  network <- sf::st_sf(stream_line_id = c("A", "B", "T"),
    upstream_cell = c(1, 2, 3), downstream_cell = c(3, 3, 4), geometry = geometry)
  reference <- sf::st_sf(source_id = "100",
    geometry = sf::st_sfc(sf::st_linestring(
      matrix(c(0, 1, 3, 1), ncol = 2, byrow = TRUE)), crs = 26915))
  list(network = network, reference = reference)
}

test_that("reference-constrained selection excludes the longer wrong tributary", {
  x <- mainstem_fixture()
  result <- select_stream_mainstem(x$network, x$reference)

  expect_s3_class(result$flowline, "sf")
  expect_equal(nrow(result$flowline), 1L)
  expect_identical(result$selected_segments$stream_line_id, c("B", "T"))
  expect_equal(result$flowline$selected_head_cell, 2)
  expect_equal(result$flowline$reference_hausdorff_m, 0)
  expect_true(result$candidates$length_m[result$candidates$head_cell == 1] >
    result$candidates$length_m[result$candidates$head_cell == 2])
  expect_equal(unname(sf::st_coordinates(result$flowline)[1, c("X", "Y")]), c(3, 1))
  expect_equal(unname(tail(sf::st_coordinates(result$flowline)[, c("X", "Y")], 1)),
    matrix(c(0, 1), nrow = 1))
})

test_that("mainstem selection is stable when network rows are reordered", {
  x <- mainstem_fixture()
  expected <- select_stream_mainstem(x$network, x$reference)
  actual <- select_stream_mainstem(x$network[c(3, 1, 2), ], x$reference)

  expect_identical(actual$selected_segments$stream_line_id,
    expected$selected_segments$stream_line_id)
  expect_equal(sf::st_geometry(actual$flowline), sf::st_geometry(expected$flowline))
  expect_equal(actual$candidates[order(actual$candidates$head_cell),
    c("head_cell", "length_m", "reference_hausdorff_m", "selected")],
    expected$candidates[order(expected$candidates$head_cell),
      c("head_cell", "length_m", "reference_hausdorff_m", "selected")])
})

test_that("selected terrain path enters the existing flowline contract", {
  x <- mainstem_fixture()
  selected <- select_stream_mainstem(x$network, x$reference)$flowline
  dem <- -terra::init(terra::rast(ncols = 7, nrows = 5,
    extent = terra::ext(-3, 4, -1, 4), crs = "EPSG:26915"), "x")

  prepared <- flowline(selected, "Fixture Reach", dem)

  expect_true(check_flowline(prepared, "create_flowline"))
  expect_identical(prepared$ReachName, "Fixture Reach")
  expect_equal(sf::st_geometry(prepared), sf::st_geometry(selected))
})

test_that("mainstem selection fails closed for invalid directed topology", {
  x <- mainstem_fixture()
  disconnected <- x$network
  disconnected$downstream_cell[1] <- 99
  expect_error(select_stream_mainstem(disconnected, x$reference),
    "exactly one observed outlet")

  duplicate <- x$network
  duplicate$upstream_cell[2] <- duplicate$upstream_cell[1]
  expect_error(select_stream_mainstem(duplicate, x$reference),
    "complete and unique")

  expect_error(select_stream_mainstem(x$network, sf::st_set_crs(x$reference, NA)),
    "CRS-defined reference")
})
