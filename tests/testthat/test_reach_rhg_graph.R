context("reach_rhg_graph")

# Extract attribute data from the fgm::sin_xs_points SpatialPointsDataFrame
sin_xs_points_df <- fgm::sin_xs_points@data

# Set variable values
streams <- "Sinsinawa"
regions <- c("Eastern United States", "IN Central Till Plain")
bankfull_elevations <- seq(103, 104, 0.1)

# Call the xs_dimensions function
sin <- xs_dimensions(xs_points = sin_xs_points_df,
                     streams = streams,
                     regions = regions,
                     bankfull_elevations = bankfull_elevations)

# Call the reach_rhg_graph function
sin_reach_rhg_graph <- reach_rhg_graph(xs_dims = sin,
                                       streams = streams,
                                       bf_elevation = bankfull_elevations)

# Print the reach_rhg_graph
sin_reach_rhg_graph

test_that("Check parameters", {
  expect_error(reach_rhg_graph(10, streams, regions, bankfull_elevations),
               info = "xs_dims not a dataframe")
  expect_error(reach_rhg_graph(sin[, !names(sin) %in% c("reach_name")],
                               streams, bankfull_elevations),
               info = "xs_dims is missing reach_name field")
  expect_error(reach_rhg_graph(sin[, !names(sin) %in% c("cross_section")],
                               streams, bankfull_elevations),
               info = "xs_dims is missing cross_section field")
  expect_error(reach_rhg_graph(sin[, !names(sin) %in% c("xs_type")],
                               streams, bankfull_elevations),
               info = "xs_dims is missing xs_type field")
  expect_error(reach_rhg_graph(sin[, !names(sin) %in% c("bankfull_elevation")],
                               streams, bankfull_elevations),
               info = "xs_dims is missing bankfull_elevation field")
  expect_error(reach_rhg_graph(sin[, !names(sin) %in% c("drainage_area")],
                               streams, bankfull_elevations),
               info = "xs_dims is missing drainage_area field")
  expect_error(reach_rhg_graph(sin[, !names(sin) %in% c("xs_area")],
                               streams, bankfull_elevations),
               info = "xs_dims is missing xs_area field")
  expect_error(reach_rhg_graph(sin[, !names(sin) %in% c("xs_width")],
                               streams, bankfull_elevations),
               info = "xs_dims is missing xs_width field")
  expect_error(reach_rhg_graph(sin[, !names(sin) %in% c("xs_depth")],
                               streams, bankfull_elevations),
               info = "xs_dims is missing xs_depth field")
  expect_error(reach_rhg_graph(sin, 8,
                               bankfull_elevations),
               info = "streams is not a character vector")
  expect_error(reach_rhg_graph(sin, streams, "a"),
               info = "bankfull_elevations is not a numeric vector")
})

test_that("Check the plot object", {
  expect_true(ggplot2::is.ggplot(sin_reach_rhg_graph))
  expect_equal(sin_reach_rhg_graph$labels$x,
               "Drainage Area (square miles)")
  expect_equal(sin_reach_rhg_graph$labels$y,
               "")
})

test_that("Check the plot's layer geom with 'log_scale' TRUE", {
  t <- reach_rhg_graph(xs_dims = sin,
                       streams = streams,
                       bf_elevation = 103.5,
                       xs_trend = FALSE,
                       log_scale = TRUE)
  expect_equal(class(t$layers[[1]]$geom)[1],
               "GeomLine")
})

test_that("Check the plot's layer geom with 'xs_trend' TRUE", {
  t <- reach_rhg_graph(xs_dims = sin,
                       streams = streams,
                       bf_elevation = bankfull_elevations,
                       xs_trend = TRUE,
                       log_scale = FALSE)
  expect_equal(class(t$layers[[1]]$geom)[1],
            "GeomLine")
})

test_that("Check the plot's layer geom with 'log_scale' TRUE", {
  t <- reach_rhg_graph(xs_dims = sin,
                       streams = streams,
                       bf_elevation = bankfull_elevations,
                       xs_trend = FALSE,
                       log_scale = TRUE)
  expect_equal(class(t$layers[[1]]$geom)[1],
               "GeomLine")
})

test_that("Check the plot's layer geom with 'log_scale' and
          'xs_trend' both TRUE", {
  t <- reach_rhg_graph(xs_dims = sin,
                       streams = streams,
                       bf_elevation = bankfull_elevations,
                       xs_trend = TRUE,
                       log_scale = TRUE)
  expect_equal(class(t$layers[[1]]$geom)[1],
               "GeomLine")
})
