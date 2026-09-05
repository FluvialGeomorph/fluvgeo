logical_test_lines <- function(coords, boundary = rep(NA_character_, length(coords))) {
  sf::st_sf(boundary = boundary,
            geometry = sf::st_sfc(lapply(coords, function(x) {
              sf::st_linestring(matrix(x, ncol = 2, byrow = TRUE))
            }), crs = 26915))
}

test_that("logical links join continuations with complete source membership", {
  x <- logical_test_lines(list(c(0,0, 1,0), c(2,0, 1,0), c(2,0, 3,0)))
  before <- x
  out <- build_logical_stream_links(x, "boundary")
  expect_identical(x, before)
  expect_equal(nrow(out$links), 1L)
  expect_equal(out$membership$input_row, 1:3)
  expect_equal(out$membership$link_row, rep(1L, 3))
  expect_true(is.na(out$links$boundary))
  expect_equal(as.numeric(sf::st_length(out$links)), 3)
  expect_true(sf::st_equals(sf::st_union(x), sf::st_union(out$links), sparse = FALSE)[1,1])
  expect_equal(sf::st_crs(out$links), sf::st_crs(x))
  again <- build_logical_stream_links(out$links, "boundary")
  expect_identical(sf::st_geometry(again$links), sf::st_geometry(out$links))
})

test_that("semantic and explicit endpoint boundaries survive", {
  x <- logical_test_lines(list(c(0,0, 1,0), c(1,0, 2,0), c(2,0, 3,0)))
  x$boundary <- c(NA, "reach", "reach")
  expect_equal(nrow(build_logical_stream_links(x, "boundary")$links), 2L)
  x$stream <- c("a", "a", "b")
  expect_equal(nrow(build_logical_stream_links(x, c("boundary", "stream"))$links), 3L)
  p <- sf::st_sfc(sf::st_point(c(2,0)), crs = 26915)
  out <- build_logical_stream_links(x, "boundary", p)
  expect_equal(nrow(out$links), 3L)
  expect_identical(sf::st_geometry(out$links), sf::st_geometry(x))
  expect_error(build_logical_stream_links(x, protected_nodes = sf::st_buffer(p, 1)), "POINT")
  expect_error(build_logical_stream_links(x, protected_nodes = p + c(0.1,0)), "CRS|exactly match")
  p2 <- sf::st_sfc(sf::st_point(c(1.5,0)), crs = 26915)
  expect_error(build_logical_stream_links(x, protected_nodes = p2), "exactly match")
  expect_error(build_logical_stream_links(x, protected_nodes = sf::st_transform(p, 3857)), "CRS")
})

test_that("branches, gaps, and disconnected components are not erased", {
  x <- logical_test_lines(list(c(0,0, 1,0), c(1,0, 2,0), c(1,0, 1,1),
                              c(10,0, 11,0), c(11,0, 12,0)))
  out <- build_logical_stream_links(x)
  expect_equal(nrow(out$links), 4L)
  expect_setequal(out$membership$input_row, 1:5)
  graph <- sfnetworks::as_sfnetwork(out$links, directed = FALSE)
  expect_equal(igraph::components(graph)$no, 2L)
  expect_equal(sum(igraph::degree(graph) == 3L), 1L)
  gap <- logical_test_lines(list(c(0,0, 1,0), c(1,0, 2,0), c(1.001,0, 3,1)))
  out <- build_logical_stream_links(gap, tolerance = 0.01)
  expect_identical(sf::st_geometry(out$links), sf::st_geometry(gap))
})

test_that("defective and cyclic geometry stays available for assessment", {
  cases <- list(
    list(c(0,0, 1,0), c(1,0, 0,0)), # reversed duplicates
    list(c(0,0, 2,0), c(1,0, 3,0)), # overlap
    list(c(0,0, 2,0), c(1,-1, 1,1)), # crossing
    list(c(0,0, 2,0), c(1,0, 1,1)), # unsplit T
    list(c(0,0, 1,0), c(1,0, 1,1), c(1,1, 0,0)), # ring
    list(c(0,0, 1,1, 0,1, 1,0)), # self-intersection
    list(c(0,0, 1,0, 1,1, 0,0)) # closed single line
  )
  for (coords in cases) {
    x <- logical_test_lines(coords)
    out <- build_logical_stream_links(x)
    expect_identical(sf::st_geometry(out$links), sf::st_geometry(x))
    expect_equal(out$membership$input_row, seq_len(nrow(x)))
  }
})

test_that("logical-link input contracts fail clearly", {
  x <- logical_test_lines(list(c(0,0, 1,0)))
  expect_error(build_logical_stream_links(x[0,]), "nonempty")
  expect_error(build_logical_stream_links(sf::st_transform(x, 4326)), "projected")
  expect_error(build_logical_stream_links(sf::st_cast(x, "MULTILINESTRING")), "LINESTRING")
  expect_error(build_logical_stream_links(x, "absent"), "boundary_fields")
  expect_error(build_logical_stream_links(x, tolerance = -1), "nonnegative")
  expect_error(build_logical_stream_links(x, tolerance = NA_real_), "nonnegative")
  expect_error(build_logical_stream_links(x, tolerance = c(0,1)), "nonnegative")
})

test_that("retained Sinsinawa produces one logical link and retains all 99 sources", {
  x <- suppressWarnings(sf::st_cast(sf::st_read(
    system.file("extdata/testing_data.gdb", package = "fluvgeodata"),
    layer = "stream_network", quiet = TRUE), "LINESTRING"))
  out <- build_logical_stream_links(x, "ReachName", tolerance = 0.01)
  expect_equal(nrow(out$links), 1L)
  expect_equal(nrow(out$membership), 99L)
  expect_equal(as.numeric(sum(sf::st_length(out$links))),
               as.numeric(sum(sf::st_length(x))), tolerance = 1e-7)
})
