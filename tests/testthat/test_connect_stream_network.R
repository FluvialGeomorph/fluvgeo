connect_test_lines <- function(coords) {
  sf::st_sf(
    stream_network_segment_id = .fg_generate_uuid(length(coords)),
    stream_network_observation_id = rep(.fg_generate_uuid(1L), length(coords)),
    direction_status = "CONFIRMED", segment_role = "UNRESOLVED",
    geometry = sf::st_sfc(lapply(coords, function(x) {
      sf::st_linestring(matrix(x, ncol = 2, byrow = TRUE))
    }), crs = 26915)
  )
}

test_that("hydroloom connections use correct stored direction and shared UUIDs", {
  x <- connect_test_lines(list(c(1,0, 0,1), c(1,0, 0,-1), c(2,0, 1,0)))
  before <- x
  out <- connect_stream_network(x)
  nodes <- out$stream_network_node
  edges <- out$stream_network
  connections <- out$stream_network_connection
  expect_identical(x, before)
  expect_identical(sf::st_geometry(edges), sf::st_geometry(x))
  expect_equal(edges$stream_network_segment_id, x$stream_network_segment_id)
  expect_equal(edges$segment_role, x$segment_role)
  expect_equal(nrow(nodes), 4L)
  expect_equal(sum(nodes$node_topology == "CONFLUENCE"), 1L)
  expect_equal(sum(nodes$node_topology == "UPSTREAM_BOUNDARY"), 2L)
  expect_equal(sum(nodes$node_topology == "DOWNSTREAM_BOUNDARY"), 1L)
  expect_true(all(grepl(.fg_uuid_pattern, nodes$node_id)))
  expect_equal(edges$downstream_node_id[1:2], rep(edges$upstream_node_id[3], 2))
  expect_setequal(connections$stream_network_segment_id[!is.na(connections$downstream_segment_id)],
                  x$stream_network_segment_id[1:2])
  expect_equal(connections$downstream_segment_id[!is.na(connections$downstream_segment_id)],
               rep(x$stream_network_segment_id[3], 2))
  expect_equal(connections$stream_network_segment_id[is.na(connections$downstream_segment_id)],
               x$stream_network_segment_id[3])
  expect_true(all(connections$node_id %in% nodes$node_id))
  expect_equal(tail(connections$stream_network_segment_id, 1), x$stream_network_segment_id[3])
  # IDs are reusable rather than tied to graph row positions.
  again <- connect_stream_network(edges[c(3,1,2),])
  expect_equal(again$stream_network$upstream_node_id, edges$upstream_node_id[c(3,1,2)])
  expect_equal(again$stream_network$downstream_node_id, edges$downstream_node_id[c(3,1,2)])
  expect_setequal(again$stream_network_node$node_id, nodes$node_id)
})

test_that("all diversion connections survive without choosing a main path", {
  x <- connect_test_lines(list(c(1,0, 0,0), c(2,1, 1,0), c(2,-1, 1,0),
                              c(3,0, 2,1), c(3,0, 2,-1), c(4,0, 3,0)))
  out <- connect_stream_network(x)
  c <- out$stream_network_connection
  first <- c[c$stream_network_segment_id == x$stream_network_segment_id[1],]
  expect_equal(nrow(first), 2L)
  expect_setequal(first$downstream_segment_id, x$stream_network_segment_id[2:3])
  expect_equal(nrow(c), 7L)
  expect_equal(sum(out$stream_network_node$node_topology == "DIVERGENCE"), 1L)
  expect_equal(sum(out$stream_network_node$node_topology == "CONFLUENCE"), 1L)
  # Every mapped relationship agrees independently with endpoint coordinates.
  for (i in which(!is.na(c$downstream_segment_id))) {
    a <- match(c$stream_network_segment_id[i], x$stream_network_segment_id)
    b <- match(c$downstream_segment_id[i], x$stream_network_segment_id)
    expect_equal(sf::st_geometry(x)[[a]][1,],
                 sf::st_geometry(x)[[b]][nrow(sf::st_geometry(x)[[b]]),])
  }
})

test_that("outlet and disconnected boundary rows retain typed missing downstream IDs", {
  x <- connect_test_lines(list(c(1,0, 0,0), c(2,0, 1.001,0)))
  out <- connect_stream_network(x)
  expect_equal(nrow(out$stream_network_node), 4L)
  expect_equal(nrow(out$stream_network_connection), 2L)
  expect_true(all(is.na(out$stream_network_connection$downstream_segment_id)))
  expect_type(out$stream_network_connection$downstream_segment_id, "character")
  single <- connect_stream_network(x[1,])
  expect_equal(nrow(single$stream_network_node), 2L)
  expect_equal(nrow(single$stream_network_connection), 1L)
})

test_that("unknown directions, directed cycles and ambiguous geometry block assignment", {
  cycle <- connect_test_lines(list(c(1,0, 0,0), c(1,1, 1,0), c(0,0, 1,1)))
  expect_error(connect_stream_network(cycle), "Directed cycle", class = "fluvgeo_connectivity_error")
  x <- connect_test_lines(list(c(1,0, 0,0)))
  x$direction_status <- "UNRESOLVED"
  expect_error(connect_stream_network(x), "CONFIRMED", class = "fluvgeo_connectivity_error")
  cases <- list(
    list(c(0,0, 1,0), c(1,0, 0,0)),
    list(c(0,0, 2,0), c(1,0, 3,0)),
    list(c(0,0, 2,0), c(1,-1, 1,1)),
    list(c(0,0, 2,0), c(1,0, 1,1)),
    list(c(0,0, 1,1, 0,1, 1,0)),
    list(c(0,0, 1,0, 1,1, 0,0))
  )
  for (coords in cases) {
    expect_error(connect_stream_network(connect_test_lines(coords)),
                 "geometry requires review", class = "fluvgeo_connectivity_error")
  }
})

test_that("identity collisions and invalid input contracts fail explicitly", {
  x <- connect_test_lines(list(c(1,0, 0,0), c(2,0, 1,0)))
  assigned <- connect_stream_network(x)$stream_network
  wrong <- assigned
  wrong$upstream_node_id[2] <- .fg_generate_uuid(1)
  expect_error(connect_stream_network(wrong), "Conflicting node IDs")
  wrong <- assigned
  wrong$downstream_node_id[2] <- wrong$upstream_node_id[1]
  expect_error(connect_stream_network(wrong), "different endpoint locations")
  assigned$upstream_node_id[2] <- NA_character_
  expect_equal(connect_stream_network(assigned)$stream_network$upstream_node_id[2],
               assigned$downstream_node_id[1])
  wrong <- x
  wrong$stream_network_segment_id[2] <- wrong$stream_network_segment_id[1]
  expect_error(connect_stream_network(wrong), "unique segment IDs")
  wrong <- x
  wrong$stream_network_observation_id[2] <- .fg_generate_uuid(1)
  expect_error(connect_stream_network(wrong), "one observation")
  expect_error(connect_stream_network(x[0,]), "nonempty")
  expect_error(connect_stream_network(sf::st_transform(x, 4326)), "projected")
  expect_error(connect_stream_network(sf::st_cast(x, "MULTILINESTRING")), "LINESTRING")
})

test_that("New Hope retains independently supplied branched and diversion relationships", {
  original <- hydroloom::hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
  expected <- hydroloom::to_flownetwork(original)
  source_ids <- as.character(original$id)
  x <- sf::st_sf(
    stream_network_segment_id = .fg_generate_uuid(nrow(original)),
    stream_network_observation_id = rep(.fg_generate_uuid(1), nrow(original)),
    direction_status = "CONFIRMED",
    geometry = sf::st_reverse(sf::st_geometry(suppressWarnings(sf::st_cast(original, "LINESTRING"))))
  )
  result <- connect_stream_network(x)
  actual <- result$stream_network_connection
  ids <- x$stream_network_segment_id
  expected_from <- ids[match(as.character(expected$id), source_ids)]
  expected_to <- ids[match(as.character(expected$toid), source_ids)]
  expect_setequal(paste(actual$stream_network_segment_id, actual$downstream_segment_id),
                  paste(expected_from, expected_to))
  expect_equal(nrow(actual), 832L)
  expect_equal(nrow(result$stream_network_node), 663L)
  expect_equal(nrow(result$stream_network), 746L)
  expect_identical(sf::st_geometry(result$stream_network), sf::st_geometry(x))
})
