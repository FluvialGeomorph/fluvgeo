drainage_test_point <- function(x = 0, y = 0) sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(x, y)), crs = 4326))
drainage_test_line <- function() sf::st_sf(comid = "123", geometry = sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(0, 0.01))), crs = 4326))
drainage_test_area <- function() sf::st_sf(huc12 = "012345678901", name = "Example", geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(-.1,-.1),c(.1,-.1),c(.1,.1),c(-.1,.1),c(-.1,-.1)))), crs = 4326))
drainage_test_index <- function(x = 0) { a <- drainage_test_point(x); a$comid <- "123"; a }

test_that("snap selection preserves click, identity and service position", {
  local_mocked_bindings(drainage_index_service = function(x) drainage_test_index(),
    drainage_feature_service = function(id) drainage_test_line())
  x <- locate_drainage_stream(drainage_test_point(.0001))
  expect_s3_class(x, "fg_drainage_location")
  expect_equal(x$comid, "123")
  expect_gt(x$snap_distance_m, 10)
  expect_lt(x$snap_distance_m, 12)
  expect_equal(unname(sf::st_coordinates(x$query_point)[1,1]), .0001)
  expect_equal(unname(sf::st_coordinates(x$snapped_point)[1,1]), 0)
  expect_equal(sf::st_crs(x$flowline)$epsg, 4326)
  expect_match(x$method, "nearest")
  expect_error(locate_drainage_stream(drainage_test_point(1)), "200 metres")
  expect_error(locate_drainage_stream(sf::st_set_crs(drainage_test_point(), NA)), "CRS")
  expect_error(locate_drainage_stream(drainage_test_line()), "geometry")
  expect_error(locate_drainage_stream(rbind(drainage_test_point(),drainage_test_point())), "exactly one")
})

test_that("unavailable, ambiguous and mismatched stream results fail closed", {
  local_mocked_bindings(drainage_index_service = function(x) NULL)
  expect_error(locate_drainage_stream(drainage_test_point()), "No usable")
  local_mocked_bindings(drainage_index_service = function(x) rbind(drainage_test_index(),drainage_test_index()))
  expect_error(locate_drainage_stream(drainage_test_point()), "unique")
  local_mocked_bindings(drainage_index_service = function(x) drainage_test_index(),
    drainage_feature_service = function(id) { x <- drainage_test_line(); x$comid <- "999"; x })
  expect_error(locate_drainage_stream(drainage_test_point()), "does not match")
})

test_that("context keeps partial success and explicit distance and scope", {
  seen <- list()
  local_mocked_bindings(drainage_index_service = function(x) drainage_test_index(),
    drainage_feature_service = function(id) drainage_test_line(),
    drainage_huc_service = function(point) drainage_test_area(),
    drainage_basin_service = function(id) stop("Example service unavailable"),
    drainage_navigation_service = function(id, mode, distance) {
      seen[[mode]] <<- list(id = id, distance = distance)
      drainage_test_line()
    })
  location <- locate_drainage_stream(drainage_test_point())
  x <- get_drainage_context(location, 30)
  expect_identical(names(x$layers), c("huc12", "basin", "upstream", "downstream"))
  expect_identical(x$location, location)
  expect_equal(x$status$status, c("available", "unavailable", "available", "available"))
  expect_match(x$status$detail[2], "service unavailable")
  expect_null(x$layers$basin)
  expect_equal(x$layers$huc12$huc12, "012345678901")
  expect_equal(seen$UT, list(id = "123", distance = 30))
  expect_equal(seen$DM, seen$UT)
  expect_true(is.na(x$network_complete))
  expect_match(x$sources[["huc12"]], "2025")
  expect_error(get_drainage_context(location, NA_real_), "distance")
  expect_error(get_drainage_context(location, 201), "distance")
  expect_error(get_drainage_context(list(comid="123")), "located stream")
})

test_that("HUC response is spatially filtered and malformed layers are unavailable", {
  local_mocked_bindings(drainage_index_service = function(x) drainage_test_index(),
    drainage_feature_service = function(id) drainage_test_line(),
    drainage_huc_service = function(point) { x <- drainage_test_area(); sf::st_geometry(x) <- sf::st_geometry(x) + c(10,10); sf::st_set_crs(x,4326) },
    drainage_basin_service = function(id) drainage_test_line(),
    drainage_navigation_service = function(...) NULL)
  x <- get_drainage_context(locate_drainage_stream(drainage_test_point()))
  expect_true(all(x$status$status == "unavailable"))
  expect_true(all(vapply(x$layers, is.null, logical(1))))
  expect_equal(x$status$outcome, c("no_features", "unresolved", "unresolved", "unresolved"))
})

test_that("optional names use one keyed attribute lookup without changing geometry", {
  line <- drainage_test_line()
  a <- line; a$comid <- "456"
  layers <- list(huc12 = NULL, basin = NULL, upstream = rbind(line, a), downstream = line)
  seen <- NULL
  local_mocked_bindings(drainage_names_service = function(ids) {
    seen <<- ids
    data.frame(comid=c("456", "123"), gnis_name=c(" ", "Test Creek"))
  })
  x <- drainage_channel_names(layers)
  expect_equal(seen, c("123", "456"))
  expect_equal(x$layers$upstream$gnis_name, c("Test Creek", NA_character_))
  expect_identical(sf::st_geometry(x$layers$upstream), sf::st_geometry(layers$upstream))
  expect_equal(x$layers$downstream$gnis_name, "Test Creek")
  local_mocked_bindings(drainage_names_service = function(ids) stop("HTTP 503"))
  x <- drainage_channel_names(layers)
  expect_identical(x$layers, layers)
  expect_equal(x$status, "unresolved")
  expect_match(x$detail, "503")
  big <- line[rep(1,501), ]; big$comid <- as.character(seq_len(501))
  expect_equal(drainage_channel_names(list(upstream=big))$status, "skipped")
})

test_that("layer outcomes retain transport evidence separately from empty and unknown", {
  local_mocked_bindings(drainage_index_service = function(x) drainage_test_index(),
    drainage_feature_service = function(id) drainage_test_line(),
    drainage_huc_service = function(point) drainage_test_area()[0, ],
    drainage_basin_service = function(id) { warning("Failed to get features: HTTP 502 Bad Gateway"); NULL },
    drainage_navigation_service = function(id, mode, distance) if (mode == "UT") NULL else drainage_test_line())
  x <- get_drainage_context(locate_drainage_stream(drainage_test_point()))
  expect_equal(x$status$outcome, c("no_features", "service_unavailable", "unresolved", "available"))
  expect_match(x$status$detail[2], "502")
  expect_false(grepl("502", x$status$detail[3]))
  expect_equal(nrow(x$layers$downstream), 1L)
  local_mocked_bindings(drainage_index_service = function(x) drainage_test_index()[0, ])
  e <- tryCatch(locate_drainage_stream(drainage_test_point()), error = identity)
  expect_equal(e$code, "no_features")
  local_mocked_bindings(drainage_index_service = function(x) { warning("Connection timed out"); drainage_test_index()[0, ] })
  e <- tryCatch(locate_drainage_stream(drainage_test_point()), error = identity)
  expect_equal(e$code, "service_unavailable")
})
