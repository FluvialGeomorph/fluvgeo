corridor_lines <- function() sf::st_sf(source_id = c("101", "102"),
  geometry = sf::st_sfc(sf::st_linestring(rbind(c(-90, 40), c(-90, 40.01))),
    sf::st_linestring(rbind(c(-90, 40.01), c(-90, 40.02))), crs = 4326))
corridor_parent <- function() sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(
  rbind(c(-90.1,39.9), c(-89.9,39.9), c(-89.9,40.1), c(-90.1,40.1), c(-90.1,39.9)))), crs = 4326))

test_that("clip-first handles crossing, discarded and point-only selections with CRS equivalence", {
  parent <- corridor_parent()
  lines <- sf::st_sf(source_id=c("crossing","outside","touch"),geometry=sf::st_sfc(
    sf::st_linestring(rbind(c(-90.2,40),c(-90,40))),
    sf::st_linestring(rbind(c(-91,40),c(-90.9,40))),
    sf::st_linestring(rbind(c(-90.2,39.8),c(-90.1,39.9))),crs=4326))
  original <- lines
  value <- preview_stream_corridor(lines,1000,"ft",parent)
  expect_identical(lines,original)
  expect_equal(value$retained_features,1L)
  expect_equal(value$clipped_lines$source_id,"crossing")
  expect_equal(value$line_lengths$retained_m[2:3],c(0,0))
  expect_equal(value$distance_m,304.8)
  expect_error(preview_stream_corridor(lines[3,],100,boundary=parent),"no positive-length")
  other <- preview_stream_corridor(sf::st_transform(lines,3857),1000,"ft",sf::st_transform(parent,3857))
  expect_equal(value$line_lengths$retained_m,other$line_lengths$retained_m,tolerance=.002)
  expect_equal(as.numeric(sf::st_area(value$area)),as.numeric(sf::st_area(other$area)),tolerance=.01)
  folder <- withr::local_tempdir()
  start_study_context(file.path(folder,"start.gpkg"),"Clip-first study")
  revise_study_context(file.path(folder,"start.gpkg"),file.path(folder,"parent.gpkg"),
    study_area_boundary=parent,add_note="Synthetic")
  saved <- add_study_stream_corridor(file.path(folder,"parent.gpkg"),file.path(folder,"stream.gpkg"),
    lines,"Crossing",1000,"ft",add_note="Synthetic clip-first")
  evidence <- sf::st_read(saved$evidence,layer="selected_lines",quiet=TRUE)
  clipped <- sf::st_read(saved$evidence,layer="clipped_lines",quiet=TRUE)
  expect_equal(evidence$source_id,lines$source_id)
  expect_equal(sf::st_coordinates(evidence),sf::st_coordinates(original))
  expect_equal(clipped$source_id,"crossing")
  context <- read_study_context(saved$context)
  expect_equal(check_study_area_containment(context$study_area,context$streams)$status,"inside")
})

test_that("slanted clipping edges pass overlay verification and publish unchanged", {
  x <- -90 + .00123456789
  parent <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(rbind(
    c(x,40),c(x+.02,40.0123456789),c(x+.01,40.03),c(x-.01,40.02),c(x,40)))),crs=4326))
  lines <- sf::st_sf(source_id="slanted",geometry=sf::st_sfc(sf::st_linestring(rbind(
    c(x+.001,40.015),c(x+.009,40.018))),crs=4326))
  preview <- preview_stream_corridor(lines,1000,boundary=parent)
  preview$area$stream_name <- "Slanted"
  expect_true(preview$clipped)
  expect_equal(check_study_area_containment(parent,preview$area)$status,"inside")
  outside <- sf::st_difference(.fg_corridor_grid(preview$area,sf::st_crs(preview$area)),
    sf::st_buffer(.fg_corridor_grid(parent,sf::st_crs(preview$area)),.002))
  expect_true(!length(outside) || all(sf::st_is_empty(outside)))
  # A centimetre displacement exceeds the explicit 2 mm precision margin.
  shifted <- sf::st_set_geometry(preview$area,sf::st_geometry(preview$area)+c(.01,0))
  sf::st_crs(shifted) <- sf::st_crs(preview$area)
  expect_equal(check_study_area_containment(parent,shifted)$status,"outside")
  folder <- withr::local_tempdir()
  start_study_context(file.path(folder,"start.gpkg"),"Slanted study")
  revise_study_context(file.path(folder,"start.gpkg"),file.path(folder,"parent.gpkg"),
    study_area_boundary=parent,add_note="Synthetic slanted-edge regression")
  result <- add_study_stream_corridor(file.path(folder,"parent.gpkg"),file.path(folder,"stream.gpkg"),
    lines,"Slanted",1000,add_note="Synthetic clipping check")
  saved <- read_study_context(result$context)
  expect_equal(check_study_area_containment(saved$study_area,saved$streams)$status,"inside")
  expect_equal(sf::st_coordinates(saved$streams),sf::st_coordinates(preview$area))
  expect_true(sf::st_crs(saved$streams) == sf::st_crs(preview$area))
})

test_that("boundary-coincident lines are retained while buffer overflow is clipped", {
  parent <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(-90,39.9), c(-89.9,39.9), c(-89.9,40.1), c(-90,40.1), c(-90,39.9)))), crs=4326))
  lines <- corridor_lines(); original <- lines
  value <- preview_stream_corridor(lines, 100, boundary=parent)
  expect_true(value$clipped)
  expect_gt(value$removed_area_m2, 0)
  expect_true(sf::st_is_valid(value$area))
  expect_identical(lines, original)
  value$area$stream_name <- "Edge"
  expect_equal(check_study_area_containment(parent, value$area)$status, "inside")
  expect_equal(value$retained_features,2L)
  expect_false(preview_stream_corridor(lines,100,boundary=corridor_parent())$clipped)
  crossing <- lines
  sf::st_geometry(crossing) <- sf::st_sfc(sf::st_linestring(rbind(c(-90.01,40),c(-89.99,40))),
    sf::st_geometry(lines)[[2]], crs=4326)
  crossed <- preview_stream_corridor(crossing,100,boundary=parent)
  expect_true(crossed$line_clipped)
  expect_lt(sum(crossed$line_lengths$retained_m),sum(crossed$line_lengths$original_m))
  folder <- withr::local_tempdir()
  start_study_context(file.path(folder,"start.gpkg"),"Edge study")
  revise_study_context(file.path(folder,"start.gpkg"),file.path(folder,"boundary.gpkg"),study_area_boundary=parent,add_note="Test")
  saved <- add_study_stream_corridor(file.path(folder,"boundary.gpkg"),file.path(folder,"stream.gpkg"),lines,"Edge",100,add_note="Test")
  context <- read_study_context(saved$context)
  expect_true(all(sf::st_equals(context$streams,value$area,sparse=FALSE)))
  evidence <- sf::st_read(saved$evidence,layer="selected_lines",quiet=TRUE)
  expect_true(all(as.logical(evidence$fg_buffer_clipped)))
  expect_equal(evidence$fg_removed_m2,rep(value$removed_area_m2,2))
  expect_match(context$analyst_notes,"clipped=TRUE")
  expect_equal(check_study_area_containment(parent,context$streams)$status,"inside")
  # A hole is an excluded area, even when both line endpoints are inside.
  holed <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(
    sf::st_geometry(corridor_parent())[[1]][[1]],
    rbind(c(-90.01,40.005),c(-90.01,40.015),c(-89.99,40.015),c(-89.99,40.005),c(-90.01,40.005)))),crs=4326))
  hole_preview <- preview_stream_corridor(lines,100,boundary=holed)
  expect_true(hole_preview$line_clipped)
  expect_lt(sum(hole_preview$line_lengths$retained_m),sum(hole_preview$line_lengths$original_m))
  near <- lines
  sf::st_geometry(near) <- sf::st_geometry(near) + c(-.000001,0)
  sf::st_crs(near) <- 4326
  expect_error(preview_stream_corridor(near,100,boundary=parent),"outside")
})

test_that("corridor buffers have explicit units, retained inputs and bounded valid geometry", {
  x <- corridor_lines(); original <- x
  s2 <- sf::sf_use_s2()
  a <- preview_stream_corridor(x, 100, "m")
  b <- preview_stream_corridor(x, 100 / .3048, "ft")
  expect_equal(a$distance_m, b$distance_m)
  expect_true(isTRUE(sf::st_equals(a$area, b$area, sparse = FALSE)[1,1]))
  expect_equal(a$polygon_parts, 1)
  expect_true(sf::st_is_valid(preview_stream_corridor(x,10)$area))
  separated <- x
  g <- sf::st_geometry(separated); g[[2]] <- g[[2]] + c(.1,0)
  sf::st_geometry(separated) <- g
  expect_equal(preview_stream_corridor(separated,10)$polygon_parts,2)
  expect_identical(x, original)
  expect_identical(sf::sf_use_s2(), s2)
  expect_error(preview_stream_corridor(x, 0), "positive")
  expect_error(preview_stream_corridor(x, 1, "US feet"), "unit")
  expect_error(preview_stream_corridor(x, 10001), "10,000")
  expect_error(preview_stream_corridor(corridor_parent(), 10), "line features")
  named <- sf::st_sf(stream_name="Creek", geometry=sf::st_geometry(a$area))
  expect_equal(check_study_area_containment(corridor_parent(), named)$status, "inside")
  expect_equal(check_study_area_containment(corridor_parent(), data.frame(stream_name="Unknown"))$status, "unknown")
  same <- corridor_parent(); same$stream_name <- "Boundary touching"
  expect_equal(check_study_area_containment(corridor_parent(), same)$status, "inside")
  shifted <- sf::st_set_geometry(named, sf::st_geometry(named) + c(1, 0)); sf::st_crs(shifted) <- 4326
  expect_equal(check_study_area_containment(corridor_parent(), shifted)$status, "outside")
})

test_that("corridor publication appends identities and retains evidence without replacing sources", {
  folder <- withr::local_tempdir()
  source <- file.path(folder, "study.gpkg")
  start_study_context(source, "Study")
  ready <- file.path(folder, "boundary.gpkg")
  revise_study_context(source, ready, study_area_boundary=corridor_parent(), add_note="Test boundary")
  hash <- tools::md5sum(ready)
  result <- add_study_stream_corridor(ready, file.path(folder,"stream.gpkg"), corridor_lines(), "Creek", 100, add_note="Test")
  saved <- read_study_context(result$context)
  expect_equal(saved$streams$stream_id, result$stream_id)
  expect_equal(saved$streams$stream_name, "Creek")
  expect_identical(tools::md5sum(ready), hash)
  evidence <- sf::st_read(result$evidence, layer="selected_lines", quiet=TRUE)
  expect_equal(evidence$source_id, c("101", "102"))
  expect_equal(evidence$fg_buffer_m, c(100,100))
  expect_equal(evidence$fg_stream_id, rep(result$stream_id,2))
  expect_match(saved$analyst_notes, "SHA256")
  second <- add_study_stream_corridor(result$context, file.path(folder,"second.gpkg"), corridor_lines(), "Second", 50, add_note="Test")
  expect_equal(nrow(read_study_context(second$context)$streams), 2)
  expect_equal(read_study_context(second$context)$streams$stream_id[1], result$stream_id)
  expect_error(add_study_stream_corridor(second$context, file.path(folder,"bad.gpkg"), corridor_lines(), "Creek", 10, add_note="Test"), "already exists")
  expect_error(add_study_stream_corridor(source, file.path(folder,"bad.gpkg"), corridor_lines(), "New", 10, add_note="Test"), "boundary")
  expect_false(file.exists(file.path(folder,"bad.gpkg")))
  outside <- corridor_lines()
  sf::st_geometry(outside) <- sf::st_geometry(outside) + c(1,0); sf::st_crs(outside) <- 4326
  count <- length(list.files(folder,pattern="stream-selection"))
  expect_error(add_study_stream_corridor(second$context, file.path(folder,"bad.gpkg"), outside, "Outside", 10, add_note="Test"), "outside")
  expect_equal(length(list.files(folder,pattern="stream-selection")),count)
  expect_error(add_study_stream_corridor(second$context, second$context, corridor_lines(), "Other", 10, add_note="Test"), "new context")
  named <- file.path(folder,"named.gpkg")
  define_study_streams(ready,named,data.frame(stream_name="Named"),add_note="Test")
  identity <- read_study_context(named)$streams$stream_id
  assigned <- add_study_stream_corridor(named,file.path(folder,"assigned.gpkg"),corridor_lines(),"Ignored",10,add_note="Test",stream_id=identity)
  expect_equal(read_study_context(assigned$context)$streams$stream_id,identity)
  expect_equal(read_study_context(assigned$context)$streams$stream_name,"Named")
  multi <- file.path(folder,"multi.gpkg")
  define_study_streams(ready,multi,data.frame(stream_name=c("One","Two")),add_note="Test")
  expect_error(add_study_stream_corridor(multi,file.path(folder,"bad.gpkg"),corridor_lines(),"Ignored",10,add_note="Test",
    stream_id=read_study_context(multi)$streams$stream_id[1]),"Multiple names-only")
})
