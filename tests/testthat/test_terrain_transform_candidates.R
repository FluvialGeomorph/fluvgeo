test_that("candidate discovery preserves full operations and never selects one", {
  network <- sf::sf_proj_network()
  x <- terrain_transform_candidates("EPSG:6344+5703","EPSG:6344+8228",c(-91.1,41.5,-90.8,41.7))
  expect_identical(sf::sf_proj_network(),network)
  expect_length(x$candidates,1L)
  p <- x$candidates[[1L]]
  expect_true(p$selectable)
  expect_match(p$definition,"z_in=m.*z_out=ft")
  expect_identical(p$accuracy,0)
  expect_true(is.null(x$selected))
  expect_true(nzchar(x$software$databases[[1L]]$sha256))
  expect_identical(x$fingerprint,terrain_transform_candidates("EPSG:6344+5703",
    "EPSG:6344+8228",c(-91.1,41.5,-90.8,41.7))$fingerprint)
  expect_false(identical(x$fingerprint,terrain_transform_candidates("EPSG:6344+5703",
    "EPSG:6344+8228",c(-91.1,41.5,-90.8,41.8))$fingerprint))
})

test_that("ballpark and missing resources are visible but not selectable", {
  x <- terrain_transform_candidates("EPSG:26915","EPSG:6344",c(-94,41,-90,43))
  expect_gt(length(x$candidates),0)
  for(c in x$candidates) {
    expect_true(c$containment)
    if(grepl("ballpark",c$description,ignore.case=TRUE) || !c$instantiable ||
       any(!vapply(c$grids,`[[`,logical(1),"locally_verified"))) {
      expect_false(c$selectable)
      expect_true(nzchar(c$reason))
    }
  }
  expect_error(terrain_transform_candidates("EPSG:6344","EPSG:6344",c(1,2,1,3)),"bounds")
  expect_error(terrain_transform_candidates("EPSG:6344","EPSG:6344",c(-94,41,-90,43),source_epoch=NA_real_),"epochs")
})

test_that("real source references group tiles and identify the existing unit-only path", {
  fixture <- Sys.getenv("FLUVGEO_REAL_MOSAIC_INPUTS")
  skip_if(!nzchar(fixture),"Provide the retained real DEM seam windows")
  trial <- readRDS(fixture)
  area <- sf::st_as_sfc(sf::st_bbox(c(xmin=-91.1,ymin=41.5,xmax=-90.8,ymax=41.7),crs=4326))
  x <- review_terrain_transformations(trial$sources,"EPSG:6344","EPSG:8228",area)
  expect_length(x$groups,1L)
  expect_false(x$groups[[1L]]$requires_choice)
  expect_match(x$groups[[1L]]$explanation,"No datum change")
  expect_equal(nrow(x$sources),length(trial$sources))
  expect_identical(x$sources$bytes,unname(file.info(trial$sources)$size))
  projected <- review_terrain_transformations(trial$sources,"EPSG:6345","EPSG:8228",area)
  expect_false(projected$groups[[1L]]$requires_choice)
  expect_match(projected$groups[[1L]]$explanation,"datums match")
  changed <- review_terrain_transformations(trial$sources,"EPSG:26915","EPSG:8228",area)
  expect_true(changed$groups[[1L]]$requires_choice)
})

test_that("datum identity distinguishes realizations but ignores projection and units", {
  expect_true(.fg_same_terrain_datum("EPSG:26915","EPSG:26916"))
  expect_true(.fg_same_terrain_datum("EPSG:6344","EPSG:6345"))
  expect_false(.fg_same_terrain_datum("EPSG:26915","EPSG:6344"))
  expect_true(.fg_same_terrain_datum("EPSG:5703","EPSG:8228",vertical=TRUE))
  expect_false(.fg_same_terrain_datum("EPSG:5703","EPSG:5702",vertical=TRUE))
})

test_that("known epochs are retained but do not enable unqualified dynamic operations", {
  x <- terrain_transform_candidates("EPSG:7912","EPSG:7912",c(-94,41,-90,43),
    source_epoch=2010,target_epoch=2020)
  expect_identical(x$source$epoch,2010)
  expect_identical(x$target$epoch,2020)
  expect_gt(length(x$candidates),0L)
  expect_true(all(vapply(x$candidates,function(c) !c$selectable && grepl("Epoch-dependent",c$reason),logical(1))))
})
