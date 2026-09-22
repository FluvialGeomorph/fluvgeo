test_that("native raster masks preserve holes and parent NoData", {
  root <- withr::local_tempdir()
  square <- function(x1,y1,x2,y2) matrix(c(x1,y1,x2,y1,x2,y2,x1,y2,x1,y1),ncol=2,byrow=TRUE)
  area <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(square(0,0,4,4),square(1,1,3,3))),crs=26915))
  plan <- .fg_dem_grid_plan(area,26915,1)
  parent <- file.path(root,"parent.tif")
  .fg_mask_write(area,plan,sf::st_crs(26915)$wkt,parent)
  expect_equal(as.vector(terra::values(terra::rast(parent))),c(1,1,1,1,1,NA,NA,1,1,NA,NA,1,1,1,1,1))
  cropped <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(square(1,1,4,4))),crs=26915))
  cropped_plan <- .fg_dem_grid_plan(cropped,26915,1,plan$index)
  cropped_path <- file.path(root,"cropped.tif")
  .fg_mask_write(cropped,cropped_plan,sf::st_crs(26915)$wkt,cropped_path,parent)
  expect_equal(as.vector(terra::values(terra::rast(cropped_path))),c(1,1,1,NA,NA,1,NA,NA,1))
  expect_equal(.fg_mask_verify(cropped_path,cropped_plan,sf::st_crs(26915)$wkt,parent),5)
})

test_that("disk block boundaries retain every row at fractional spacing", {
  root <- withr::local_tempdir()
  area <- sf::st_sf(geometry=sf::st_as_sfc(sf::st_bbox(c(xmin=-150,ymin=-150,xmax=235.5,ymax=235.5),crs=26915)))
  plan <- .fg_dem_grid_plan(area,26915,1.5)
  path <- file.path(root,"blocks.tif");.fg_mask_write(area,plan,sf::st_crs(26915)$wkt,path)
  expect_equal(.fg_mask_verify(path,plan,sf::st_crs(26915)$wkt),257^2)
  expect_equal(terra::res(terra::rast(path)),c(1.5,1.5))
})

test_that("Stream and Reach masks reopen with hashes, aligned children and immutable prior editions", {
  root <- withr::local_tempdir(); args <- preflight_fixture(root);args$sources <- NULL
  originals <- tools::md5sum(c(args$context,args$selection,args$group))
  args$directory <- file.path(root,"first")
  m <- do.call(write_event_masks,args)
  expect_identical(m$recipe_key,do.call(event_mask_key,args[c("context","selection","group","stream_id")]))
  inputs <- do.call(.fg_event_grid_inputs,args[c("context","selection","group","stream_id")])
  renamed <- inputs;renamed$ctx$streams$stream_name <- "New label"
  renamed$settings$year <- 2021
  expect_identical(.fg_mask_recipe(inputs,args$stream_id),.fg_mask_recipe(renamed,args$stream_id))
  renamed$settings$cell_size <- 2
  expect_false(identical(.fg_mask_recipe(inputs,args$stream_id),.fg_mask_recipe(renamed,args$stream_id)))
  expect_equal(vapply(m$products,function(p) p$valid_cells,numeric(1)),c(121,25,4))
  reopened <- read_event_masks(args$directory)
  expect_equal(reopened$grid$anchor,c(0,0));expect_equal(reopened$grid$cell_size,1)
  expect_equal(length(reopened$products),3L)
  expect_identical(tools::md5sum(c(args$context,args$selection,args$group)),originals)
  before <- tools::md5sum(list.files(args$directory,full.names=TRUE))
  expect_error(do.call(write_event_masks,args),"new mask attempt")
  args$study_mask_source <- args$directory
  args$directory <- file.path(root,"second")
  write <- .fg_mask_write;writes <- 0L
  with_mocked_bindings(m2 <- do.call(write_event_masks,args),
    .fg_mask_write=function(...) {writes <<- writes+1L;write(...)})
  expect_equal(writes,2L)
  with_mocked_bindings(expect_length(read_event_masks(args$directory,verify=FALSE)$products,3L),
    .fg_mask_verify=function(...) stop("Unexpected pixel scan"),
    .fg_dem_hash=function(...) stop("Unexpected checksum scan"))
  expect_identical(lapply(m$products,function(p) p$plan),lapply(m2$products,function(p) p$plan))
  expect_identical(tools::md5sum(names(before)),before)
  manifest_path <- file.path(args$directory,"verified.json")
  altered <- jsonlite::read_json(manifest_path)
  altered$grid$cell_size <- 2
  jsonlite::write_json(altered,manifest_path,auto_unbox=TRUE,digits=NA,null="null")
  expect_error(read_event_masks(args$directory),"differs from the Event grid")
  altered$grid$cell_size <- 1;altered$products[[3]]$parent <- 1
  jsonlite::write_json(altered,manifest_path,auto_unbox=TRUE,digits=NA,null="null")
  expect_error(read_event_masks(args$directory),"hierarchy")
  altered$products[[3]]$parent <- 2
  jsonlite::write_json(altered,manifest_path,auto_unbox=TRUE,digits=NA,null="null")
  con <- file(file.path(args$directory,"mask-0002.tif"),"ab");writeBin(as.raw(0),con);close(con)
  expect_error(read_event_masks(args$directory),"checksum")
})

test_that("mask interruption cannot publish an incomplete masks", {
  root <- withr::local_tempdir();args <- preflight_fixture(root);args$sources <- NULL
  args$directory <- file.path(root,"budget")
  calls <- 0L
  with_mocked_bindings(expect_error(do.call(write_event_masks,args),"cancelled"),
    .fg_mask_checkpoint=function(...) {calls <<- calls+1L;if(calls>2L) stop("Mask creation cancelled.")})
  expect_false(file.exists(file.path(args$directory,"verified.json")))
  expect_error(read_event_masks(args$directory),"incomplete")
  mismatch <- args;mismatch$directory <- file.path(root,"mismatched-count")
  with_mocked_bindings(expect_error(do.call(write_event_masks,mismatch),"verification failed"),
    .fg_mask_verify=function(...) stop("verification failed"))
  expect_false(file.exists(file.path(mismatch$directory,"verified.json")))
  missing <- preflight_fixture(withr::local_tempdir(),reach_polygon=FALSE);missing$sources <- NULL
  missing$directory <- file.path(dirname(missing$context),"masks")
  expect_error(do.call(write_event_masks,missing),"Reach polygons")
  stale <- args;stale$directory <- file.path(root,"stale");stale$selection <- file.path(root,"new-selection.gpkg")
  file.copy(args$selection,stale$selection)
  expect_error(do.call(write_event_masks,stale),"changed")
})

test_that("Streams with no Reaches produce a two-level masks", {
  root <- withr::local_tempdir();args <- preflight_fixture(root,no_reaches=TRUE);args$sources <- NULL
  args$directory <- file.path(root,"masks")
  m <- do.call(write_event_masks,args)
  expect_equal(vapply(m$products,function(p) p$level,character(1)),c("Study Area","Stream"))
  expect_length(read_event_masks(args$directory)$products,2L)
})

test_that("wide grids have no arbitrary row-width cap", {
  root <- withr::local_tempdir()
  area <- sf::st_sf(geometry=sf::st_as_sfc(sf::st_bbox(c(xmin=0,ymin=0,xmax=70001,ymax=3),crs=26915)))
  plan <- .fg_dem_grid_plan(area,26915,1)
  path <- file.path(root,"wide.tif")
  .fg_mask_write(area,plan,sf::st_crs(26915)$wkt,path)
  expect_equal(.fg_mask_verify(path,plan,sf::st_crs(26915)$wkt),210003)
})
