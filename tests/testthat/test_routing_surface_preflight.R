test_that("routing preflight discovers sparse row runs without changing its DEM", {
  path <- tempfile(fileext=".tif")
  values <- matrix(c(
    NA,1,2,NA,5,NA,
    NA,3,4,NA,6,7,
    NA,NA,8,9,NA,NA,
    10,NA,NA,11,12,NA
  ),nrow=4,byrow=TRUE)
  dem <- terra::rast(values,extent=terra::ext(0,6,0,4),crs="EPSG:26915")
  terra::writeRaster(dem,path,datatype="FLT4S")
  before <- tools::md5sum(path)

  p <- .fg_routing_surface_preflight(path,job_memory_mb=512,
    block_memory_mb=.0001,fixed_headroom_mb=1,foreground_reserve_mb=0)

  expect_equal(p$status,"PASS")
  expect_equal(p$dimensions,c(rows=4,columns=6,cells=24))
  expect_equal(p$valid_cells,12)
  expect_equal(p$valid_runs,7)
  expect_equal(p$maximum_runs_per_row,2)
  expect_equal(p$elevation_range,c(minimum=1,maximum=12))
  expect_equal(p$runs$row,c(1,1,2,2,3,4,4))
  expect_equal(p$runs$col_start,c(2,5,2,5,3,1,4))
  expect_equal(p$runs$col_end,c(3,5,3,6,4,1,5))
  expect_equal(p$runs$compact_start,c(1,3,4,6,8,10,11))
  expect_lt(p$memory$estimated_engine_mib,p$memory$estimated_peak_mib)
  expect_true(all(c("float64_accumulation","uint32_indegree",
    "uint32_work_queue","observed_worker_transient","fixed_worker_headroom",
    "foreground_session_reserve") %in%
    names(p$memory$parts_bytes)))
  expect_named(p$timings_seconds,c("hash_before","scan","hash_after","total"))
  expect_true(all(unlist(p$timings_seconds)>=0))
  expect_gte(p$timings_seconds$total,p$timings_seconds$scan)
  expect_identical(tools::md5sum(path),before)
  expect_false(p$processing_authorized)
})

test_that("routing preflight blocks work that exceeds its safe memory allowance", {
  path <- tempfile(fileext=".tif")
  dem <- terra::rast(nrows=10,ncols=10,xmin=0,xmax=10,ymin=0,ymax=10,
    crs="EPSG:26915",vals=seq_len(100))
  terra::writeRaster(dem,path,datatype="FLT4S")
  blocked <- .fg_routing_surface_preflight(path,job_memory_mb=1,
    safety_fraction=.5,block_memory_mb=.001,fixed_headroom_mb=1,
    foreground_reserve_mb=0)
  expect_equal(blocked$status,"BLOCKED")
  expect_match(blocked$refusal,"exceeds")
  expect_error(.fg_routing_surface_preflight(path,safety_fraction=1),"between")
})

test_that("deployment sizing separates foreground and routing-worker reserves", {
  estimate <- .fg_routing_memory_estimate(1956115,4701,2095090)
  dedicated <- .fg_routing_memory_estimate(1956115,4701,2095090,
    foreground_reserve_mb=0)

  expect_gt(estimate$total_mib,2048*.75)
  expect_lt(estimate$total_mib,3072*.75)
  expect_lt(dedicated$total_mib,1024*.75)
  expect_equal(estimate$bytes[["fixed_worker_headroom"]],416*1024^2)
  expect_equal(estimate$bytes[["foreground_session_reserve"]],960*1024^2)
})

test_that("priority flood can assign and resolve native D8 directions in one state", {
  source <- tempfile(fileext=".tif")
  output_dir <- tempfile("native-d8-")
  dir.create(output_dir)
  dem <- terra::rast(matrix(c(9,8,7,6,5,4,3,2,1),nrow=3,byrow=TRUE),
    extent=terra::ext(0,3,0,3),crs="EPSG:26915")
  terra::writeRaster(dem,source,datatype="FLT4S")

  result <- .fg_priority_flood_route(source,9,
    file.path(output_dir,"routing.tif"),file.path(output_dir,"fill.tif"),
    job_memory_mb=256,safety_fraction=.9,block_memory_mb=.001,
    fixed_headroom_mb=1,foreground_reserve_mb=0,
    direction_filename=file.path(output_dir,"direction.tif"),
    accumulation_filename=file.path(output_dir,"accumulation.tif"))

  direction <- terra::values(terra::rast(result$direction),mat=FALSE)
  accumulation <- terra::values(terra::rast(result$accumulation),mat=FALSE)
  expect_identical(result$schema,"PRIORITY_FLOOD_ROUTE_3")
  expect_equal(result$direction_evidence$directed_cells,8)
  expect_equal(result$flat_resolution$unresolved_cells,0)
  expect_equal(direction,c(4,4,4,4,4,4,1,1,0))
  expect_equal(accumulation,c(1,1,1,2,2,2,3,6,9))
  expect_equal(result$accumulation_evidence$maximum_accumulation,9)
  expect_true(all(unlist(result$timings_seconds)>=0))
})
