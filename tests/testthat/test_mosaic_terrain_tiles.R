# Explicit opt-in: actual downloaded DEM windows prepared outside the package.
# No analyst files are modified and no synthetic elevation values are generated.
test_that("real DEM windows mosaic without changing sampled source elevations", {
  fixture <- Sys.getenv("FLUVGEO_REAL_MOSAIC_INPUTS")
  skip_if(!nzchar(fixture), "Set FLUVGEO_REAL_MOSAIC_INPUTS to the real-window inputs.rds")
  trial <- readRDS(fixture)
  paths <- trial$sources
  before <- tools::md5sum(paths)
  output <- tempfile(fileext = ".tif")
  on.exit(unlink(output))
  result <- mosaic_terrain_tiles(paths, output, "first")
  joined <- terra::rast(result$path)
  expect_equal(terra::res(joined), terra::res(terra::rast(paths[1])))
  expect_identical(terra::datatype(joined), "FLT4S")
  for (path in paths) {
    r <- terra::rast(path)
    # Small deterministic samples from real cells, including both seam edges.
    rows <- unique(c(1L, ceiling(terra::nrow(r)/2), terra::nrow(r)))
    cols <- unique(c(1L, ceiling(terra::ncol(r)/2), terra::ncol(r)))
    rc <- expand.grid(row = rows, col = cols)
    cells <- terra::cellFromRowCol(r, rc$row, rc$col)
    xy <- terra::xyFromCell(r, cells)
    expect_equal(unname(as.matrix(terra::extract(joined, xy))),
                 unname(as.matrix(terra::extract(r, xy))), tolerance = 0)
  }
  expect_identical(tools::md5sum(paths), before)
  expect_error(mosaic_terrain_tiles(paths, output, "first"), "new output")
  expect_error(mosaic_terrain_tiles(paths, tempfile(), "mean"), "arg")
  last <- tempfile(fileext = ".tif"); on.exit(unlink(last), add = TRUE)
  expect_identical(mosaic_terrain_tiles(paths, last, "last")$overlap, "last")
})

test_that("real saved source files can be assembled only within the requested window", {
  fixture <- Sys.getenv("FLUVGEO_REAL_MOSAIC_INPUTS")
  skip_if(!nzchar(fixture), "Provide the existing real DEM fixture")
  trial <- readRDS(fixture)
  reference <- readRDS(file.path(trial$root,"result.rds"))$result$path
  expected <- terra::rast(reference)
  extent <- as.vector(terra::ext(expected))
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  before <- file.info(trial$originals)[c("size","mtime")]
  result <- mosaic_terrain_tiles(trial$originals,file.path(root,"dem.tif"),"first",extent)
  actual <- terra::rast(result$path)
  expect_true(terra::compareGeom(expected,actual))
  expect_identical(terra::datatype(actual),"FLT4S")
  expect_identical(terra::units(actual),terra::units(expected))
  rc <- expand.grid(row=round(seq(1,terra::nrow(expected),length.out=8)),
                    col=round(seq(1,terra::ncol(expected),length.out=8)))
  xy <- terra::xyFromCell(expected,terra::cellFromRowCol(expected,rc$row,rc$col))
  expect_identical(terra::extract(actual,xy)[[1]],terra::extract(expected,xy)[[1]])
  expect_identical(file.info(trial$originals)[c("size","mtime")],before)
  expect_length(list.dirs(root,recursive=FALSE),0L)
  expect_error(mosaic_terrain_tiles(trial$originals,file.path(root,"bad.tif"),"first",c(1,0,1,2)),"extent")
  expect_error(mosaic_terrain_tiles(trial$originals,file.path(root,"away.tif"),"first",extent+1e7),"intersects")
  expect_false(file.exists(file.path(root,"away.tif")))
})
