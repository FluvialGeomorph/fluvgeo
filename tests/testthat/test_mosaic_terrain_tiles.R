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
