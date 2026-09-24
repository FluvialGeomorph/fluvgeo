test_that("a saved real Reach mask preserves terrain inside and removes terrain outside", {
  fixture <- Sys.getenv("FLUVGEO_REAL_MASK_INPUTS")
  skip_if(!nzchar(fixture), "Set FLUVGEO_REAL_MASK_INPUTS to the real mask-inputs.rds")
  trial <- readRDS(fixture)
  inputs <- c(trial$unmasked_result$path, trial$mask_file)
  before <- tools::md5sum(inputs)
  filename <- tempfile(fileext = ".tif"); on.exit(unlink(filename))
  # Real DEM has a compound horizontal/NAVD88 CRS; membership mask is 2D.
  # The backend checks horizontal equivalence without removing either CRS.
  expect_warning(result <- mask_terrain_mosaic(inputs[1], inputs[2], filename), "CRS do not match")
  original <- terra::rast(inputs[1]); domain <- terra::rast(inputs[2]); out <- terra::rast(filename)
  expect_true(terra::compareGeom(original, out))
  expect_identical(terra::datatype(out), terra::datatype(original))
  rc <- expand.grid(row = round(seq(1, terra::nrow(out), length.out = 8)),
                    col = round(seq(1, terra::ncol(out), length.out = 8)))
  xy <- terra::xyFromCell(out, terra::cellFromRowCol(out, rc$row, rc$col))
  source_values <- terra::extract(original, xy)[[1]]
  allowed <- !is.na(terra::extract(domain, xy)[[1]])
  observed <- terra::extract(out, xy)[[1]]
  expect_true(any(allowed) && any(!allowed))
  expect_equal(observed[allowed], source_values[allowed], tolerance = 0)
  expect_true(all(is.na(observed[!allowed])))
  expect_identical(tools::md5sum(inputs), before)
  expect_error(mask_terrain_mosaic(inputs[1], inputs[2], filename), "new output")
  expect_identical(result$scientific_acceptance, FALSE)
})
