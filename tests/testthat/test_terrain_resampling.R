# Elevations come only from the retained, opt-in real DEM seam window.
test_that("real DEM mosaics resample to coarse, fine and shifted same-CRS grids", {
  fixture <- Sys.getenv("FLUVGEO_REAL_MOSAIC_INPUTS")
  skip_if(!nzchar(fixture), "Provide the existing real DEM seam fixture")
  trial <- readRDS(fixture)
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive = TRUE))
  before <- tools::md5sum(trial$sources)
  base <- mosaic_terrain_tiles(trial$sources, file.path(root,"source.tif"), "first")
  source <- terra::rast(base$path)
  domain <- terra::rast(readRDS(file.path(trial$root,"feet-result.rds"))$mask_file)
  e <- as.vector(terra::ext(source)) + c(24,-24,24,-24)
  for (spacing in c(0.5, 2, 3.3)) {
    grid <- terra::rast(xmin=e[1]+0.25, xmax=e[2]+0.25, ymin=e[3]+0.25,
      ymax=e[4]+0.25, resolution=spacing, crs=terra::crs(domain))
    terra::values(grid) <- 1
    template <- file.path(root,paste0("template-",spacing,".tif"))
    terra::writeRaster(grid,template)
    result <- mosaic_terrain_tiles(trial$sources,file.path(root,paste0("out-",spacing,".tif")),
      "first",template=template)
    out <- terra::rast(result$path)
    expect_true(terra::compareGeom(grid,out,crs=FALSE))
    expect_identical(terra::units(out),terra::units(source))
    expect_true(isTRUE(sf::st_crs(terra::crs(out))==sf::st_crs(terra::crs(source))))
    expect_identical(result$resampling,"bilinear")
    expect_identical(terra::datatype(out),"FLT4S")
    # A separate uncropped native resample detects lost interpolation support
    # at processing-window edges and at the original source-tile seam.
    terra::crs(grid) <- terra::crs(source)
    reference <- terra::resample(source,grid,method="bilinear")
    expect_identical(is.na(terra::values(out)),is.na(terra::values(reference)))
    expect_lte(max(abs(terra::values(out)-terra::values(reference)),na.rm=TRUE),5e-5)
    if(spacing==0.5) {
      # Independent four-neighbor bilinear calculation at 64 actual DEM cells.
      cells <- terra::cellFromRowCol(out,rep(round(seq(3,terra::nrow(out)-3,length.out=8)),8),
        rep(round(seq(3,terra::ncol(out)-3,length.out=8)),each=8))
      xy <- terra::xyFromCell(out,cells)
      origin <- c(terra::xmin(source),terra::ymin(source)) + terra::res(source)/2
      uv <- sweep(sweep(xy,2,origin,"-"),2,terra::res(source),"/")
      lower <- floor(uv); weights <- uv-lower
      value <- function(dx,dy) {
        loc <- sweep(sweep(lower,2,c(dx,dy),"+"),2,terra::res(source),"*")
        terra::extract(source,sweep(loc,2,origin,"+"))[[1]]
      }
      expected <- value(0,0)*(1-weights[,1])*(1-weights[,2]) +
        value(1,0)*weights[,1]*(1-weights[,2]) + value(0,1)*(1-weights[,1])*weights[,2] +
        value(1,1)*weights[,1]*weights[,2]
      valid <- is.finite(expected)
      expect_true(sum(valid)>40)
      expect_lte(max(abs(terra::extract(out,xy)[[1]][valid]-expected[valid])),5e-5)
    }
    expect_warning(masked <- mask_terrain_mosaic(result$path,template,
      file.path(root,paste0("masked-",spacing,".tif"))), "CRS do not match")
    feet <- terrain_to_international_feet(masked$path,file.path(root,paste0("feet-",spacing,".tif")))
    expect_lte(max(abs(terra::values(terra::rast(feet$path))-terra::values(out)/0.3048),
      na.rm=TRUE),0.0001)
    expect_error(mosaic_terrain_tiles(trial$sources,result$path,"first",template=template),"new output")
  }
  expect_identical(tools::md5sum(trial$sources),before)
  terra::crs(domain) <- "EPSG:6345"
  wrong <- file.path(root,"wrong-crs.tif"); terra::writeRaster(domain,wrong)
  expect_error(mosaic_terrain_tiles(trial$sources,file.path(root,"bad.tif"),"first",template=wrong),
    "same projected horizontal CRS")
  expect_false(file.exists(file.path(root,"bad.tif")))
})
