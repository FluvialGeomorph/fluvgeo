test_that("real terrain on differing grids retains source priority and seam support", {
  fixture <- Sys.getenv("FLUVGEO_REAL_MOSAIC_INPUTS")
  skip_if(!nzchar(fixture),"Provide the existing real DEM seam windows")
  trial <- readRDS(fixture)
  root <- tempfile();dir.create(root);withr::defer(unlink(root,recursive=TRUE))
  base <- mosaic_terrain_tiles(trial$sources,file.path(root,"native.tif"),"first")
  native <- terra::rast(base$path)
  # Derived real-data inputs: no invented elevations or relabelled coordinates.
  coarse <- file.path(root,"coarse.tif")
  terra::aggregate(native,fact=2,fun="mean",filename=coarse,wopt=list(datatype="FLT4S"))
  e <- as.vector(terra::ext(native))
  shifted <- terra::rast(xmin=e[1]+0.25,xmax=e[2]+0.25,ymin=e[3]+0.25,ymax=e[4]+0.25,
    resolution=terra::res(native),crs=terra::crs(native))
  shifted_path <- file.path(root,"shifted.tif")
  terra::resample(native,shifted,method="bilinear",filename=shifted_path,wopt=list(datatype="FLT4S"))
  mask <- terra::rast(readRDS(file.path(trial$root,"feet-result.rds"))$mask_file)
  target <- terra::rast(xmin=e[1]-4,xmax=e[2]+4,ymin=e[3]-4,ymax=e[4]+4,
    resolution=1.3,crs=terra::crs(mask))
  terra::values(target) <- 1
  template <- file.path(root,"template.tif");terra::writeRaster(target,template)
  sampling_grid <- terra::rast(target);terra::crs(sampling_grid) <- terra::crs(native)
  before <- tools::md5sum(c(trial$sources,coarse,shifted_path,template))
  sample_surface <- function(path) as.numeric(terra::values(
    terra::resample(terra::rast(path),sampling_grid,method="bilinear")))
  combine <- function(a,b) {missing <- is.na(a);a[missing] <- b[missing];a}
  check <- function(paths,order,label,expected) {
    result <- mosaic_terrain_tiles(paths,file.path(root,paste0(label,".tif")),order,template=template)
    actual <- terra::rast(result$path);v <- as.numeric(terra::values(actual))
    expect_true(terra::compareGeom(actual,target,crs=FALSE))
    expect_identical(is.na(v),is.na(expected))
    expect_lte(max(abs(v-expected),na.rm=TRUE),5e-5)
    expect_true(isTRUE(sf::st_crs(terra::crs(actual))==sf::st_crs(terra::crs(native))))
    expect_identical(terra::units(actual),terra::units(native))
    expect_identical(terra::datatype(actual),"FLT4S")
    expect_true(result$mixed_source_grids)
    result
  }
  n <- sample_surface(base$path);c <- sample_surface(coarse)
  # Adjacent native tiles must be joined before interpolation across their seam.
  check(c(trial$sources,coarse),"first","native-first",combine(n,c))
  check(c(trial$sources,coarse),"last","coarse-first",combine(c,n))
  expect_true(any(abs(n-c)>1e-4,na.rm=TRUE))
  a <- sample_surface(trial$sources[1]);b <- sample_surface(trial$sources[2])
  expect_true(any(is.na(a) & is.finite(c))) # Lower-priority source supplies coverage.
  # Equal-grid sources separated by a different-priority grid cannot be reordered.
  check(c(trial$sources[1],coarse,trial$sources[2]),"first","interleaved-first",
    combine(combine(a,c),b))
  check(c(trial$sources[1],coarse,trial$sources[2]),"last","interleaved-last",
    combine(combine(b,c),a))
  check(c(trial$sources[1],shifted_path),"first","shifted-origin",
    combine(a,sample_surface(shifted_path)))
  # Inserting a different-grid source outside the target cannot break a seam.
  inner <- terra::crop(target,terra::ext(e+c(24,-24,24,-24)),snap="in")
  inner_path <- file.path(root,"inner-template.tif");terra::writeRaster(inner,inner_path)
  corner <- file.path(root,"corner.tif")
  terra::crop(terra::rast(coarse),terra::ext(c(e[1],e[1]+8,e[3],e[3]+8)),filename=corner)
  uninterrupted <- mosaic_terrain_tiles(trial$sources,file.path(root,"uninterrupted.tif"),
    "first",template=inner_path)
  extra <- mosaic_terrain_tiles(c(trial$sources[1],corner,trial$sources[2]),
    file.path(root,"with-corner.tif"),"first",template=inner_path)
  expect_identical(terra::values(terra::rast(extra$path)),
    terra::values(terra::rast(uninterrupted$path)))
  # An output grid entirely outside the inputs cannot publish a DEM.
  far_template <- terra::rast(xmin=e[2]+100,xmax=e[2]+110,ymin=e[3],ymax=e[3]+10,
    resolution=2,crs=terra::crs(mask));terra::values(far_template)<-1
  far <- file.path(root,"far.tif");terra::writeRaster(far_template,far)
  expect_error(mosaic_terrain_tiles(c(trial$sources[1],coarse),file.path(root,"absent.tif"),
    "first",template=far),"No source tile intersects")
  expect_false(file.exists(file.path(root,"absent.tif")))
  expect_error(mosaic_terrain_tiles(c(trial$sources[1],coarse),file.path(root,"missing-template.tif"),
    "first"),"output-grid template")
  feet <- terrain_to_international_feet(coarse,file.path(root,"feet.tif"))
  expect_error(mosaic_terrain_tiles(c(trial$sources[1],feet$path),file.path(root,"bad-reference.tif"),
    "first",template=template),"full CRS and elevation units")
  expect_identical(tools::md5sum(c(trial$sources,coarse,shifted_path,template)),before)
  expect_length(list.dirs(root,recursive=FALSE),0L)
})
