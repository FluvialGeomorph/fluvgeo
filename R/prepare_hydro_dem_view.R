#' Prepare a viewport for close inspection of a DEM
#'
#' @param source Path to an elevation GeoTIFF.
#' @param bounds Named west, south, east, north bounds in longitude/latitude.
#' @param directory Existing job-owned output directory.
#' @param pixels Display cell budget, not an analytical processing limit.
#' @param cache_directory Persistent display cache directory, or NULL for a job-local cache.
#' @param build_cache Build missing full-raster display pyramids. If FALSE, reuse
#'   existing pyramids or prepare only the visible window without waiting for a cache.
#' @return Paths to Web Mercator elevation and hillshade display rasters, range,
#'   source resolution, and whether source cells were aggregated for display;
#'   empty views instead return empty=TRUE and an actionable message.
#' @export
prepare_hydro_dem_view <- function(source,bounds,directory,pixels=600000L,cache_directory=NULL,build_cache=TRUE) {
  r <- terra::rast(source)
  b <- unname(unlist(bounds[c("west","south","east","north")]))
  if(length(b)!=4L || any(!is.finite(b)) || b[1]>=b[3] || b[2]>=b[4]) stop("Invalid map bounds.")
  # Leaflet reports geographic degrees even though its tiles use Web Mercator.
  # Intersect in geographic space first: projecting a world-sized rectangle into
  # a local UTM zone can wrap/fold its edges and miss the entire DEM.
  extent <- unname(as.vector(terra::ext(r)))
  footprint <- sf::st_as_sfc(sf::st_bbox(c(xmin=extent[1],ymin=extent[3],
    xmax=extent[2],ymax=extent[4]),crs=sf::st_crs(terra::crs(r))))
  geographic <- unname(sf::st_bbox(sf::st_transform(footprint,4326)))
  if(b[3]-b[1]>=360) b[c(1,3)] <- geographic[c(1,3)] else {
    shift <- 360*round((mean(geographic[c(1,3)])-mean(b[c(1,3)]))/360)
    b[c(1,3)] <- b[c(1,3)]+shift
  }
  b <- c(max(b[1],geographic[1]),max(b[2],geographic[2]),
    min(b[3],geographic[3]),min(b[4],geographic[4]))
  empty <- list(empty=TRUE,message="No DEM cells in this view. Use Return to Stream or pan back to the channel.")
  if(b[1]>=b[3] || b[2]>=b[4]) return(empty)
  box <- sf::st_as_sfc(sf::st_bbox(c(xmin=b[1],ymin=b[2],xmax=b[3],ymax=b[4]),crs=4326))
  local <- sf::st_transform(box,terra::crs(r))
  grid <- terra::crop(terra::rast(r),terra::ext(terra::vect(local))+2*max(terra::res(r)),snap="out")
  factor <- max(1L,ceiling(sqrt(terra::ncell(grid)/pixels)))
  if(is.null(cache_directory)) cache_directory <- file.path(directory,"display-cache")
  cached <- .fg_hydro_display_cache(source,cache_directory,build=build_cache)
  e <- unname(as.vector(terra::ext(grid)))
  options <- c("-projwin",as.character(e[c(1,4,2,3)]),"-outsize",
    as.character(ceiling(c(terra::ncol(grid),terra::nrow(grid))/factor)),"-r","nearest")
  sf::gdal_utils("translate",if(is.null(cached)) source else cached$elevation,
    file.path(directory,"window.tif"),options=options)
  if(!is.null(cached)) sf::gdal_utils("translate",cached$hill,
    file.path(directory,"window-hill.tif"),options=options)
  clipped <- terra::rast(file.path(directory,"window.tif"))
  limits <- as.numeric(terra::global(clipped,c("min","max"),na.rm=TRUE)[1,])
  if(any(!is.finite(limits))) return(empty)
  # Source elevations are international feet in FG Studio; convert for slope
  # when the horizontal projected grid is in metres. Metadata determines units.
  unit <- terra::units(r)[1]
  if(is.null(cached)) {
    z <- clipped
    if(unit %in% c("ft","foot","feet") &&
       sf::st_crs(terra::crs(r))$units_gdal %in% c("metre","meter","m")) z <- clipped*.3048
    terrain <- terra::terrain(z,v=c("slope","aspect"),unit="radians",
      filename=file.path(directory,"window-terrain.tif"))
    hill <- terra::shade(terrain$slope,terrain$aspect,normalize=TRUE,
      filename=file.path(directory,"window-hill.tif"))
  } else hill <- terra::rast(file.path(directory,"window-hill.tif"))
  target <- terra::project(clipped,"EPSG:3857",method="near",
    filename=file.path(directory,"elevation.tif"),overwrite=TRUE)
  terra::project(hill,target,method="bilinear",filename=file.path(directory,"hill.tif"),overwrite=TRUE)
  list(elevation=file.path(directory,"elevation.tif"),hill=file.path(directory,"hill.tif"),
    limits=limits,native=factor==1L,resolution=terra::res(r),unit=unit)
}

.fg_hydro_display_cache <- function(source,directory,build=TRUE) {
  info <- file.info(source)
  identity <- list(schema="HYDRO_DISPLAY_PYRAMIDS_1",source=normalizePath(source,winslash="/"),
    bytes=info$size,modified=as.numeric(info$mtime))
  key <- unclass(as.character(openssl::md5(serialize(identity,NULL))))
  path <- file.path(directory,key)
  result <- list(elevation=file.path(path,"elevation.tif"),hill=file.path(path,"hill.tif"))
  if(file.exists(file.path(path,"complete.rds")) && all(file.exists(unlist(result)))) return(result)
  if(!isTRUE(build)) return(NULL)
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  stage <- tempfile("building-",directory);dir.create(stage)
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  r <- terra::rast(source);z <- r
  if(terra::units(r)[1] %in% c("ft","foot","feet") &&
    sf::st_crs(terra::crs(r))$units_gdal %in% c("metre","meter","m")) z <- r*.3048
  terrain <- terra::terrain(z,v=c("slope","aspect"),unit="radians",filename=file.path(stage,"terrain.tif"))
  hill <- terra::shade(terrain$slope,terrain$aspect,normalize=TRUE,filename=file.path(stage,"raw-hill.tif"))
  cog <- c("-of","COG","-co","COMPRESS=DEFLATE","-co","RESAMPLING=AVERAGE","-co","BIGTIFF=IF_SAFER")
  sf::gdal_utils("translate",source,file.path(stage,"elevation.tif"),options=cog)
  sf::gdal_utils("translate",terra::sources(hill),file.path(stage,"hill.tif"),options=cog)
  unlink(file.path(stage,c("terrain.tif","raw-hill.tif")))
  saveRDS(identity,file.path(stage,"complete.rds"))
  if(!dir.exists(path) && !file.rename(stage,path)) stop("Could not publish display pyramids.")
  result
}
