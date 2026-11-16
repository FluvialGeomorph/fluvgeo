#' Prepare a viewport for close inspection of a DEM
#'
#' @param source Path to an elevation GeoTIFF.
#' @param bounds Named west, south, east, north bounds in longitude/latitude.
#' @param directory Existing job-owned output directory.
#' @param pixels Display cell budget, not an analytical processing limit.
#' @param cache_directory Persistent bounded display cache directory, or NULL for
#'   a job-local cache. Persistent caches reuse both source pyramids and recently
#'   prepared viewports without retaining raster values in R or browser memory.
#' @param build_cache Build missing full-raster display pyramids. If FALSE, reuse
#'   existing pyramids or prepare only the visible window without waiting for a cache.
#' @param source_id Optional immutable content identity supplied by a managed
#'   producer. It permits a cache prepared before an atomic file move to be reused
#'   from the published path.
#' @return Paths to Web Mercator elevation and hillshade display rasters, range,
#'   source resolution, and whether source cells were aggregated for display;
#'   empty views instead return empty=TRUE and an actionable message.
#' @export
prepare_hydro_dem_view <- function(source,bounds,directory,pixels=600000L,
                                   cache_directory=NULL,build_cache=TRUE,
                                   source_id=NULL) {
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
  cached <- .fg_hydro_display_cache(source,cache_directory,build=build_cache,
    source_id=source_id)
  e <- unname(as.vector(terra::ext(grid)))
  size <- ceiling(c(terra::ncol(grid),terra::nrow(grid))/factor)
  options <- c("-projwin",as.character(e[c(1,4,2,3)]),"-outsize",
    as.character(size),"-r","nearest")
  view <- if(is.null(cache_directory)) NULL else .fg_hydro_view_cache(
    source,cache_directory,e,size,!is.null(cached),source_id)
  if(!is.null(view)) return(view)
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
  result <- list(elevation=file.path(directory,"elevation.tif"),hill=file.path(directory,"hill.tif"),
    limits=limits,native=factor==1L,resolution=terra::res(r),unit=unit)
  if(!is.null(cache_directory)) result <- .fg_hydro_view_cache_publish(
    source,cache_directory,e,size,!is.null(cached),result,source_id=source_id)
  result
}

.fg_hydro_view_identity <- function(source,extent,size,pyramid,source_id=NULL) {
  info <- file.info(source)
  list(schema="HYDRO_DISPLAY_VIEW_1",
    source=if(is.null(source_id)) normalizePath(source,winslash="/") else as.character(source_id),
    bytes=info$size,modified=as.numeric(info$mtime),extent=extent,
    size=as.integer(size),pyramid=isTRUE(pyramid))
}

.fg_hydro_view_cache <- function(source,directory,extent,size,pyramid,source_id=NULL) {
  identity <- .fg_hydro_view_identity(source,extent,size,pyramid,source_id)
  key <- unclass(as.character(openssl::md5(serialize(identity,NULL))))
  path <- file.path(directory,"views",key)
  metadata <- suppressWarnings(tryCatch(readRDS(file.path(path,"result.rds")),
    error=function(e) NULL))
  files <- c(elevation=file.path(path,"elevation.tif"),hill=file.path(path,"hill.tif"))
  if(is.null(metadata) || !identical(metadata$identity,identity) || !all(file.exists(files)))
    return(NULL)
  try(Sys.setFileTime(file.path(path,"result.rds"),Sys.time()),silent=TRUE)
  c(as.list(files),metadata$result)
}

.fg_hydro_view_cache_publish <- function(source,directory,extent,size,pyramid,result,
                                         maximum=12L,source_id=NULL) {
  identity <- .fg_hydro_view_identity(source,extent,size,pyramid,source_id)
  key <- unclass(as.character(openssl::md5(serialize(identity,NULL))))
  root <- file.path(directory,"views");dir.create(root,recursive=TRUE,showWarnings=FALSE)
  path <- file.path(root,key)
  existing <- .fg_hydro_view_cache(source,directory,extent,size,pyramid,source_id)
  if(!is.null(existing)) return(existing)
  stage <- tempfile("view-",root);dir.create(stage)
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  if(!file.copy(result$elevation,file.path(stage,"elevation.tif")) ||
      !file.copy(result$hill,file.path(stage,"hill.tif")))
    return(result)
  metadata <- result[setdiff(names(result),c("elevation","hill"))]
  saveRDS(list(identity=identity,result=metadata),file.path(stage,"result.rds"))
  if(!dir.exists(path)) file.rename(stage,path)
  cached <- .fg_hydro_view_cache(source,directory,extent,size,pyramid,source_id)
  entries <- list.dirs(root,recursive=FALSE,full.names=TRUE)
  complete <- entries[file.exists(file.path(entries,"result.rds"))]
  if(length(complete)>maximum) {
    age <- file.info(file.path(complete,"result.rds"))$mtime
    remove <- complete[order(age)][seq_len(length(complete)-maximum)]
    unlink(remove,recursive=TRUE)
  }
  if(is.null(cached)) result else cached
}

.fg_hydro_display_cache <- function(source,directory,build=TRUE,source_id=NULL) {
  info <- file.info(source)
  identity <- list(schema="HYDRO_DISPLAY_PYRAMIDS_1",
    source=if(is.null(source_id)) normalizePath(source,winslash="/") else as.character(source_id),
    bytes=info$size,modified=as.numeric(info$mtime))
  key <- unclass(as.character(openssl::md5(serialize(identity,NULL))))
  path <- file.path(directory,key)
  result <- list(elevation=file.path(path,"elevation.tif"),hill=file.path(path,"hill.tif"))
  if(file.exists(file.path(path,"complete.rds")) && all(file.exists(unlist(result)))) return(result)
  # Reuse caches created before managed producers supplied content identities.
  # New builds still use the move-stable identity.
  if(!is.null(source_id)) {
    legacy <- .fg_hydro_display_cache(source,directory,build=FALSE,source_id=NULL)
    if(!is.null(legacy)) return(legacy)
  }
  if(!isTRUE(build)) return(NULL)
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  stage <- tempfile("building-",directory);dir.create(stage)
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  r <- terra::rast(source);z <- r
  if(terra::units(r)[1] %in% c("ft","foot","feet") &&
    sf::st_crs(terra::crs(r))$units_gdal %in% c("metre","meter","m")) z <- r*.3048
  terrain <- terra::terrain(z,v=c("slope","aspect"),unit="radians",filename=file.path(stage,"terrain.tif"))
  hill <- terra::shade(terrain$slope,terrain$aspect,normalize=TRUE,filename=file.path(stage,"raw-hill.tif"))
  cog <- c("-of","COG","-stats","-co","COMPRESS=DEFLATE","-co",
    "RESAMPLING=AVERAGE","-co","BIGTIFF=IF_SAFER")
  sf::gdal_utils("translate",source,file.path(stage,"elevation.tif"),options=cog)
  sf::gdal_utils("translate",terra::sources(hill),file.path(stage,"hill.tif"),options=cog)
  unlink(file.path(stage,c("terrain.tif","raw-hill.tif")))
  saveRDS(identity,file.path(stage,"complete.rds"))
  if(!dir.exists(path) && !file.rename(stage,path)) stop("Could not publish display pyramids.")
  result
}

#' Precompute a Hydro DEM display cache
#'
#' Builds full-resolution elevation and hillshade Cloud Optimized GeoTIFFs with
#' internal overviews and statistics. This is intended for a producer worker;
#' interactive maps should call [prepare_hydro_dem_view()] with `build_cache=FALSE`.
#'
#' @param source Path to an elevation GeoTIFF.
#' @param cache_directory Persistent display cache directory.
#' @param source_id Optional immutable content identity retained across an atomic move.
#' @return Paths to the prepared elevation and hillshade cache rasters.
#' @export
prepare_hydro_dem_display_cache <- function(source,cache_directory,source_id=NULL) {
  if(!file.exists(source)) stop("Hydro DEM source does not exist.")
  .fg_hydro_display_cache(source,cache_directory,build=TRUE,source_id=source_id)
}
