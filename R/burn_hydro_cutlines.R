#' Lower DEM cutline zones to their minimum elevation
#'
#' @param source Path to a single-band projected elevation GeoTIFF.
#' @param cutlines An sf object containing polylines with a declared CRS.
#' @param filename New output GeoTIFF path. Existing files are never overwritten.
#' @return Processing evidence, including output path, cutline zone minima,
#'   rasterization and reference definitions. The source raster is unchanged.
#' @export
burn_hydro_cutlines <- function(source,cutlines,filename) {
  r <- terra::rast(source)
  if(terra::nlyr(r)!=1L || terra::is.lonlat(r)) stop("Use a single-band projected Stream DEM.")
  if(!inherits(cutlines,"sf") || !nrow(cutlines) || is.na(sf::st_crs(cutlines)) ||
     any(!sf::st_geometry_type(cutlines) %in% c("LINESTRING","MULTILINESTRING")) ||
     any(sf::st_is_empty(cutlines)) || any(!sf::st_is_valid(cutlines)))
    stop("Draw valid cutlines before applying hydro modification.")
  if(file.exists(filename)) stop("Hydro DEM output already exists.")
  source_hash <- .fg_file_sha256(source)
  scratch <- tempfile("hydro-burn-",dirname(filename));dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  # WGS84 browser drawings are horizontally projected to the existing DEM grid.
  # Elevations never enter this coordinate operation; raster datums/units stay put.
  operation <- NULL
  if(isTRUE(sf::st_crs(cutlines)==sf::st_crs(terra::crs(r)))) lines <- cutlines else {
    network <- sf::sf_proj_network();on.exit(sf::sf_proj_network(network),add=TRUE)
    sf::sf_proj_network(FALSE)
    # sf custom pipelines honor source-axis order. CRS84 explicitly describes
    # Leaflet's longitude/latitude XY order, unlike EPSG:4326's authority axes.
    display_lines <- if(isTRUE(sf::st_crs(cutlines)$epsg==4326L))
      sf::st_transform(cutlines,"OGC:CRS84") else cutlines
    candidates <- sf::sf_proj_pipelines(sf::st_crs(display_lines),sf::st_crs(terra::crs(r)),
      axis_order_authority_compliant=FALSE)
    available <- which(candidates$instantiable)
    if(!length(available)) stop("No local operation can map these drawn cutlines to the DEM grid.")
    i <- available[1]
    operation <- as.list(as.data.frame(candidates)[i,,drop=FALSE])
    operation$source_wkt <- sf::st_crs(display_lines)$wkt
    operation$purpose <- "Map-digitized XY placement; no DEM datum or elevation transformation"
    operation$grids <- lapply(attr(candidates,"grids")[[i]],function(g) {
      paths <- unique(c(g$out_full_name,file.path(sf::sf_proj_search_paths(),g$out_short_name)))
      paths <- paths[file.exists(paths) & !dir.exists(paths)]
      g$sha256 <- if(length(paths)) .fg_file_sha256(paths[1]) else NULL;g
    })
    lines <- sf::st_transform(display_lines,terra::crs(r),pipeline=operation$definition)
  }
  lines$zone <- seq_len(nrow(lines))
  sampled <- terra::extract(r,terra::vect(lines),fun=min,na.rm=TRUE)
  skipped <- lines$zone[!is.finite(sampled[,2])]
  lines <- lines[!lines$zone %in% skipped,,drop=FALSE]
  if(!nrow(lines)) stop(paste0("Cutline(s) ",paste(skipped,collapse=", "),
    " cross only NoData or lie outside this Stream DEM. Its rectangular extent can include cells with no elevation."))
  # Only the cutline envelope needs zone calculations. Pad by one source cell
  # so lines on cell boundaries retain every touched cell; never resample.
  footprint <- terra::ext(terra::vect(lines))
  footprint <- terra::ext(as.vector(footprint) +
    rep(terra::res(r),each=2)*c(-1,1,-1,1))
  window <- terra::crop(r,footprint,snap="out",filename=file.path(scratch,"window.tif"))
  zones <- terra::rasterize(terra::vect(lines),window,field="zone",fun="min",touches=TRUE,background=0,
    filename=file.path(scratch,"zones.tif"),wopt=list(datatype="INT4S"))
  minima <- terra::zonal(window,zones,fun="min",na.rm=TRUE)
  minima <- minima[minima[,1]>0,,drop=FALSE]
  minima <- minima[is.finite(minima[,2]),,drop=FALSE]
  covered <- setdiff(lines$zone,minima[,1])
  if(!nrow(minima)) stop("No cutline cells have valid elevations in this Stream DEM.")
  lowered <- terra::subst(zones,from=minima[,1],to=minima[,2],
    filename=file.path(scratch,"minima.tif"),wopt=list(datatype="FLT4S"))
  # Preserve the source mask, including NoData inside a cutline zone.
  patch <- terra::ifel(is.na(window) | zones==0,NA,lowered,
    filename=file.path(scratch,"patch.tif"))
  # Both inputs share the exact grid. Use a file-backed merge; restore metadata
  # explicitly because terra's merge does not carry elevation units forward.
  out <- terra::merge(patch,r,first=TRUE,na.rm=TRUE,algo=1,
    filename=file.path(scratch,"merged.tif"),
    wopt=list(datatype="FLT4S",gdal="COMPRESS=NONE"))
  terra::crs(out) <- terra::crs(r);terra::units(out) <- terra::units(r)
  terra::writeRaster(out,filename,datatype="FLT4S",
    gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER"))
  reopened <- terra::rast(filename)
  if(!isTRUE(terra::compareGeom(r,reopened,stopOnError=FALSE))) stop("Hydro DEM grid changed unexpectedly.")
  list(path=filename,method="Minimum elevation within each rasterized cutline zone",
    source_sha256=source_hash,output_sha256=.fg_file_sha256(filename),
    rasterization="terra touched cells; lowest cutline order wins shared cells",
    widen_cells=0L,zone_minima=stats::setNames(as.list(minima[,2]),minima[,1]),
    skipped_nodata_cutlines=skipped,covered_cutlines=covered,
    cutline_input_crs=sf::st_crs(cutlines)$wkt,cutline_grid_crs=sf::st_crs(lines)$wkt,
    cutline_display_operation=operation,
    crs=terra::crs(r),units=terra::units(r),resolution=terra::res(r),
    dimensions=dim(r),datatype=terra::datatype(reopened),
    software=list(fluvgeo=as.character(utils::packageVersion("fluvgeo")),
      terra=as.character(utils::packageVersion("terra")),sf=as.character(utils::packageVersion("sf")),
      geospatial=as.list(sf::sf_extSoftVersion())),
    completed=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
}
