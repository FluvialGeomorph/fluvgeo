#' Preview a downloaded DEM in its source pixel grid
#'
#' @param attempt Download attempt directory.
#' @param file_id One saved source file ID.
#' @param max_dimension Maximum display rows or columns, integer from 2 to 512.
#' @param window Optional integer vector: zero-based column offset, row offset,
#'   width and height in source pixels. Must lie entirely inside the source.
#' @return The receipt-bound inspection with a preview list: values (row-major
#'   matrix, first row at top), source_size (columns, rows), sampled_size,
#'   window, native (no display downsampling), method and band_unit.
#'   Non-finite display values are NA.
#' @details Uses internal GeoTIFF georeferencing and nearest-neighbour sampling
#'   from the base raster, with existing overviews disabled. Band scale/offset are
#'   applied to Float64 display values. Temporary value/mask GeoTIFFs are removed
#'   on return. Embedded masks are used; external sidecars are excluded.
#'   Original files are unchanged and their checksum is checked again afterward.
#'   This is a whole-tile or source-window view, not an analytical derivative or proof of
#'   complete coverage, full pixel readability, or terrain suitability.
#' @export
preview_stream_dem_download <- function(attempt, file_id, max_dimension = 512L, window = NULL) {
  if (!is.numeric(max_dimension) || length(max_dimension)!=1L ||
      !is.finite(max_dimension) || max_dimension!=floor(max_dimension) ||
      max_dimension<2 || max_dimension>512) .fg_abort("max_dimension must be an integer from 2 to 512.")
  result <- inspect_stream_dem_download(attempt, file_id)
  source <- result$observation$path
  size <- result$observation$internal_compound$grid$size
  if (length(size)!=2L || any(!is.finite(size)) || any(size<1))
    .fg_abort("Source grid dimensions are unavailable.")
  if(is.null(window)) window <- c(0,0,size)
  if(!is.numeric(window) || length(window)!=4L || any(!is.finite(window)) ||
      any(window!=floor(window)) || any(window[1:2]<0) || any(window[3:4]<1) ||
      any(window[1:2]+window[3:4]>size))
    .fg_abort("window must contain integer offsets and positive dimensions inside the source grid.")
  window <- unname(window)
  output_size <- pmax(1L, as.integer(floor(window[3:4] * min(1, max_dimension/max(window[3:4])))))
  scratch <- tempfile("dem-preview-");dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  target <- file.path(scratch,"preview.tif")
  sf::gdal_utils("translate",source,target,options=c("-of","GTiff","-ot","Float64",
    "-oo","GEOREF_SOURCES=INTERNAL","-ovr","NONE","-r","nearest","-unscale","-a_nodata","nan",
    "-srcwin",as.character(window),"-outsize",as.character(output_size)),
    config_options=c(GDAL_PAM_ENABLED="NO",GTIFF_REPORT_COMPD_CS="TRUE",
      GDAL_DISABLE_READDIR_ON_OPEN="EMPTY_DIR"),quiet=TRUE)
  raster <- terra::rast(target)
  values <- matrix(terra::values(raster,mat=FALSE),nrow=output_size[2],ncol=output_size[1],byrow=TRUE)
  # Read the same sampled source mask explicitly: not every raster reader
  # applies internal mask bands, and scale/offset can change a NoData sentinel.
  mask_path <- file.path(scratch,"mask.tif")
  sf::gdal_utils("translate",source,mask_path,options=c("-of","GTiff","-ot","Byte",
    "-b","mask","-oo","GEOREF_SOURCES=INTERNAL","-ovr","NONE","-r","nearest",
    "-srcwin",as.character(window),"-outsize",as.character(output_size)),config_options=c(GDAL_PAM_ENABLED="NO",
      GDAL_DISABLE_READDIR_ON_OPEN="EMPTY_DIR"),quiet=TRUE)
  valid <- matrix(terra::values(terra::rast(mask_path),mat=FALSE),
    nrow=output_size[2],ncol=output_size[1],byrow=TRUE)
  values[is.na(valid) | valid==0] <- NA_real_
  values[!is.finite(values)] <- NA_real_
  if (!identical(.fg_file_sha256(source),result$sha256))
    .fg_abort("Source changed during preview.")
  result$preview <- list(values=values,source_size=size,sampled_size=output_size,window=window,
    native=all(output_size==window[3:4]),
    method="Nearest neighbour from base raster; band scale/offset applied",
    band_unit=result$observation$internal_compound$band_unit)
  result
}
