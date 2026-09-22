#' Inspect one recorded source DEM without modifying it
#'
#' @param attempt Directory returned by prepare_stream_dem_download().
#' @param file_id One file ID in the immutable saved selection.
#' @param cache_dir Optional session-owned directory for reusable metadata.
#' @param refresh Recheck the checksum even when a matching cache entry exists.
#' @return List containing file_id, title, receipt SHA-256 and observation from
#'   inspect_terrain_vertical_reference(). Grid size is columns then rows;
#'   geotransform is the six GDAL affine coefficients in source CRS units.
#' @details Verifies receipt association, containment, size and the inspected
#'   file's checksum. No network, raster statistics, processing, acceptance or
#'   persistent inspection record. Metadata readability does not establish full
#'   pixel readability, coverage, ground resolution or scientific suitability.
#' @export
inspect_stream_dem_download <- function(attempt, file_id, cache_dir=NULL, refresh=FALSE) {
  file_id <- .fg_required_text(file_id, "file_id")
  if (length(file_id) != 1L) .fg_abort("Select exactly one source file.")
  a <- .fg_dem_request(attempt)
  status <- read_stream_dem_download(attempt, verify = FALSE)
  i <- match(file_id, status$files$file_id)
  if (is.na(i) || !identical(status$files$outcome[i], "RECORDED"))
    .fg_abort("Selected source has no valid local download receipt.")
  receipt <- jsonlite::read_json(.fg_dem_inside(file.path(attempt, "receipts",
    sprintf("%06d.json", i)), a$root), simplifyVector = TRUE)
  path <- .fg_dem_inside(file.path(a$root, status$files$asset[i]), a$root)
  stamp <- .fg_dem_view_stamp(path)
  key <- list(path=path,file_id=file_id,title=status$files$title[i],receipt=receipt$sha256,stamp=stamp,
    version=as.character(utils::packageVersion("fluvgeo")))
  cache <- NULL
  if(!is.null(cache_dir)) {
    dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
    cache <- file.path(cache_dir,paste0(as.character(openssl::sha256(serialize(key,NULL))),".rds"))
    saved <- if(!refresh && file.exists(cache)) tryCatch(readRDS(cache),error=function(e) NULL) else NULL
    if(!is.null(saved) && identical(saved$key,key)) return(saved$result)
  }
  observation <- inspect_terrain_vertical_reference(path)
  if (!identical(observation$sha256, receipt$sha256))
    .fg_abort("Source checksum differs from its download receipt.")
  if(!identical(stamp,.fg_dem_view_stamp(path))) .fg_abort("Source changed during inspection.")
  result <- list(file_id = file_id, title = status$files$title[i], sha256 = receipt$sha256,
    observation = observation)
  if(!is.null(cache)) saveRDS(list(key=key,result=result),cache)
  result
}

# Change indicators for managed viewing caches, not a replacement for SHA-256
# integrity verification at acquisition or an explicitly refreshed inspection.
.fg_dem_view_stamp <- function(path) {
  stem <- tools::file_path_sans_ext(path)
  paths <- c(path,paste0(path,c(".aux.xml",".ovr",".msk")),paste0(stem,c(".tfw",".tifw",".wld")))
  file.info(paths)[,c("size","mtime","ctime"),drop=FALSE]
}
