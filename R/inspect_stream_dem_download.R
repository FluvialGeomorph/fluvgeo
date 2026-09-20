#' Inspect one recorded source DEM without modifying it
#'
#' @param attempt Directory returned by prepare_stream_dem_download().
#' @param file_id One file ID in the immutable saved selection.
#' @return List containing file_id, title, receipt SHA-256 and observation from
#'   inspect_terrain_vertical_reference(). Grid size is columns then rows;
#'   geotransform is the six GDAL affine coefficients in source CRS units.
#' @details Verifies receipt association, containment, size and the inspected
#'   file's checksum. No network, raster statistics, processing, acceptance or
#'   persistent inspection record. Metadata readability does not establish full
#'   pixel readability, coverage, ground resolution or scientific suitability.
#' @export
inspect_stream_dem_download <- function(attempt, file_id) {
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
  if (!.fg_dem_receipt_valid(receipt, a$root, verify = TRUE))
    .fg_abort("Source checksum differs from its download receipt.")
  observation <- inspect_terrain_vertical_reference(path)
  if (!identical(observation$sha256, receipt$sha256))
    .fg_abort("Source checksum differs from its download receipt.")
  list(file_id = file_id, title = status$files$title[i], sha256 = receipt$sha256,
    observation = observation)
}
