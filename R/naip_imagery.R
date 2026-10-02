.fg_naip_service <- paste0(
  "https://apps.geo.fpac.usda.gov/geo-imagery/rest/services/",
  "naip/conus_naip/ImageServer/exportImage"
)

.fg_naip_dimensions <- function(extent, max_cells = 4000000L) {
  if (length(max_cells) != 1L || !is.finite(max_cells) || max_cells < 1)
    stop("The NAIP image cell limit must be a positive number.", call. = FALSE)
  width <- unname(extent[["xmax"]] - extent[["xmin"]])
  height <- unname(extent[["ymax"]] - extent[["ymin"]])
  if (!is.finite(width) || !is.finite(height) || width <= 0 || height <= 0)
    stop("The imagery extent must have positive width and height.", call. = FALSE)

  ratio <- width / height
  columns <- floor(sqrt(max_cells * ratio))
  rows <- floor(sqrt(max_cells / ratio))
  columns <- max(1L, min(15000L, columns))
  rows <- max(1L, min(4100L, rows))

  # Reapply the aspect ratio after either service limit is reached.
  if (columns / rows > ratio) columns <- max(1L, floor(rows * ratio))
  if (columns / rows < ratio) rows <- max(1L, floor(columns / ratio))
  c(columns = as.integer(columns), rows = as.integer(rows))
}

.fg_naip_transfer <- function(request, path) {
  httr2::req_perform(httr2::req_timeout(request, 60), path = path)
}

.fg_naip_image <- function(location, transfer = .fg_naip_transfer) {
  projected <- sf::st_transform(location, sf::st_crs(3857))
  extent <- sf::st_bbox(projected)
  dimensions <- .fg_naip_dimensions(
    extent,
    max_cells = getOption("fluvgeo.naip.max_cells", 4000000L)
  )
  bbox <- paste(format(unname(extent), scientific = FALSE, trim = TRUE),
                collapse = ",")
  size <- paste(dimensions, collapse = ",")

  cache_dir <- file.path(tempdir(), "fluvgeo-naip")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_key <- paste(
    format(round(unname(extent), 2), scientific = FALSE, trim = TRUE),
    dimensions,
    sep = "_",
    collapse = "_"
  )
  target <- file.path(cache_dir, paste0(cache_key, ".tif"))

  if (!file.exists(target)) {
    request <- httr2::request(.fg_naip_service)
    request <- do.call(httr2::req_url_query, c(list(request), list(
      bbox = bbox,
      bboxSR = 3857,
      imageSR = 3857,
      size = size,
      format = "tiff",
      interpolation = "RSP_BilinearInterpolation",
      f = "image"
    )))
    stage <- tempfile("naip-", tmpdir = cache_dir, fileext = ".tif")
    on.exit(unlink(stage), add = TRUE)
    tryCatch(
      transfer(request, stage),
      error = function(error) stop(
        "Unable to retrieve USDA NAIP imagery: ", conditionMessage(error),
        call. = FALSE
      )
    )
    if (!file.exists(stage) || !is.finite(file.info(stage)$size) ||
        file.info(stage)$size <= 0)
      stop("USDA NAIP returned an empty imagery response.", call. = FALSE)
    if (!file.rename(stage, target) && !file.exists(target))
      stop("Unable to cache the USDA NAIP imagery response.", call. = FALSE)
  }

  imagery <- tryCatch(
    terra::rast(target),
    error = function(error) stop(
      "USDA NAIP returned an unreadable imagery response: ",
      conditionMessage(error), call. = FALSE
    )
  )
  if (terra::nlyr(imagery) < 3L)
    stop("USDA NAIP imagery does not contain RGB bands.", call. = FALSE)
  imagery <- imagery[[1:3]]
  names(imagery) <- c("red", "green", "blue")
  imagery
}
