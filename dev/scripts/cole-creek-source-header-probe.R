# Read-only, offline source-header qualification; no production API or intake.
# From workspace root: Rscript this-file.R <sample-directory> <new-output.json>
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, dir.exists(args[1]), !file.exists(args[2]))
files <- file.path(args[1], c('douglas-2022-133500_110500.tif',
  'douglas-2022-135000_113500.tif', 'omaha_bare_earth_02.img'))
stopifnot(all(file.exists(files)))
before <- tools::md5sum(files)
flow <- sf::st_read('fluvgeodata/inst/extdata/y2010_R1.gdb',
  layer = 'flowline', quiet = TRUE)
inspect <- function(path) {
  info <- jsonlite::fromJSON(sf::gdal_utils('info', path,
    options = c('-json', '-norat'), quiet = TRUE), simplifyVector = FALSE)
  transform <- unlist(info$geoTransform)
  size <- unlist(info$size)
  stopifnot(transform[3] == 0, transform[5] == 0)
  extent <- sf::st_as_sfc(sf::st_bbox(c(xmin = transform[1],
    ymin = transform[4] + size[2] * transform[6],
    xmax = transform[1] + size[1] * transform[2], ymax = transform[4]),
    crs = sf::st_crs(info$coordinateSystem$wkt)))
  projected <- sf::st_transform(flow, sf::st_crs(extent))
  intersects <- any(lengths(sf::st_intersects(projected, extent)) > 0)
  sample <- NULL
  if (grepl('135000_113500', path, fixed = TRUE)) {
    stopifnot(intersects)
    part <- suppressWarnings(sf::st_intersection(sf::st_geometry(projected), extent))
    point <- sf::st_line_sample(part, n = 1, type = 'regular')
    point <- sf::st_cast(point, 'POINT')
    raster <- terra::rast(path)
    elevation <- terra::extract(raster, terra::vect(point))
    stopifnot(nrow(elevation) > 0, all(is.finite(elevation[[2]])))
    sample <- list(description = 'Midpoint(s) of clipped R1 line; readability only, not full-coverage or scientific QA',
      coordinates = unname(sf::st_coordinates(point)[, 1:2, drop = FALSE]),
      band_values = elevation[[2]])
  }
  list(file = basename(path), bytes = file.info(path)$size,
    driver = info$driverShortName, size = size,
    geotransform = transform, horizontal_wkt = info$coordinateSystem$wkt,
    band_type = info$bands[[1]]$type, band_unit = info$bands[[1]]$unit,
    nodata = info$bands[[1]]$noDataValue,
    metadata = info$metadata,
    dependencies = lapply(info$files, function(p) list(file = basename(p), present = file.exists(p))),
    intersects_R1 = intersects,
    contains_R1 = all(lengths(sf::st_covered_by(projected, extent)) > 0),
    read_sample = sample)
}
results <- lapply(files, inspect)
stopifnot(identical(before, tools::md5sum(files)))
stopifnot(!results[[1]]$intersects_R1, results[[2]]$intersects_R1,
  results[[3]]$contains_R1, results[[2]]$band_unit == 'foot')
jsonlite::write_json(list(scope = 'Read-only sample inspection, not source acceptance',
  inspected_at = format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'),
  software = as.list(sf::sf_extSoftVersion()), terra = as.character(packageVersion('terra')),
  samples_unchanged = TRUE, results = results), args[2],
  pretty = TRUE, auto_unbox = TRUE, digits = 15, null = 'null')
cat('Sample inspection passed; source files unchanged.\n')
