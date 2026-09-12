# Read-only investigation, not a source-association or discovery API.
# Run from the workspace root with the public-download directory as the argument.
# See FGDB/dev/architecture/wesm-archive-source-discovery.md for download/query context.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
gdb <- "fluvgeodata/inst/extdata/y2010_R1.gdb"
source_files <- list.files(gdb, full.names = TRUE, recursive = TRUE)
before <- tools::md5sum(source_files)
flowline <- sf::st_read(gdb, layer = "flowline", quiet = TRUE)
dem <- terra::rast(gdb, subds = "dem_2010_ft_50")
rectangle <- sf::st_as_sfc(sf::st_bbox(c(
  xmin = terra::xmin(dem), ymin = terra::ymin(dem),
  xmax = terra::xmax(dem), ymax = terra::ymax(dem)),
  crs = sf::st_crs(terra::crs(dem))))
compare <- function(path) {
  raw <- sf::st_read(path, quiet = TRUE)
  projected <- sf::st_transform(raw, sf::st_crs(flowline))
  # Diagnose a repaired in-memory copy; never overwrite or hide source validity.
  valid <- sf::st_make_valid(projected)
  list(
    file = basename(path),
    validity_before = sf::st_is_valid(projected, reason = TRUE),
    validity_after = sf::st_is_valid(valid, reason = TRUE),
    geographic_bbox = as.numeric(sf::st_bbox(sf::st_transform(raw, 4326))),
    flowline_covered_by_rows = lapply(sf::st_covered_by(flowline, valid), as.integer),
    dem_rectangle_covered_by_rows = lapply(sf::st_covered_by(rectangle, valid), as.integer),
    distance_from_flowline_m = as.numeric(sf::st_distance(flowline, valid))
  )
}
files <- c("papio-2010.geojson", "NE_PapioCo_footprint2.shp",
           "cole-creek-spatial-candidates.geojson")
checks <- lapply(file.path(args[1], files), compare)
candidates <- sf::st_drop_geometry(sf::st_read(
  file.path(args[1], files[3]), quiet = TRUE))
for (field in intersect(c("collect_start", "collect_end"), names(candidates))) {
  candidates[[field]] <- format(as.POSIXct(candidates[[field]] / 1000,
    origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d", tz = "UTC")
}
stopifnot(identical(before, tools::md5sum(source_files)))
cat(jsonlite::toJSON(list(
  scope = "Public footprints versus retained Cole Creek 2010; not confirmed lineage",
  source_files_unchanged = TRUE,
  dem_geographic_bbox = as.numeric(sf::st_bbox(sf::st_transform(rectangle, 4326))),
  footprint_checks = checks, spatial_candidates = candidates,
  libraries = as.list(sf::sf_extSoftVersion())
), auto_unbox = TRUE, pretty = TRUE, digits = 9), "\n")
