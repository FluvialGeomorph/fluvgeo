# Run from fluvgeo: Rscript dev/scripts/cole-creek-geopackage-probe.R <NEW directory>
# Limited migration evidence, NOT a general archive converter or accepted profile.
# Preserves original files. Tests one flowline and both retained rasters per event.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || file.exists(args[1])) stop("Supply one new output directory.")
out <- args[1]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
checks <- data.frame(source = character(), object = character(), check = character(), result = character(),
  max_abs_difference = double(), tolerance = double())
record <- function(source, object, check, ok, difference = NA_real_, tolerance = NA_real_) {
  checks <<- rbind(checks, data.frame(source = source, object = object, check = check,
    result = if (isTRUE(ok)) "PASS" else "FAIL", max_abs_difference = difference, tolerance = tolerance))
  utils::write.csv(checks, file.path(out, "conformance.csv"), row.names = FALSE)
  if (!isTRUE(ok)) stop(source, " / ", object, ": ", check, " failed. Candidate is not approved.")
}
versions <- data.frame(component = c("R", "sf", "terra", names(sf::sf_extSoftVersion())),
  version = c(as.character(getRversion()), as.character(utils::packageVersion("sf")),
    as.character(utils::packageVersion("terra")), unname(sf::sf_extSoftVersion())))
utils::write.csv(versions, file.path(out, "runtime.csv"), row.names = FALSE)
options(warn = 2) # A warning is not a qualified conversion.
creation_options <- data.frame(scope = c("vector", "vector", "raster", "raster", "raster", "raster"),
  option = c("driver", "CRS_WKT_EXTENSION", "source datatype", "TILE_FORMAT", "container", "adapter"),
  value = c("GPKG", "YES", "Float32; no output cast", "TIFF", "one new file per raster; no mixed append", "sf::gdal_utils translate via raster-only VRT"))
utils::write.csv(creation_options, file.path(out, "creation-options.csv"), row.names = FALSE)
for (year in c(2006L, 2010L, 2016L)) {
  source <- paste0("../fluvgeodata/inst/extdata/y", year, "_R1.gdb")
  source_files <- list.files(source, full.names = TRUE, recursive = TRUE)
  before <- tools::md5sum(source_files)
  # Intake case names are not governed Study Area/Stream/Reach/Event identities.
  case_dir <- file.path(out, "intake", paste0("cole-creek-", year))
  dir.create(case_dir, recursive = TRUE)
  dest <- file.path(case_dir, "source-copy.gpkg")
  flow <- sf::st_read(source, "flowline", quiet = TRUE)
  sf::st_write(flow, dest, "flowline", driver = "GPKG",
    dataset_options = "CRS_WKT_EXTENSION=YES", quiet = TRUE)
  back <- sf::st_read(dest, "flowline", quiet = TRUE)
  record(basename(source), "flowline", "CRS semantic equivalence", sf::st_crs(flow) == sf::st_crs(back))
  record(basename(source), "flowline", "exact geometry including coordinate order and dimensions",
    identical(sf::st_as_binary(sf::st_geometry(flow)), sf::st_as_binary(sf::st_geometry(back))))
  record(basename(source), "flowline", "attribute values and R types",
    identical(sf::st_drop_geometry(flow), sf::st_drop_geometry(back)))
  writeLines(sf::st_crs(flow)$wkt, file.path(case_dir, "source-flowline-crs.wkt"))
  writeLines(sf::st_crs(back)$wkt, file.path(case_dir, "reopened-flowline-crs.wkt"))
  raster_sources <- terra::sources(terra::sds(source))
  # terra::sds names repeat the GDB basename here; use the actual GDAL layer IDs.
  raster_names <- sub('.*":', '', raster_sources)
  stopifnot(!anyDuplicated(raster_names), all(grepl("^[A-Za-z][A-Za-z0-9_]*$", raster_names)))
  for (i in seq_along(raster_sources)) {
    dem <- terra::rast(raster_sources[i])
    name <- raster_names[i]
    record(basename(source), name, "source single-band Float32", terra::nlyr(dem) == 1L && terra::datatype(dem) == "FLT4S")
    original_values <- terra::values(dem, mat = FALSE)
    raster_dest <- file.path(case_dir, paste0(name, ".gpkg"))
    vrt <- file.path(case_dir, paste0(name, "-input.vrt"))
    terra::vrt(raster_sources[i], filename = vrt)
    sf::gdal_utils("translate", vrt, raster_dest, quiet = TRUE,
      options = c("-of", "GPKG", "-b", "1", "-co", paste0("RASTER_TABLE=", name), "-co", "TILE_FORMAT=TIFF"))
    unlink(vrt) # Only this generated scratch VRT, never an archive input.
    bare_copy <- tempfile(fileext = ".gpkg")
    stopifnot(file.copy(raster_dest, bare_copy))
    restored <- terra::rast(bare_copy) # Verify without any adjacent auxiliary file.
    record(basename(source), name, "CRS semantic equivalence", sf::st_crs(terra::crs(dem)) == sf::st_crs(terra::crs(restored)))
    delta <- max(abs(as.vector(terra::ext(dem)) - as.vector(terra::ext(restored))))
    tolerance <- 32 * .Machine$double.eps * max(1, abs(as.vector(terra::ext(dem))))
    record(basename(source), name, "grid dimensions/resolution exact; extent within 32 scaled machine epsilons",
      identical(dim(dem), dim(restored)) && delta <= tolerance &&
      identical(terra::res(dem), terra::res(restored)), delta, tolerance)
    record(basename(source), name, "reopened pixel type", terra::datatype(restored) == "FLT4S")
    restored_values <- terra::values(restored, mat = FALSE)
    record(basename(source), name, "NoData mask", identical(is.na(original_values), is.na(restored_values)))
    record(basename(source), name, "exact finite cell values",
      identical(original_values[!is.na(original_values)], restored_values[!is.na(restored_values)]))
    writeLines(terra::crs(dem), file.path(case_dir, paste0(name, "-source-crs.wkt")))
    writeLines(terra::crs(restored), file.path(case_dir, paste0(name, "-reopened-crs.wkt")))
    rm(restored)
    unlink(bare_copy)
  }
  record(basename(source), "intake", "selected vector and raster copies remain readable",
    "flowline" %in% sf::st_layers(dest)$name && all(vapply(raster_names,
      function(nm) terra::nlyr(terra::rast(file.path(case_dir, paste0(nm, ".gpkg")))) == 1L, logical(1))))
  record(basename(source), "archive", "source file inventory and checksums unchanged",
    identical(source_files, list.files(source, full.names = TRUE, recursive = TRUE)) &&
    identical(before, tools::md5sum(source_files)))
  utils::write.csv(data.frame(source_file = names(before), md5 = unname(before)),
    file.path(case_dir, "source-checksums.csv"), row.names = FALSE)
}
cat(nrow(checks), " checks passed. Limited candidates only; other legacy layers, domains, attachments,\n",
  "vertical reference truth, enterprise paths and other CRS/data types are not qualified.\n", sep = "")
