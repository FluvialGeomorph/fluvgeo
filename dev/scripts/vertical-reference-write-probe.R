# Development diagnostic only: synthetic custom vertical-unit serialization.
# Run from fluvgeo root with a NEW output-directory argument. No archive input.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, !file.exists(args[1]))
dir.create(args[1], recursive = TRUE)
pkgload::load_all(".", quiet = TRUE)
r <- terra::rast(nrows = 2, ncols = 2, xmin = 500000, xmax = 500002,
  ymin = 4500000, ymax = 4500002, crs = "EPSG:26914")
terra::values(r) <- c(0, -1, .125, NA)
source <- file.path(args[1], "synthetic-base.tif")
destination <- file.path(args[1], "synthetic-custom-vertical.tif")
terra::writeRaster(r, source, datatype = "FLT4S")
requested <- paste0('COMPOUNDCRS["Synthetic international feet",', sf::st_crs(26914)$wkt,
  ',VERTCRS["NAVD88 international feet synthetic",',
  'VDATUM["North American Vertical Datum 1988",ID["EPSG",5103]],CS[vertical,1],',
  'AXIS["height",up,LENGTHUNIT["foot",0.3048]]]]')
sf::gdal_utils("translate", source, destination,
  options = c("-a_srs", requested, "-co", "GEOTIFF_VERSION=1.0",
              "-co", "GEOTIFF_KEYS_FLAVOR=ESRI_PE"),
  config_options = c(GTIFF_REPORT_COMPD_CS = "TRUE"), quiet = TRUE)
observed <- inspect_terrain_vertical_reference(destination)
evidence <- list(requested_wkt = sf::st_crs(requested)$wkt,
  intended_vertical_metres_per_unit = .3048, observed = observed,
  scope = "Synthetic serialization probe; assigns metadata, does not transform elevations.")
jsonlite::write_json(evidence, file.path(args[1], "write-read-observation.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
print(observed$internal_compound$vertical_crs$coordinate_system$axis[[1]]$unit)
