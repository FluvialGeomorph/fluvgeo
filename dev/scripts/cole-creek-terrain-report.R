# Run from fluvgeo with one NEW output directory argument.
# Read-only source inventory; local UUIDs below are explicitly provisional demo
# identities, not reconciled FGDB identities or approval of legacy observations.
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || dir.exists(args[1])) stop("Supply one new output directory.")
out <- args[1]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
years <- c(2006L, 2010L, 2016L)
gdbs <- paste0("../fluvgeodata/inst/extdata/y", years, "_R1.gdb")
inventory <- lapply(seq_along(gdbs), function(i) {
  layers <- sf::st_layers(gdbs[i])
  flowline <- sf::st_read(gdbs[i], layer = "flowline", quiet = TRUE)
  stopifnot(all(flowline$ReachName == "Cole Creek R1"))
  rasters <- terra::sources(terra::sds(gdbs[i]))
  data.frame(source_dataset = basename(gdbs[i]),
    availability_notes = paste(nrow(flowline), "flowline;", length(layers$name), "vector layers;", length(rasters), "readable raster subdatasets."))
})
inventory <- do.call(rbind, inventory)
streams <- data.frame(stream_id = "33333333-3333-4333-8333-333333333333",
  study_area_id = "11111111-1111-4111-8111-111111111111", stream_name = "Cole Creek")
reaches <- data.frame(reach_id = "44444444-4444-4444-8444-444444444444", stream_id = streams$stream_id, reach_name = "R1")
events <- cbind(data.frame(survey_event_id = c("66666666-6666-4666-8666-666666666666",
  "77777777-7777-4777-8777-777777777777", "88888888-8888-4888-8888-888888888888"),
  reach_id = reaches$reach_id, survey_year = years), inventory)
config <- create_stream_network_configuration("22222222-2222-4222-8222-222222222222", streams$study_area_id,
  "Cole Creek R1 retained example (provisional)", "STREAM", streams, actor = "terrain-report-demo")
obs <- create_stream_network_observation("55555555-5555-4555-8555-555555555555",
  config$stream_network_configuration$stream_network_configuration_id, observation_year = 2006L,
  evidence_class = "SOURCE_NETWORK_RETAINED", coverage_status = "PARTIAL_CONFIGURATION",
  derivation_method_id = "LEGACY_UNKNOWN", topology_tolerance = 0.01, topology_tolerance_unit = "METRE",
  native_horizontal_crs = "EPSG:26914", horizontal_unit = "METRE", provenance_completeness = "PARTIAL_LEGACY",
  actor = "terrain-report-demo")
raw <- sf::st_read(gdbs[1], layer = "stream_network", quiet = TRUE)
prepared <- prepare_stream_network_from_features(raw,
  data.frame(source_row = seq_len(nrow(raw)), stream_id = streams$stream_id, reach_id = reaches$reach_id),
  config$stream_network_configuration, config$stream_network_configuration_stream, obs,
  actor = "terrain-report-demo")
bundle <- c(config, list(stream_network_observation = obs), prepared)
gpkg <- file.path(out, "cole-creek-network-draft.gpkg")
write_stream_network_geodatabase(bundle, gpkg)
dem <- terra::rast(gdbs[1], subds = "dem_2006_ft_50")
summary <- terrain_development_summary(streams = streams, reaches = reaches, survey_events = events,
  network = gpkg, dem = dem,
  analyst_notes = paste("Papillion Creek Study Area / Cole Creek / Reach R1. Scope and Survey Event years confirmed by the user.",
    "PROVISIONAL DEMONSTRATION: UUIDs and the 0.01 m diagnostic tolerance are test scaffolding, not reconciled FGDB identities or analyst-approved processing parameters.",
    "The retained 2006 network is displayed without automated repair or acceptance. No Papillion Creek Study Area AOI, wider Stream inventory, or other Reach definitions were supplied."),
  terrain_notes = paste("Displayed terrain extent: y2006_R1.gdb / dem_2006_ft_50, read with terra/GDAL.",
    "The ft name is a source label, not verified vertical-reference metadata. Later retained DEMs are dem_2010_ft_50 and dem_2016_hydro_50; each file also retains a detrended raster.",
    "These Reach-scale products do not establish retention of the original Stream-scale extraction DEM. The 2006 network date follows its containing file and remains provisional derivation provenance."))
html <- file.path(out, "cole-creek-terrain-development.html")
terrain_development_report(summary, html)
stopifnot(summary$observation$review_status == "DRAFT", nrow(summary$surveys) == 3L)
cat(normalizePath(html, winslash = "/"), "\n")
