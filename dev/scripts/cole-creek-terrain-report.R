# Run from fluvgeo with a NEW output directory and optional successful probe directory.
# Read-only source inventory; local UUIDs below are explicitly provisional demo
# identities, not reconciled FGDB identities or approval of legacy observations.
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) > 2L || file.exists(args[1])) stop("Supply a new output directory and optional probe directory, --folder or --folder-structure.")
structure_mode <- length(args) == 2L && identical(args[2], "--folder-structure")
folder_mode <- length(args) == 2L && args[2] %in% c("--folder", "--folder-structure")
folder_manifest <- NULL
out <- args[1]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
years <- c(2006L, 2010L, 2016L)
gdbs <- paste0("../fluvgeodata/inst/extdata/y", years, "_R1.gdb")
structure_gdb <- "../fluvgeodata/inst/extdata/NWO_Papillion_ColeCreek_Stream.gdb"
source_files <- unlist(lapply(c(gdbs, if (structure_mode) structure_gdb),
  list.files, full.names = TRUE, recursive = TRUE))
source_hashes <- tools::md5sum(source_files)
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
study_area <- data.frame(study_area_id = streams$study_area_id, study_area_name = "Papillion Creek")
config_streams <- streams
if (structure_mode) {
  source("dev/scripts/cole-creek-study-structure.R", local = TRUE)
  structure <- cole_creek_study_structure(structure_gdb,
    sf::st_read(gdbs[1], layer = "flowline", quiet = TRUE), study_area$study_area_id, reaches$reach_id)
  study_area <- structure$study_area; streams <- structure$streams; reaches <- structure$reaches
  config_streams <- sf::st_drop_geometry(streams[structure$parent_index, ])
  utils::write.csv(structure$source_mapping, file.path(out, "stream-source-mapping.csv"), row.names = FALSE)
  jsonlite::write_json(structure$checks, file.path(out, "study-boundary-evidence.json"), pretty = TRUE, auto_unbox = TRUE)
}
events <- cbind(data.frame(survey_event_id = c("66666666-6666-4666-8666-666666666666",
  "77777777-7777-4777-8777-777777777777", "88888888-8888-4888-8888-888888888888"),
  reach_id = reaches$reach_id, survey_year = years), inventory)
config <- create_stream_network_configuration("22222222-2222-4222-8222-222222222222", study_area$study_area_id,
  "Cole Creek R1 retained example (provisional)", "STREAM", config_streams, actor = "terrain-report-demo")
obs <- create_stream_network_observation("55555555-5555-4555-8555-555555555555",
  config$stream_network_configuration$stream_network_configuration_id, observation_year = 2006L,
  evidence_class = "SOURCE_NETWORK_RETAINED", coverage_status = "PARTIAL_CONFIGURATION",
  derivation_method_id = "LEGACY_UNKNOWN", topology_tolerance = 0.01, topology_tolerance_unit = "METRE",
  native_horizontal_crs = "EPSG:26914", horizontal_unit = "METRE", provenance_completeness = "PARTIAL_LEGACY",
  actor = "terrain-report-demo")
raw <- sf::st_read(gdbs[1], layer = "stream_network", quiet = TRUE)
prepared <- prepare_stream_network_from_features(raw,
  data.frame(source_row = seq_len(nrow(raw)), stream_id = config_streams$stream_id, reach_id = reaches$reach_id),
  config$stream_network_configuration, config$stream_network_configuration_stream, obs,
  actor = "terrain-report-demo")
bundle <- c(config, list(stream_network_observation = obs), prepared)
gpkg <- file.path(out, "cole-creek-network-draft.gpkg")
write_stream_network_geodatabase(bundle, gpkg)
dem <- terra::rast(gdbs[1], subds = "dem_2006_ft_50")
dem_names <- c("dem_2006_ft_50", "dem_2010_ft_50", "dem_2016_hydro_50")
survey_dems <- setNames(lapply(seq_along(years), function(i) terra::rast(gdbs[i], subds = dem_names[i])), events$survey_event_id)
if (length(args) == 2L && !folder_mode) {
  conformance <- utils::read.csv(file.path(args[2], "conformance.csv"))
  stopifnot(nrow(conformance) == 51L, all(conformance$result == "PASS"))
  paths <- file.path(args[2], "intake", paste0("cole-creek-", years), paste0(dem_names, ".gpkg"))
  survey_dems <- setNames(lapply(paths, terra::rast), events$survey_event_id)
  events$source_dataset <- paste(events$source_dataset, "->", paths)
  dem <- survey_dems[[1]]
}
if (folder_mode) {
  dir.create(file.path(out, "rasters"))
  artifacts <- data.frame(artifact_id = "retained-network", path = basename(gpkg), role = "draft-network")
  event_links <- list()
  checks <- list()
  for (i in seq_along(years)) {
    datasets <- terra::sources(terra::sds(gdbs[i]))
    for (j in seq_along(datasets)) {
      source <- terra::rast(datasets[j])
      label <- paste0("cole-", years[i], "-raster-", j)
      relative <- paste0("rasters/", label, ".tif")
      target <- terra::writeRaster(source, file.path(out, relative),
        datatype = terra::datatype(source), gdal = "COMPRESS=DEFLATE")
      before <- terra::values(source); after <- terra::values(target)
      stopifnot(identical(is.na(before), is.na(after)),
        identical(before[!is.na(before)], after[!is.na(before)]),
        isTRUE(sf::st_crs(terra::crs(source)) == sf::st_crs(terra::crs(target))),
        identical(terra::res(source), terra::res(target)),
        isTRUE(all.equal(as.vector(terra::ext(source)), as.vector(terra::ext(target)), tolerance = 0)))
      artifacts <- rbind(artifacts, data.frame(artifact_id = label, path = relative,
        role = paste(basename(gdbs[i]), sub(".*:", "", datasets[j]), sep = " | ")))
      checks[[label]] <- list(source_gdb = basename(gdbs[i]), source_layer = sub(".*:", "", datasets[j]),
        path = relative, exact_values = TRUE, exact_mask = TRUE, grid_and_crs = TRUE)
      event_links[[label]] <- data.frame(artifact_id = label, survey_event_id = events$survey_event_id[i],
        purpose = paste('Retained terrain:', sub('.*:', '', datasets[j])),
        evidence = paste('Read from', basename(gdbs[i]), 'for the user-confirmed Cole Creek R1 event;',
          'exact copied values/grid verified. Demonstration event UUID is provisional.'),
        analyst = 'terrain-report-demo (supplied scope; provisional association)',
        use_for_report = grepl(paste0(':', dem_names[i], '$'), datasets[j]))
      if (grepl(paste0(":", dem_names[i], "$"), datasets[j])) survey_dems[[i]] <- target
    }
  }
  dem <- survey_dems[[1]]
  folder_manifest <- write_terrain_manifest(out, artifacts, "cole-creek-retained-2006-2010-2016",
    event_links = do.call(rbind, event_links))
  stopifnot(all(vapply(survey_dems, function(d) all(grepl("\\.tif$", terra::sources(d))), logical(1))))
  survey_dems <- NULL # Reopen event grids only through the saved explicit links.
  jsonlite::write_json(checks, file.path(out, "geotiff-copy-evidence.json"), pretty = TRUE, auto_unbox = TRUE)
}
reconstruction <- data.frame(case_id = c("cole-scope", "papillion-aoi", "terrain-vertical-reference"),
  source_ref = c("User-confirmed scope; flowline ReachName in all three GDBs",
    "Supplied Cole Creek archive fixtures", "DEM filenames and retained CRS metadata"),
  proposed_structure = c("Papillion Creek / Cole Creek / R1; Survey Events 2006, 2010, 2016", NA_character_, NA_character_),
  evidence = c("ReachName is Cole Creek R1; the user supplied the parent Study Area and Survey Event years.",
    "No analyst-defined Papillion Creek AOI has been supplied. Reach-scale DEM rectangles are not substitutes.",
    "Horizontal CRS can be read. A filename containing ft does not verify the elevation unit or vertical datum."),
  status = c("CONFIRMED", "UNKNOWN", "UNKNOWN"),
  analyst = c("User (scope confirmation in development discussion)", NA_character_, NA_character_),
  decision_notes = c("Use supplied scope in this demonstration; UUIDs remain provisional and unreconciled.", NA_character_, NA_character_))
if (structure_mode) {
  reconstruction <- reconstruction[reconstruction$case_id == "terrain-vertical-reference", ]
  reconstruction <- rbind(reconstruction, data.frame(
    case_id = c("papillion-stream-scope", "papillion-aoi", "cole-parent", "papillion-flowline-variants"),
    source_ref = c(rep("NWO_Papillion_ColeCreek_Stream.gdb / Papillion_HUC12; user clarification 2026-09-10", 2),
      "y2006_R1.gdb / flowline spatial containment in Papillion_HUC12",
      "NWO_Papillion_ColeCreek_Stream.gdb / Papillion_flowline"),
    proposed_structure = c("Seven HUC12-named Stream areas in NWO_Papillion",
      "Study Area = dissolved union of the seven selected Stream areas",
      paste("NWO_Papillion /", config_streams$stream_name, "/ Cole Creek R1; Survey Events 2006, 2010, 2016"), NA_character_),
    evidence = c("User confirms HUC12 names and segmentation were chosen for this project, not required generally.",
      "User directs merging the edge-matching HUC12 boundaries; the derived union is one valid polygon.",
      "The retained 2006 R1 flowline is entirely inside HUC12 102300060204 (Little Papillion Creek). Survey years and Cole Creek R1 labels were previously user-confirmed.",
      "100 line features have 38 distinct labels, repeated geometry and differing measure attributes. No variants were selected, dropped or repaired."),
    status = c("CONFIRMED", "CONFIRMED", "PROPOSED", "UNKNOWN"),
    analyst = c(rep("User (project interpretation in development discussion)", 2), NA_character_, NA_character_),
    decision_notes = c("Project convention only; future storage must explicitly identify hierarchy and rationale.",
      "Source polygons retained unchanged. Union computed in EPSG:26914 without snapping or repair.",
      "Spatially inferred parent used for this provisional demonstration, not reconciled FGDB identity.",
      "Keep the wider network and additional Reach definitions outside this adoption step.")))
  rownames(reconstruction) <- NULL
}
summary_args <- list(
  study_area = study_area,
  streams = streams, reaches = reaches, survey_events = events, survey_dems = survey_dems,
  reconstruction = reconstruction,
  folder_manifest = folder_manifest,
  network = gpkg, dem = if (folder_mode) NULL else dem,
  analyst_notes = paste("Papillion Creek Study Area / Cole Creek / Reach R1. Scope and Survey Event years confirmed by the user.",
    "PROVISIONAL DEMONSTRATION: UUIDs and the 0.01 m diagnostic tolerance are test scaffolding, not reconciled FGDB identities or analyst-approved processing parameters.",
    "The retained 2006 network is displayed without automated repair or acceptance. No Papillion Creek Study Area AOI, wider Stream inventory, or other Reach definitions were supplied.", sep = "\n\n"),
  terrain_notes = paste(if (folder_mode)
    "Displayed terrain grids are reopened GeoTIFF copies. All six retained rasters were checked for exact values/NoData and unchanged grids/CRS. The selected-file intake manifest adds fresh integrity checks, not complete event acceptance."
    else if (length(args) == 2L)
    "Displayed terrain grids are reopened GeoPackage probe copies. Selected flowline and raster value/CRS checks passed; this is not a complete archive migration or FGDB-ready dataset."
    else "Displayed terrain grids are read from original GDBs with terra/GDAL.",
    "The ft name is a source label, not verified vertical-reference metadata. Later retained DEMs are dem_2010_ft_50 and dem_2016_hydro_50; each file also retains a detrended raster.",
    "These Reach-scale products do not establish retention of the original Stream-scale extraction DEM. The 2006 network date follows its containing file and remains provisional derivation provenance."))
if (structure_mode) summary_args$analyst_notes <- paste(
  "NWO_Papillion: seven analyst-selected Stream areas use the supplied HUC12 names and boundaries. The Study Area is their dissolved union, as confirmed by the user. This is a project-specific convention, not a requirement to use HUC12 segmentation.",
  "Cole Creek R1 lies within the Little Papillion Creek Stream area. This parent association is spatially inferred for review. The retained survey inventory remains limited to Cole Creek R1 in 2006, 2010 and 2016; empty Stream branches do not imply that other Reaches or surveys never existed.",
  "The wider Papillion_flowline variants and five Cole Creek corridor polygons remain source evidence, not newly adopted network or Reach boundaries. Their interpretation and a standardized explicit hierarchy format remain future work.",
  "PROVISIONAL DEMONSTRATION: all UUIDs and the 0.01 m diagnostic tolerance are test scaffolding, not reconciled FGDB identities or analyst-approved processing parameters. The retained 2006 network is displayed without repair or acceptance.", sep = "\n\n")
summary <- do.call(terrain_development_summary, summary_args)
if (folder_mode) {
  saved_args <- summary_args
  saved_args$dem <- NULL; saved_args$survey_dems <- NULL
  saved_args$network <- basename(gpkg)
  saved_args$folder_manifest <- basename(folder_manifest)
  context <- do.call(write_study_context,
    c(list(dsn = file.path(out, "cole-creek-study.gpkg")), saved_args))
  reopened <- read_study_context_summary(context)
  for (field in c("study_area", "streams", "reaches", "surveys", "event_evidence",
                  "event_artifacts", "reconstruction", "assessment", "gaps")) {
    before <- summary[[field]]; after <- reopened[[field]]
    if (inherits(before, "sf")) {
      # Compare native coordinates and semantic CRS, not incidental WKT spelling.
      stopifnot(identical(sf::st_as_binary(sf::st_geometry(before)), sf::st_as_binary(sf::st_geometry(after))),
        isTRUE(sf::st_crs(before) == sf::st_crs(after)))
      before <- sf::st_drop_geometry(before); after <- sf::st_drop_geometry(after)
    }
    comparison <- all.equal(before, after)
    if (!isTRUE(comparison)) stop(field, ": ", paste(comparison, collapse = "; "))
  }
  summary <- reopened
}
html <- file.path(out, "cole-creek-terrain-development.html")
terrain_development_report(summary, html)
stopifnot(summary$observation$review_status == "DRAFT", nrow(summary$surveys) == 3L)
if (folder_mode) stopifnot(!any(summary$folder_inventory$assessment$status == "BLOCKED"),
  nrow(summary$event_artifacts) == 6L, sum(summary$event_artifacts$grid_status == 'GRID_LOADED') == 3L,
  all(summary$event_evidence$evidence_status == 'GRID_SUPPLIED'))
stopifnot(identical(tools::md5sum(source_files), source_hashes))
utils::write.csv(data.frame(source = names(source_hashes), md5 = unname(source_hashes)),
  file.path(out, "source-checksums.csv"), row.names = FALSE)
utils::write.csv(summary$reconstruction, file.path(out, "archive-interpretations.csv"), row.names = FALSE)
utils::write.csv(summary$assessment, file.path(out, "study-assessment.csv"), row.names = FALSE)
utils::write.csv(summary$review_actions, file.path(out, "review-actions.csv"), row.names = FALSE)
utils::write.csv(summary$review_action_members, file.path(out, "review-action-members.csv"), row.names = FALSE)
cat(normalizePath(html, winslash = "/"), "\n")
