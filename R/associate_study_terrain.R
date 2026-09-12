#' Associate a local terrain file with a recorded Survey Event
#'
#' Adds an explicit event grid selection using the existing terrain inventory.
#' Original files, context, manifest records and associations are preserved.
#' Selection enables metadata review, not scientific acceptance or FGDB loading.
#'
#' @param dsn Existing Study Area context GeoPackage.
#' @param output_file New context .gpkg beside dsn.
#' @param survey_event_id Exact saved Survey Event UUID.
#' @param terrain_file Existing single-band GeoTIFF within the manifest folder
#'   or its descendants. No copying, export or reprojection is performed.
#' @param evidence Nonempty basis for the file-to-event association.
#' @param analyst Nonempty supplied attribution, not an approval signature.
#' @param manifest_file New JSON path within the context folder tree. When an
#'   inventory already exists, save beside that manifest to preserve asset paths.
#' @param report_file Optional new HTML report path.
#' @param report_purpose Report view: definition (default), terrain or staging.
#' @return List of context, report and manifest paths. Missing vertical metadata
#'   remains unknown. Existing file snapshots are never refreshed. A previously
#'   inventoried file can serve another event if its integrity checks pass.
#'   Existing event selections and duplicate associations are refused, not replaced.
#'   Outputs are not one transaction: failure after manifest publication retains
#'   it, and report failure can also retain context. Inspect retained outputs;
#'   use read-only reporting if the context was saved. Never mutate assets during
#'   this operation. No hierarchy, acquisition date or file equivalence is inferred.
#' @export
associate_study_terrain <- function(dsn, output_file, survey_event_id, terrain_file,
    evidence, analyst, manifest_file, report_file = NULL, report_purpose = "definition") {
  purpose <- .fg_choice(report_purpose, c("terrain", "definition", "staging"), "report_purpose")
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  if (dirname(dsn) != dirname(output_file)) .fg_abort("Save the revised context beside the original.")
  if (file.exists(output_file)) .fg_abort("Context destination already exists.")
  if (!is.null(report_file)) {
    report_file <- .fg_required_text(report_file, "report_file")
    if (!grepl("\\.html$", report_file, ignore.case = TRUE) || !dir.exists(dirname(report_file)))
      .fg_abort("Supply a new .html report path in an existing directory.")
    if (file.exists(report_file)) .fg_abort("Report destination already exists.")
  }
  id <- .fg_required_text(survey_event_id, "survey_event_id")
  evidence <- .fg_required_text(evidence, "evidence")
  analyst <- .fg_required_text(analyst, "analyst")
  manifest_file <- .fg_required_text(manifest_file, "manifest_file")
  root <- .fg_manifest_root(dirname(manifest_file))
  filename <- basename(manifest_file)
  if (!grepl("^[^:/\\\\]+\\.json$", filename)) .fg_abort("Supply a new JSON manifest filename.")
  manifest_file <- .fg_manifest_path(root, filename)
  if (file.exists(manifest_file)) .fg_abort("Manifest destination already exists.")
  inside <- function(path, parent) {
    if (.Platform$OS.type == "windows") { path <- tolower(path); parent <- tolower(parent) }
    path == parent || startsWith(path, paste0(parent, "/"))
  }
  if (!inside(root, dirname(dsn))) .fg_abort("The manifest must stay inside the context folder tree.")
  terrain_file <- .fg_required_text(terrain_file, "terrain_file")
  if (!file.exists(terrain_file) || dir.exists(terrain_file) || !grepl("\\.tiff?$", terrain_file, ignore.case = TRUE))
    .fg_abort("Select an existing single-band GeoTIFF file.")
  terrain_file <- normalizePath(terrain_file, winslash = "/", mustWork = TRUE)
  if (!inside(terrain_file, root)) .fg_abort("Place the GeoTIFF inside the manifest folder tree before associating it; no file was copied.")
  relative <- substring(terrain_file, nchar(root) + 2L)
  .fg_manifest_path(root, relative)
  args <- read_study_context(dsn)
  if (is.null(args$survey_events) || !id %in% args$survey_events$survey_event_id)
    .fg_abort("Select an exact existing Survey Event ID; no event was created.")
  old <- NULL; links <- NULL; reuse <- integer()
  if (!is.null(args$folder_manifest)) {
    if (!identical(dirname(args$folder_manifest), root))
      .fg_abort("Save the new manifest beside the existing manifest to retain relative asset paths.")
    inspected <- inspect_terrain_folder(args$folder_manifest)
    old <- jsonlite::read_json(args$folder_manifest, simplifyVector = FALSE)
    links <- inspected$event_links
    if (!is.null(links)) {
      if (any(!links$survey_event_id %in% args$survey_events$survey_event_id))
        .fg_abort("Existing manifest references events outside this context; resolve them before adding associations.")
      if (id %in% links$survey_event_id[links$use_for_report])
        .fg_abort("This event already has a selected terrain file; replacement requires separate review.")
    }
    reuse <- which(tolower(inspected$artifacts$path) == tolower(relative))
    if (length(reuse)) {
      artifact_id <- inspected$artifacts$artifact_id[reuse]
      if (artifact_id %in% inspected$assessment$entity_id[inspected$assessment$status == "BLOCKED"] ||
          !inspected$artifacts$hash_verified[reuse])
        .fg_abort("The inventoried terrain file has unresolved integrity or metadata conflicts; its fingerprint was not refreshed.")
    }
  }
  if (!length(reuse)) {
    artifact_id <- .fg_generate_uuid(1)
    scratch <- tempfile("terrain-entry-", tmpdir = root, fileext = ".json")
    on.exit(unlink(scratch), add = TRUE)
    write_terrain_manifest(root,
      data.frame(artifact_id = artifact_id, path = relative, role = "event-terrain"),
      if (is.null(old)) args$streams$study_area_id[1L] else old$intake_id,
      filename = basename(scratch))
    added <- jsonlite::read_json(scratch, simplifyVector = FALSE)
  }
  new_link <- data.frame(artifact_id = artifact_id, survey_event_id = id,
    purpose = "Terrain grid for event review", evidence = evidence, analyst = analyst,
    use_for_report = TRUE)
  links <- if (is.null(links)) new_link else rbind(links, new_link)
  manifest <- if (is.null(old)) added else old
  if (!is.null(old) && !length(reuse)) manifest$artifacts <- c(old$artifacts, added$artifacts)
  manifest$schema <- "FLUVGEO_TERRAIN_INTAKE_2"
  manifest$event_links <- .fg_terrain_event_links(links,
    vapply(manifest$artifacts, `[[`, character(1), "artifact_id"),
    vapply(manifest$artifacts, function(a) a$observed$format, character(1)))
  # Keep old artifact observations and hashes, even if unrelated files are now
  # missing/changed. Fresh inspection must continue to expose those findings.
  manifest$created_at <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  manifest$software <- list(fluvgeo = as.character(utils::packageVersion("fluvgeo")),
    terra = as.character(utils::packageVersion("terra")), gdal = terra::gdal())
  manifest_file <- .fg_publish_terrain_manifest(manifest, root, filename)
  args$folder_manifest <- manifest_file
  result <- tryCatch(.fg_save_study_revision(args, dsn, output_file, report_file, purpose),
    error = function(e) .fg_abort(paste("Manifest saved at", manifest_file,
      "but context/report publication failed:", conditionMessage(e),
      "Retain published outputs for inspection; no originals were replaced.")))
  c(result, list(manifest = manifest_file))
}
