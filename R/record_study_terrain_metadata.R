#' Record evidenced vertical metadata for associated terrain
#'
#' Fills previously unknown elevation-unit and/or vertical-reference assertions.
#' Saves new manifest/context snapshots without editing rasters, their CRS,
#' observed metadata, fingerprints or event associations. Assertions are not
#' independent verification, unit conversion or scientific acceptance.
#'
#' @param dsn Existing saved Study Area context GeoPackage.
#' @param output_file New context .gpkg beside dsn.
#' @param survey_event_id Exact event UUID with a selected terrain file.
#' @param vertical_unit Optional elevation-unit assertion. NULL or blank preserves
#'   the current value; never inferred from horizontal CRS or filename.
#' @param vertical_reference Optional vertical datum/reference assertion. NULL or
#'   blank preserves the current value. At least one new field is required.
#' @param evidence Nonempty source/basis for the supplied metadata.
#' @param analyst Nonempty attribution, not an approval signature.
#' @param manifest_file New JSON beside the existing manifest.
#' @param report_file Optional new HTML report.
#' @return List of context, report and manifest paths. Metadata belongs to the
#'   artifact and applies to every event sharing it. Existing known values cannot
#'   be replaced by this initial-entry tool. Partial knowledge remains explicit.
#'   Prior evidence is retained with appended attribution. Missing/changed or
#'   conflicted selected files are refused; unrelated findings remain visible.
#'   Outputs are not a single transaction: later failures retain already published
#'   manifest/context files for inspection. Never modify inputs concurrently.
#' @export
record_study_terrain_metadata <- function(dsn, output_file, survey_event_id,
    vertical_unit = NULL, vertical_reference = NULL, evidence, analyst,
    manifest_file, report_file = NULL) {
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  if (dirname(dsn) != dirname(output_file)) .fg_abort("Save the revised context beside the original.")
  if (file.exists(output_file)) .fg_abort("Context destination already exists.")
  if (!is.null(report_file)) {
    report_file <- .fg_required_text(report_file,"report_file")
    if (!grepl("\\.html$",report_file,ignore.case=TRUE) || !dir.exists(dirname(report_file)))
      .fg_abort("Supply a new .html report path in an existing directory.")
    if (file.exists(report_file)) .fg_abort("Report destination already exists.")
  }
  id <- .fg_required_text(survey_event_id,"survey_event_id")
  evidence <- .fg_required_text(evidence,"evidence")
  analyst <- .fg_required_text(analyst,"analyst")
  optional <- function(x, label) {
    if (is.null(x) || (is.character(x) && length(x)==1L && !is.na(x) && !nzchar(trimws(x)))) return(NULL)
    .fg_required_text(x,label)
  }
  supplied <- list(vertical_unit=optional(vertical_unit,"vertical_unit"),
    vertical_reference=optional(vertical_reference,"vertical_reference"))
  if (all(vapply(supplied,is.null,logical(1)))) .fg_abort("Supply at least one new vertical metadata field; unknown values need not be invented.")
  context_hash <- .fg_file_sha256(dsn)
  args <- read_study_context(dsn)
  if (is.null(args$survey_events) || !id %in% args$survey_events$survey_event_id)
    .fg_abort("Select an exact existing Survey Event ID.")
  if (is.null(args$folder_manifest)) .fg_abort("Associate terrain before recording its metadata.")
  old_path <- args$folder_manifest; old_hash <- .fg_file_sha256(old_path)
  manifest_file <- .fg_required_text(manifest_file,"manifest_file")
  root <- .fg_manifest_root(dirname(manifest_file)); filename <- basename(manifest_file)
  if (!identical(root,dirname(old_path))) .fg_abort("Save the new manifest beside the existing manifest.")
  if (!grepl("^[^:/\\\\]+\\.json$",filename)) .fg_abort("Supply a new JSON manifest filename.")
  manifest_file <- .fg_manifest_path(root,filename)
  if (file.exists(manifest_file)) .fg_abort("Manifest destination already exists.")
  inspected <- inspect_terrain_folder(old_path)
  links <- inspected$event_links
  j <- if (is.null(links)) integer() else which(links$survey_event_id==id & links$use_for_report)
  if (length(j)!=1L) .fg_abort("The event must have one explicitly selected terrain file.")
  artifact_id <- links$artifact_id[j]
  blocked <- inspected$assessment$entity_id[inspected$assessment$status=="BLOCKED"]
  if (artifact_id %in% blocked) .fg_abort("Selected terrain has integrity or metadata conflicts; its fingerprint was not refreshed.")
  manifest <- jsonlite::read_json(old_path)
  i <- match(artifact_id,vapply(manifest$artifacts,`[[`,character(1),"artifact_id"))
  record <- manifest$artifacts[[i]]; changed <- FALSE
  for (field in names(supplied)) if (!is.null(supplied[[field]])) {
    if (!is.null(record[[field]]) && !identical(record[[field]],supplied[[field]]))
      .fg_abort("Previously recorded metadata cannot be replaced by this initial-entry tool; review the correction separately.")
    if (is.null(record[[field]])) { record[[field]] <- supplied[[field]]; changed <- TRUE }
  }
  if (!changed) .fg_abort("No new metadata supplied; existing assertions were preserved.")
  band_unit <- record$observed$band_unit
  if (!is.null(record$vertical_unit) && nzchar(band_unit) &&
      .fg_manifest_unit(record$vertical_unit)!=.fg_manifest_unit(band_unit))
    .fg_abort("Supplied elevation units conflict with the observed raster band unit.")
  stamp <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
  record$metadata_evidence <- paste(c(record$metadata_evidence,
    paste0("[",stamp,"] ",analyst,": ",evidence)),collapse="\n\n")
  manifest$artifacts[[i]] <- record
  manifest$created_at <- stamp
  manifest$software <- list(fluvgeo=as.character(utils::packageVersion("fluvgeo")),terra=as.character(utils::packageVersion("terra")),gdal=terra::gdal())
  if (!identical(context_hash,.fg_file_sha256(dsn)) || !identical(old_hash,.fg_file_sha256(old_path)) ||
      !identical(record$sha256,.fg_file_sha256(.fg_manifest_path(root,record$path))))
    .fg_abort("Inputs changed during metadata recording; retry against stable evidence.")
  manifest_file <- .fg_publish_terrain_manifest(manifest,root,filename)
  args$folder_manifest <- manifest_file
  result <- tryCatch(.fg_save_study_revision(args,dsn,output_file,report_file,"definition"),
    error=function(e) .fg_abort(paste("Manifest saved at",manifest_file,"but context/report publication failed:",conditionMessage(e),"Retain published outputs for inspection.")))
  c(result,list(manifest=manifest_file))
}
