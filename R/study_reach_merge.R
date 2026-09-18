#' Preview combining saved Reaches under one retained identity
#' @param dsn Existing context GeoPackage.
#' @param reach_ids Two or more distinct saved Reach identities in one Stream.
#' @param retain_reach_id One selected Reach identity to retain.
#' @return Combined inherited-buffer preview with retained/retired IDs and Survey
#'   Event reassignment count. Requires intact segment evidence. Linked networks
#'   or terrain manifests need separate reconciliation and are refused. No writes.
#' @export
preview_study_reach_merge <- function(dsn, reach_ids, retain_reach_id) {
  args <- read_study_context(dsn)
  old <- args$reaches
  if (!inherits(old, "sf") || !is.character(reach_ids) || length(reach_ids) < 2L ||
      anyNA(reach_ids) || anyDuplicated(reach_ids) || !all(reach_ids %in% old$reach_id))
    .fg_abort("Choose at least two distinct saved Reaches.")
  keep <- .fg_required_text(retain_reach_id, "retain_reach_id")
  if (!keep %in% reach_ids) .fg_abort("Choose which selected Reach identity to retain.")
  chosen <- old[match(reach_ids, old$reach_id), ]
  parents <- unique(chosen$stream_id)
  if (length(parents) != 1L) .fg_abort("Combine Reaches within one parent Stream only.")
  if (!is.null(args$network) || !is.null(args$folder_manifest))
    .fg_abort("Linked network or terrain-manifest references require separate reconciliation before merging Reaches.")
  source <- read_study_stream_segments(dsn, parents)
  if (isTRUE(source$piece_state)) {
    ids <- source$pieces$piece_id[source$pieces$fg_reach_id %in% reach_ids]
    if (!all(reach_ids %in% source$pieces$fg_reach_id)) .fg_abort("Reach pieces are missing.")
    view <- .fg_reach_preview(source,ids)
    retired <- setdiff(reach_ids,keep)
    return(c(view,list(retain_reach_id=keep,retired_reach_ids=retired,reach_ids=reach_ids,
      stream_id=parents,reach_name=old$reach_name[match(keep,old$reach_id)],
      reassigned_events=if (is.null(args$survey_events)) 0L else sum(args$survey_events$reach_id %in% retired))))
  }
  mappings <- source$reach_mappings
  if (!all(reach_ids %in% mappings$reach_id))
    .fg_abort("Every selected Reach needs retained segment evidence before merging.")
  # An edited Reach polygon may no longer represent its retained source lines.
  # Verify both evidence integrity and its agreement with the current record.
  for (rid in reach_ids) {
    pattern <- "(reach-selection-[0-9a-f-]+\\.gpkg) / retained_line and reach_area; SHA256 ([0-9a-f]{64})\\."
    hit <- regmatches(source$mapping_notes[[rid]], regexec(pattern, source$mapping_notes[[rid]], perl = TRUE))[[1]]
    if (length(hit) != 3L) .fg_abort("Reach evidence link is missing or malformed.")
    path <- file.path(dirname(dsn), hit[2])
    if (!file.exists(path) || .fg_file_sha256(path) != hit[3])
      .fg_abort("Reach evidence is missing or changed; merge is blocked.")
    area <- sf::st_read(path, layer = "reach_area", quiet = TRUE)
    line <- sf::st_read(path, layer = "retained_line", quiet = TRUE)
    record <- chosen[chosen$reach_id == rid, ]
    ids <- mappings$source_id[mappings$reach_id == rid]
    if (!inherits(area, "sf") || nrow(area) != 1L || !identical(area$reach_id, rid) ||
        !inherits(line, "sf") || !setequal(line$source_id, ids) ||
        !all(line$fg_reach_id == rid) || !isTRUE(sf::st_crs(area) == sf::st_crs(record)) ||
        !isTRUE(all.equal(sf::st_coordinates(area), sf::st_coordinates(record))))
      .fg_abort("Reach evidence no longer agrees with its current geometry or identity; reconcile it before merging.")
  }
  ids <- mappings$source_id[mappings$reach_id %in% reach_ids]
  if (anyDuplicated(ids) || !all(ids %in% source$lines$source_id))
    .fg_abort("Selected Reach source assignments are overlapping or unresolved.")
  view <- .fg_reach_preview(source, ids)
  retired <- setdiff(reach_ids, keep)
  c(view, list(retain_reach_id = keep, retired_reach_ids = retired, reach_ids = reach_ids,
    stream_id = parents, reach_name = old$reach_name[match(keep, old$reach_id)],
    reassigned_events = if (is.null(args$survey_events)) 0L else sum(args$survey_events$reach_id %in% retired)))
}

#' Combine saved Reaches in a new revision
#' @param dsn Existing context GeoPackage.
#' @param output_file New context GeoPackage beside dsn.
#' @param reach_ids Two or more saved Reach identities from one Stream.
#' @param retain_reach_id Selected identity to retain; other selected identities
#'   leave the current inventory but remain in earlier immutable revisions.
#' @param reach_name Name for the combined Reach, unique in its parent Stream.
#' @param report_purpose Revision purpose, normally definition.
#' @return Revision paths, evidence and retained Reach ID. Survey Events retain
#'   their identities and attributes; only retired parent IDs are reassigned.
#'   Source files and prior revisions are untouched. No linked network/manifest
#'   rewrite is attempted. Evidence-first publication can leave orphan evidence.
#' @export
merge_study_reaches <- function(dsn, output_file, reach_ids, retain_reach_id,
                                reach_name, report_purpose = "definition") {
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  purpose <- .fg_choice(report_purpose, c("definition", "terrain", "staging"), "report_purpose")
  if (dirname(dsn) != dirname(output_file) || file.exists(output_file))
    .fg_abort("Supply a new context destination beside the original.")
  name <- .fg_required_text(reach_name, "reach_name")
  view <- preview_study_reach_merge(dsn, reach_ids, retain_reach_id)
  args <- read_study_context(dsn); old <- args$reaches
  others <- old[!old$reach_id %in% reach_ids, ]
  if (any(others$stream_id == view$stream_id & tolower(trimws(others$reach_name)) == tolower(name)))
    .fg_abort("A Reach with this name already exists in this Stream.")
  item <- old[old$reach_id == retain_reach_id, ]
  item$reach_name <- name
  sf::st_geometry(item) <- sf::st_geometry(sf::st_transform(view$area, sf::st_crs(old)))
  result_reaches <- old[!old$reach_id %in% view$retired_reach_ids, ]
  j <- match(retain_reach_id, result_reaches$reach_id)
  result_reaches$reach_name[j] <- name
  sf::st_geometry(result_reaches)[j] <- sf::st_geometry(item)
  args$reaches <- result_reaches
  if (!is.null(args$survey_events)) args$survey_events$reach_id[
    args$survey_events$reach_id %in% view$retired_reach_ids] <- retain_reach_id
  if (isTRUE(view$source$piece_state)) {
    pieces <- view$source$pieces
    pieces$fg_reach_id[pieces$fg_reach_id %in% reach_ids] <- retain_reach_id
    args <- .fg_publish_pieces(args,dsn,view$source,pieces,"Combine Reaches")
    result <- .fg_save_study_revision(args,dsn,output_file,NULL,purpose)
    return(c(result,list(reach_id=retain_reach_id,evidence=attr(args,"piece_evidence"))))
  }
  evidence <- file.path(dirname(dsn), paste0("reach-selection-", .fg_generate_uuid(1L), ".gpkg"))
  if (file.exists(evidence)) .fg_abort("Reach evidence destination already exists.")
  line <- view$source$lines[match(view$source_id, view$source$lines$source_id), ]
  line$fg_reach_id <- retain_reach_id
  sf::st_write(line, evidence, layer = "retained_line", quiet = TRUE)
  sf::st_write(item, evidence, layer = "reach_area", quiet = TRUE)
  a <- sf::st_read(evidence, layer = "reach_area", quiet = TRUE)
  b <- sf::st_read(evidence, layer = "retained_line", quiet = TRUE)
  if (!identical(a$reach_id, retain_reach_id) || !identical(b$source_id, line$source_id) ||
      !isTRUE(all.equal(sf::st_coordinates(a), sf::st_coordinates(item))) ||
      !isTRUE(all.equal(sf::st_coordinates(b), sf::st_coordinates(line))))
    .fg_abort("Merged evidence failed round-trip verification; context was not saved.")
  note <- paste0("Reach merge [", retain_reach_id, "] Stream [", view$stream_id,
    "] source [", paste(view$source_id, collapse = ","), "]: ", basename(evidence),
    " / retained_line and reach_area; SHA256 ", .fg_file_sha256(evidence),
    ". Retired Reach identities: ", paste(view$retired_reach_ids, collapse = ","),
    "; reassigned Survey Events: ", view$reassigned_events,
    "; previous context: ", basename(dsn), "; parent evidence SHA256 ", view$source$sha256,
    ". Inherited buffer and processing CRS; prior revisions and evidence retained.")
  args$analyst_notes <- paste(args$analyst_notes, note, sep = "\n\n")
  result <- .fg_save_study_revision(args, dsn, output_file, NULL, purpose)
  c(result, list(evidence = evidence, reach_id = retain_reach_id))
}
