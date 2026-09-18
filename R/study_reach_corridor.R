#' Read retained Stream segments for Reach definition
#'
#' Resolves the hash-linked Stream evidence written by add_study_stream_corridor.
#' Missing, changed or ambiguous evidence fails closed. No live discovery occurs.
#' @param dsn Existing study context GeoPackage.
#' @param stream_id Exact saved spatial Stream identity.
#' @return List with parent, lines, distance, unit, distance_m, method, evidence,
#'   sha256, assigned_source_ids, reach_mappings and mapping_notes. Mappings link
#'   current Reach identities to source IDs and their latest evidence notes.
#'   Lines retain the Stream processing CRS. ordering reports their downstream-to-
#'   upstream reference order, derived before clipping; unresolved is explicit.
#' @export
read_study_stream_segments <- function(dsn, stream_id) {
  dsn <- .fg_network_dsn(dsn)
  id <- .fg_required_text(stream_id, "stream_id")
  args <- read_study_context(dsn)
  if (!inherits(args$streams, "sf") || !id %in% args$streams$stream_id)
    .fg_abort("Select a saved Stream with an extent polygon.")
  notes <- strsplit(if (is.na(args$analyst_notes)) "" else args$analyst_notes, "\n", fixed = TRUE)[[1]]
  candidates <- notes[grepl(paste0("[", id, "]:"), notes, fixed = TRUE)]
  pattern <- "Source lines/parameters: (stream-selection-[0-9a-f-]+\\.gpkg) / selected_lines and clipped_lines;[^\n]*?; SHA256 ([0-9a-f]{64})\\."
  hits <- regmatches(candidates, regexec(pattern, candidates, perl = TRUE))
  hits <- Filter(function(x) length(x) == 3L, hits)
  if (length(hits) != 1L) .fg_abort("Stream segment evidence is missing or ambiguous; recover it before defining Reaches.")
  evidence <- file.path(dirname(dsn), hits[[1]][2])
  if (!file.exists(evidence) || .fg_file_sha256(evidence) != hits[[1]][3])
    .fg_abort("Stream segment evidence is missing or changed; nothing was inferred.")
  x <- sf::st_read(evidence, layer = "clipped_lines", quiet = TRUE)
  fields <- c("source_id", "fg_stream_id", "fg_buffer_distance", "fg_buffer_unit", "fg_buffer_m", "fg_buffer_method")
  if (!inherits(x, "sf") || !nrow(x) || nrow(x) > 500L || !all(fields %in% names(x)) ||
      is.na(sf::st_crs(x)) || sf::st_is_longlat(x) ||
      anyNA(x$source_id) || anyDuplicated(x$source_id) || !all(grepl("^[0-9]+$", x$source_id)) ||
      anyNA(x$fg_stream_id) || !all(x$fg_stream_id == id))
    .fg_abort("Stream evidence does not contain unique retained NHDPlus segments in a processing CRS.")
  parameters <- unique(sf::st_drop_geometry(x)[fields[-c(1,2)]])
  if (nrow(parameters) != 1L || anyNA(parameters) ||
      !parameters$fg_buffer_unit %in% c("m", "ft") ||
      !is.finite(parameters$fg_buffer_distance) || parameters$fg_buffer_distance <= 0 ||
      !is.finite(parameters$fg_buffer_m) || parameters$fg_buffer_m > 10000 ||
      abs(parameters$fg_buffer_m - parameters$fg_buffer_distance *
        if (parameters$fg_buffer_unit == "ft") .3048 else 1) > 1e-8 ||
      !startsWith(parameters$fg_buffer_method, "sf/GEOS clip lines -> metric buffer -> clip area; local WGS84 azimuthal equidistant CRS; 100 m geodesic edge densification; 0.001 m overlay grid; 0.002 m line-clip/containment precision margin; nQuadSegs=30;"))
    .fg_abort("Stream buffer metadata is inconsistent or uses an unsupported method; do not guess Reach settings.")
  assigned <- character()
  mappings <- data.frame(reach_id = character(), source_id = character())
  mapping_notes <- character()
  if (!is.null(args$reaches)) {
    # Only mappings tied to identities present in this revision count as assigned.
    for (rid in args$reaches$reach_id[args$reaches$stream_id == id]) {
      prefix <- paste0("Reach segment [", rid, "] Stream [", id, "] source [")
      merge_prefix <- paste0("Reach merge [", rid, "] Stream [", id, "] source [")
      row <- notes[startsWith(notes, prefix) | startsWith(notes, merge_prefix)]
      if (length(row) > 1L && !startsWith(tail(row, 1L), merge_prefix))
        .fg_abort("Ambiguous Reach segment mapping.")
      if (length(row)) {
        row <- tail(row, 1L)
        start <- if (startsWith(row, merge_prefix)) merge_prefix else prefix
        ids <- strsplit(sub("\\].*$", "", substring(row, nchar(start) + 1L)), ",", fixed = TRUE)[[1]]
        assigned <- c(assigned, ids)
        mappings <- rbind(mappings, data.frame(reach_id = rid, source_id = ids))
        mapping_notes[rid] <- row
      }
    }
  }
  # Order using the original reference network: clipping may disconnect or
  # multipart a source segment but must not change its network position.
  original <- sf::st_read(evidence, layer = "selected_lines", quiet = TRUE)
  walk <- order_drainage_flowlines(original, NULL, "upstream", "source_id")
  walk <- walk[walk$source_id %in% x$source_id, ]
  if (nrow(walk) != nrow(x) || anyDuplicated(walk$source_id))
    .fg_abort("Original and retained Stream source identities do not agree.")
  x <- x[match(walk$source_id, x$source_id), ]
  x$selection_id <- x$source_id
  mappings$selection_id <- mappings$source_id
  result <- list(parent = args$streams[args$streams$stream_id == id, ], lines = x,
    ordering = walk,
    distance = parameters$fg_buffer_distance, unit = parameters$fg_buffer_unit,
    distance_m = parameters$fg_buffer_m, method = parameters$fg_buffer_method,
    evidence = evidence, sha256 = hits[[1]][3], assigned_source_ids = unique(assigned),
    reach_mappings = mappings, mapping_notes = mapping_notes,
    assigned_selection_ids = unique(assigned), piece_state = FALSE)
  .fg_restore_pieces(result, args, dsn, notes)
}

#' Preview a Reach with its saved Stream's buffer settings
#' @param dsn Existing study context GeoPackage.
#' @param stream_id Exact saved parent Stream identity.
#' @param source_id One or more unique selection_id values from the Stream reader:
#'   COMIDs before piece editing, distinct local piece IDs after a split.
#' @return Corridor preview plus source, source_id and inherited settings. Uses
#'   the Stream processing CRS and buffer algorithm, clips to the parent Stream,
#'   and verifies containment. Adjacent Reach buffers may overlap. No writes.
#' @export
preview_study_reach_corridor <- function(dsn, stream_id, source_id) {
  source <- read_study_stream_segments(dsn, stream_id)
  id <- source_id
  if (!is.character(id) || !length(id) || anyNA(id) || anyDuplicated(id) ||
      !all(id %in% source$lines$selection_id)) .fg_abort("Choose unique retained segments from this Stream.")
  if (any(id %in% source$assigned_selection_ids)) .fg_abort("A selected segment already has a Reach in this Stream.")
  .fg_reach_preview(source, id)
}

.fg_reach_preview <- function(source, id) {
  line <- source$lines[match(id, source$lines$selection_id), ]
  previous <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(previous)), add = TRUE)
  suppressMessages(sf::sf_use_s2(TRUE))
  value <- .fg_clipped_corridor(line, source$distance_m, source$parent, crs = sf::st_crs(line))
  area <- value$area; area$reach_name <- "Candidate"
  if (any(check_study_area_containment(source$parent, reaches = area)$status != "inside"))
    .fg_abort("Reach containment could not be verified; nothing was saved.")
  c(value, list(source = source, source_id = id, selection_id = id))
}

#' Save one Reach from retained Stream segments
#' @param dsn Existing study context GeoPackage.
#' @param output_file New context GeoPackage beside dsn; never overwritten.
#' @param stream_id Exact saved parent Stream identity.
#' @param source_id One or more selection_id values from the Stream reader.
#' @param reach_name Explicit unique Reach name within this Stream.
#' @param report_purpose Revision purpose, normally definition.
#' @return Revision paths plus evidence and reach_id. Recomputes the preview,
#'   retains hash-linked segment/area evidence, and preserves existing records.
#'   Evidence is written first; failure may leave an unreferenced evidence file.
#' @export
add_study_reach_corridor <- function(dsn, output_file, stream_id, source_id,
                                     reach_name, report_purpose = "definition") {
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  purpose <- .fg_choice(report_purpose, c("definition", "terrain", "staging"), "report_purpose")
  if (dirname(dsn) != dirname(output_file) || file.exists(output_file))
    .fg_abort("Supply a new context destination beside the original.")
  name <- .fg_required_text(reach_name, "reach_name")
  args <- read_study_context(dsn); old <- args$reaches
  if (!is.null(old) && !inherits(old, "sf"))
    .fg_abort("Assign existing names-only Reach areas before adding spatial Reaches.")
  if (!is.null(old) && any(old$stream_id == stream_id & tolower(trimws(old$reach_name)) == tolower(name)))
    .fg_abort("A Reach with this name already exists in this Stream.")
  view <- preview_study_reach_corridor(dsn, stream_id, source_id)
  id <- .fg_generate_uuid(1L)
  item <- sf::st_sf(reach_id = id, stream_id = stream_id, reach_name = name,
    geometry = sf::st_geometry(view$area))
  if (!is.null(old)) {
    item <- sf::st_transform(item, sf::st_crs(old))
    old <- sf::st_cast(old, "MULTIPOLYGON")
    names(item)[names(item) == attr(item, "sf_column")] <- attr(old, "sf_column")
    sf::st_geometry(item) <- attr(old, "sf_column")
    args$reaches <- rbind(old, item)
  } else args$reaches <- item
  if (isTRUE(view$source$piece_state)) {
    pieces <- view$source$pieces
    pieces$fg_reach_id[match(source_id,pieces$piece_id)] <- id
    args <- .fg_publish_pieces(args,dsn,view$source,pieces,"Add Reach")
    result <- .fg_save_study_revision(args,dsn,output_file,NULL,purpose)
    return(c(result,list(reach_id=id,evidence=attr(args,"piece_evidence"))))
  }
  evidence <- file.path(dirname(dsn), paste0("reach-selection-", id, ".gpkg"))
  if (file.exists(evidence)) .fg_abort("Reach evidence destination already exists.")
  line <- view$source$lines[match(source_id, view$source$lines$source_id), ]
  line$fg_reach_id <- id
  sf::st_write(line, evidence, layer = "retained_line", quiet = TRUE)
  sf::st_write(item, evidence, layer = "reach_area", quiet = TRUE)
  restored <- sf::st_read(evidence, layer = "reach_area", quiet = TRUE)
  restored_line <- sf::st_read(evidence, layer = "retained_line", quiet = TRUE)
  if (!identical(restored$reach_id, id) || !identical(restored_line$source_id, source_id) ||
      !isTRUE(all.equal(sf::st_coordinates(restored), sf::st_coordinates(item))) ||
      !isTRUE(all.equal(sf::st_coordinates(restored_line), sf::st_coordinates(line))))
    .fg_abort("Reach evidence round-trip failed; context was not saved.")
  note <- paste0("Reach segment [", id, "] Stream [", stream_id, "] source [", paste(source_id, collapse = ","),
    "]: ", basename(evidence), " / retained_line and reach_area; SHA256 ", .fg_file_sha256(evidence),
    ". Inherited ", view$source$distance, " ", view$source$unit, " on EACH side; parent evidence ",
    basename(view$source$evidence), " SHA256 ", view$source$sha256,
    "; clipped to Stream; adjacent Reach buffers may overlap.")
  args$analyst_notes <- if (is.na(args$analyst_notes)) note else paste(args$analyst_notes, note, sep = "\n\n")
  result <- .fg_save_study_revision(args, dsn, output_file, NULL, purpose)
  c(result, list(evidence = evidence, reach_id = id))
}
