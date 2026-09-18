#' Preview an explicitly selected Stream corridor
#'
#' Clips selected lines, then buffers and clips in a local metric
#' CRS when a boundary is supplied. Without a boundary, uses an S2 outer buffer.
#' Neither is a floodplain delineation or repaired/ordered channel network.
#' @param lines sf with 1 to 500 valid nonempty finite XY line features and a CRS.
#' @param distance Positive buffer distance on EACH side of the lines.
#' @param unit Distance unit: "m" or international "ft" (exactly 0.3048 m).
#' @param boundary Optional Study Area polygon. Clip lines first, buffer their
#'   retained portions, then clip the buffer, in a local metric CRS. NULL
#'   preserves the unrestricted spherical buffer preview.
#' @return List containing area (one MULTIPOLYGON with CRS), distance_m,
#'   selected_features, polygon_parts, method, unclipped_area, clipped and
#'   removed_area_m2. Boundary previews also include clipped_lines, source_rows,
#'   line_lengths, retained_features, line_clipped and processing_crs. No writes.
#' @export
preview_stream_corridor <- function(lines, distance, unit = "m", boundary = NULL) {
  unit <- .fg_choice(unit, c("m", "ft"), "unit")
  if (!is.numeric(distance) || length(distance) != 1L || !is.finite(distance) || distance <= 0)
    .fg_abort("Supply a positive buffer distance on each side of the channel.")
  metres <- distance * if (unit == "ft") 0.3048 else 1
  if (metres > 10000) .fg_abort("This corridor preview supports buffers up to 10,000 m per side.")
  if (!inherits(lines, "sf") || nrow(lines) < 1L || nrow(lines) > 500L ||
      is.na(sf::st_crs(lines)) ||
      !all(as.character(sf::st_geometry_type(lines)) %in% c("LINESTRING", "MULTILINESTRING")) ||
      !all(vapply(sf::st_geometry(lines), inherits, logical(1), "XY")) ||
      any(sf::st_is_empty(lines)) || !all(sf::st_is_valid(lines) %in% TRUE))
    .fg_abort("Select 1 to 500 valid, nonempty CRS-defined XY line features.")
  g <- sf::st_transform(sf::st_geometry(lines), 4326)
  xy <- sf::st_coordinates(g)
  if (nrow(xy) > 1000000L || any(!is.finite(xy[, 1:2])) ||
      any(abs(xy[, 1]) > 180) || any(abs(xy[, 2]) > 90))
    .fg_abort("Selected line coordinates are invalid or exceed the preview limit.")
  previous <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(previous)), add = TRUE)
  suppressMessages(sf::sf_use_s2(TRUE))
  if (!is.null(boundary)) return(.fg_clipped_corridor(lines, metres, boundary))
  # Buffer the collection once: unioning separately tessellated buffers can
  # introduce duplicate vertices at adjacent segment joins in narrow corridors.
  area <- sf::st_cast(sf::st_buffer(sf::st_combine(g), dist = metres, max_cells = 10000L), "MULTIPOLYGON")
  if (sf::st_is_empty(area) || !isTRUE(sf::st_is_valid(area)))
    .fg_abort("Buffer did not produce a valid area; nothing was repaired.")
  full <- area
  list(area = sf::st_sf(geometry = area), distance_m = metres,
    selected_features = nrow(lines), polygon_parts = length(sf::st_cast(area, "POLYGON")),
    unclipped_area = sf::st_sf(geometry = full), clipped = FALSE, removed_area_m2 = 0,
    method = "sf/S2 spherical outer-cell buffer of collected lines; max_cells=10000")
}

# Local working CRS, not a project analysis-CRS decision. Limit the domain to
# regional studies; a projection centred on the parent must not be used globally.
.fg_corridor_crs <- function(boundary) {
  b <- sf::st_bbox(sf::st_transform(boundary, 4326))
  if (b[[3]] - b[[1]] > 6 || b[[4]] - b[[2]] > 6 ||
      b[[2]] < -80 || b[[4]] > 80)
    .fg_abort("Corridor processing requires a regional Study Area (at most 6 degrees wide/high, between 80 S and 80 N).")
  sf::st_crs(sprintf("+proj=aeqd +lat_0=%.12f +lon_0=%.12f +datum=WGS84 +units=m +no_defs",
    (b[[2]] + b[[4]]) / 2, (b[[1]] + b[[3]]) / 2))
}

# Materialize a common millimetre overlay grid. This is numerical precision,
# not an outside-area allowance or a claim of source positional accuracy.
.fg_corridor_grid <- function(x, crs) {
  g <- sf::st_geometry(x)
  if (sf::st_crs(g) != crs)
    g <- sf::st_segmentize(sf::st_transform(g,4326), dfMaxLength = 100)
  g <- sf::st_transform(g, crs)
  sf::st_as_sfc(sf::st_as_binary(sf::st_set_precision(g, 1000)), crs = crs) |>
    sf::st_set_precision(1000)
}

.fg_clipped_corridor <- function(lines, metres, boundary, crs = NULL) {
  check_study_area_containment(boundary)
  if (is.null(crs)) crs <- .fg_corridor_crs(boundary)
  parent <- .fg_corridor_grid(boundary, crs)
  g <- .fg_corridor_grid(lines, crs)
  retained <- list(); rows <- integer()
  original_length <- as.numeric(sf::st_length(g))
  retained_length <- numeric(length(g))
  # The same 2 mm numerical margin used for final verification preserves
  # coincident edges after geodesic densification and projection/grid rounding.
  # Area clipping below uses the unexpanded parent. No source geometry changes.
  line_parent <- sf::st_buffer(parent, .002)
  for (i in seq_along(g)) {
    part <- sf::st_intersection(g[i], line_parent)
    types <- as.character(sf::st_geometry_type(part))
    part <- part[types %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION")]
    if (!length(part)) next
    part <- suppressWarnings(sf::st_collection_extract(part, "LINESTRING"))
    part <- part[!sf::st_is_empty(part)]
    # Sub-grid point-contact remnants must not become a buffered Stream.
    if (!length(part) || sum(as.numeric(sf::st_length(part))) <= .004) next
    part <- sf::st_cast(sf::st_combine(part), "MULTILINESTRING")
    retained[[length(retained) + 1L]] <- part[[1]]
    rows <- c(rows, i)
    retained_length[i] <- sum(as.numeric(sf::st_length(part)))
  }
  if (!length(rows))
    .fg_abort("Selected flowlines are outside the Study Area or only touch its boundary; no positive-length line remains after clipping.")
  clipped_lines <- lines[rows, ]
  sf::st_geometry(clipped_lines) <- sf::st_sfc(retained, crs = crs, precision = 1000)
  sf::st_geometry(clipped_lines) <- .fg_corridor_grid(clipped_lines, crs)
  full <- sf::st_cast(sf::st_buffer(sf::st_union(sf::st_geometry(clipped_lines)), metres,
    nQuadSegs = 30L), "MULTIPOLYGON")
  full <- .fg_corridor_grid(full, crs)
  outside <- sf::st_difference(full, parent)
  clipped <- length(outside) > 0L && any(!sf::st_is_empty(outside))
  area <- if (clipped) sf::st_cast(sf::st_intersection(full, parent), "MULTIPOLYGON") else full
  if (length(area) != 1L || sf::st_is_empty(area) || !isTRUE(sf::st_is_valid(area)) ||
      as.numeric(sf::st_area(area)) <= 0)
    .fg_abort("Clipping did not produce a valid Stream area; nothing was saved.")
  removed <- max(0, as.numeric(sf::st_area(full)) - as.numeric(sf::st_area(area)))
  # Persist rounded coordinates, not only an in-memory precision attribute.
  area <- .fg_corridor_grid(area, crs)
  # Precision is expressed in coordinate units. Do not carry a metre-grid
  # attribute into a client's later longitude/latitude display transformation.
  area <- sf::st_set_precision(area, 0)
  full <- sf::st_set_precision(full, 0)
  sf::st_geometry(clipped_lines) <- sf::st_set_precision(sf::st_geometry(clipped_lines), 0)
  list(area = sf::st_sf(geometry = area), distance_m = metres,
    selected_features = nrow(lines), retained_features = length(rows),
    clipped_lines = clipped_lines, source_rows = rows,
    line_lengths = data.frame(source_row = seq_len(nrow(lines)),
      original_m = original_length, retained_m = retained_length),
    line_clipped = any(original_length - retained_length > .001),
    polygon_parts = length(sf::st_cast(area, "POLYGON")),
    unclipped_area = sf::st_sf(geometry = full), clipped = clipped,
    removed_area_m2 = removed, processing_crs = crs$wkt,
    method = paste0("sf/GEOS clip lines -> metric buffer -> clip area; local WGS84 azimuthal equidistant CRS; ",
      "100 m geodesic edge densification; 0.001 m overlay grid; 0.002 m line-clip/containment precision margin; nQuadSegs=30; ", crs$input))
}

#' Check child areas against a proposed Study Area boundary
#'
#' Read-only GEOS outside-difference check in the parent's local metric CRS,
#' with a 1 mm overlay grid and 2 mm numerical precision margin. No saved
#' geometry is expanded. Named records without geometry are explicitly unknown.
#' @param boundary One valid CRS-defined XY Study Area polygon.
#' @param streams,reaches Optional saved child tables or polygon sf objects.
#' @return Data frame with level, name and status: inside, outside or unknown.
#' @export
check_study_area_containment <- function(boundary, streams = NULL, reaches = NULL) {
  if (!inherits(boundary, "sf") || nrow(boundary) != 1L)
    .fg_abort("Supply one Study Area POLYGON boundary.")
  .fg_terrain_polygon(boundary)
  if (!inherits(sf::st_geometry(boundary)[[1]], "XY") || !all(is.finite(sf::st_coordinates(boundary))))
    .fg_abort("Study Area boundary must have finite XY coordinates.")
  previous <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(previous)), add = TRUE)
  suppressMessages(sf::sf_use_s2(TRUE))
  crs <- .fg_corridor_crs(boundary)
  parent <- .fg_corridor_grid(boundary, crs)
  out <- data.frame(level = character(), name = character(), status = character())
  for (kind in c("stream", "reach")) {
    x <- if (kind == "stream") streams else reaches
    if (is.null(x) || !nrow(x)) next
    status <- rep("unknown", nrow(x))
    if (inherits(x, "sf")) {
      .fg_terrain_polygon(x)
      if (!all(vapply(sf::st_geometry(x), inherits, logical(1), "XY")) || !all(is.finite(sf::st_coordinates(x))))
        .fg_abort("Child areas must have finite XY coordinates.")
      child <- .fg_corridor_grid(x, crs)
      covered <- vapply(seq_along(child), function(i) {
        # Grid rounding can put intersection vertices up to a grid diagonal
        # outside a slanted edge. This fixed 2 mm numerical margin is not a
        # user-selected corridor tolerance and does not expand the saved area.
        outside <- sf::st_difference(child[i], sf::st_buffer(parent, .002))
        !length(outside) || all(sf::st_is_empty(outside))
      }, logical(1))
      status <- ifelse(covered, "inside", "outside")
    }
    out <- rbind(out, data.frame(level = kind, name = x[[paste0(kind, "_name")]], status = status))
  }
  out
}

#' Save one selected Stream corridor in a new study revision
#'
#' Adds one new Stream, or supplies the area of one existing names-only Stream
#' by explicit ID. Existing spatial Streams are not edited by this operation.
#' Selected lines are clipped first; their buffered area is clipped again.
#' Selected lines and
#' parameters are retained in a separate evidence GeoPackage beside the context.
#' @param dsn Existing study context GeoPackage.
#' @param output_file New GeoPackage beside dsn; never overwritten.
#' @param lines Selected line sf with unique nonempty character source_id values.
#'   All source fields and native geometry are retained as evidence.
#' @param stream_name Explicit name for a new Stream; not inferred from source names.
#' @param distance,unit See preview_stream_corridor().
#' @param add_note Required selection rationale.
#' @param stream_id Optional exact existing names-only Stream ID. Its name is
#'   retained. If several names-only Streams exist, this bounded tool refuses
#'   partial assignment because the context format cannot mix missing areas.
#' @param report_purpose Context revision purpose, normally "definition".
#' @return List with context, report (NULL), evidence and stream_id. Evidence is
#'   written first and hash-linked in notes; failure can leave unreferenced
#'   evidence but does not replace any earlier context. No network acceptance,
#'   Reach creation, topology repair or enterprise loading is performed.
#' @export
add_study_stream_corridor <- function(dsn, output_file, lines, stream_name,
    distance, unit = "m", add_note, stream_id = NULL, report_purpose = "definition") {
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  purpose <- .fg_choice(report_purpose, c("definition", "staging", "terrain"), "report_purpose")
  if (dirname(dsn) != dirname(output_file) || file.exists(output_file))
    .fg_abort("Supply a new context destination beside the original.")
  note <- .fg_required_text(add_note, "add_note")
  args <- read_study_context(dsn)
  if (!inherits(args$study_area, "sf")) .fg_abort("Save a Study Area boundary before defining a Stream corridor.")
  old <- args$streams
  if (is.null(stream_id)) {
    name <- .fg_required_text(stream_name, "stream_name")
    if (!is.null(old) && any(tolower(trimws(old$stream_name)) == tolower(name)))
      .fg_abort("A Stream with this name already exists; select its saved identity instead.")
    if (!is.null(old) && !inherits(old, "sf"))
      .fg_abort("Assign the existing named Stream areas before adding another Stream.")
    id <- .fg_generate_uuid(1L)
  } else {
    id <- .fg_required_text(stream_id, "stream_id")
    if (is.null(old) || !id %in% old$stream_id || inherits(old, "sf"))
      .fg_abort("Select an existing names-only Stream; saved spatial Streams are not replaced.")
    if (nrow(old) != 1L)
      .fg_abort("Multiple names-only Streams require their areas together; partial assignment is not supported yet.")
    name <- old$stream_name[[1]]
  }
  preview <- preview_stream_corridor(lines, distance, unit, boundary = args$study_area)
  item <- sf::st_sf(stream_id = id, study_area_id = args$study_area$study_area_id,
    stream_name = name, geometry = sf::st_geometry(preview$area))
  check <- check_study_area_containment(args$study_area, item)
  if (any(check$status != "inside"))
    .fg_abort("The clipped Stream area could not be verified inside the Study Area; nothing was saved.")
  if (!is.null(old) && inherits(old, "sf")) {
    # Convert the new area to the existing semantic CRS without modifying old rows.
    item <- sf::st_transform(item, sf::st_crs(old))
    old <- sf::st_cast(old, "MULTIPOLYGON")
    args$streams <- rbind(old, item)
  } else args$streams <- item
  ids <- lines$source_id
  if (!is.character(ids) || length(ids) != nrow(lines) || anyNA(ids) ||
      any(!nzchar(trimws(ids))) || anyDuplicated(ids))
    .fg_abort("Selected lines require unique nonempty character source_id values.")
  reserved <- c("fg_stream_id", "fg_buffer_distance", "fg_buffer_unit", "fg_buffer_m", "fg_buffer_method", "fg_selected_at", "fg_buffer_clipped", "fg_removed_m2", "fg_original_m", "fg_retained_m")
  if (any(reserved %in% names(lines))) .fg_abort("Source lines contain reserved fg_ evidence fields.")
  lines$fg_stream_id <- id; lines$fg_buffer_distance <- distance; lines$fg_buffer_unit <- unit
  lines$fg_buffer_m <- preview$distance_m; lines$fg_buffer_method <- preview$method
  lines$fg_buffer_clipped <- preview$clipped; lines$fg_removed_m2 <- preview$removed_area_m2
  lines$fg_selected_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  lines$fg_original_m <- preview$line_lengths$original_m
  lines$fg_retained_m <- preview$line_lengths$retained_m
  evidence <- file.path(dirname(dsn), paste0("stream-selection-", .fg_generate_uuid(1L), ".gpkg"))
  if (file.exists(evidence)) .fg_abort("Evidence destination already exists.")
  sf::st_write(lines, evidence, layer = "selected_lines", append = FALSE, quiet = TRUE)
  processed <- lines[preview$source_rows, ]
  sf::st_geometry(processed) <- sf::st_geometry(preview$clipped_lines)
  sf::st_write(processed, evidence, layer = "clipped_lines", append = FALSE, quiet = TRUE)
  processed_read <- sf::st_read(evidence, layer = "clipped_lines", quiet = TRUE)
  if (!identical(processed_read$source_id, processed$source_id) ||
      !isTRUE(all.equal(sf::st_coordinates(processed_read), sf::st_coordinates(processed))))
    .fg_abort("Clipped-line evidence could not be verified; context was not saved.")
  retained <- sf::st_read(evidence, layer = "selected_lines", quiet = TRUE)
  if (!identical(retained$source_id, ids) ||
      !all(vapply(seq_len(nrow(lines)), function(i) i %in% sf::st_equals(retained[i, ], lines)[[1]], logical(1))))
    .fg_abort("Selected-line evidence could not be verified; context was not saved.")
  note <- paste0("Stream ", name, " [", id, "]: ", distance, " ", unit,
    " on EACH side; ", preview$method, "; clipped=", preview$clipped,
    "; removed area m2=", signif(preview$removed_area_m2, 8), ". Source lines/parameters: ", basename(evidence),
    " / selected_lines and clipped_lines; retained line length m=",
    signif(sum(preview$line_lengths$retained_m), 8), "; SHA256 ", .fg_file_sha256(evidence), ". ", note)
  args$analyst_notes <- if (is.na(args$analyst_notes)) note else paste(args$analyst_notes, note, sep = "\n\n")
  result <- .fg_save_study_revision(args, dsn, output_file, NULL, purpose)
  c(result, list(evidence = evidence, stream_id = id))
}
