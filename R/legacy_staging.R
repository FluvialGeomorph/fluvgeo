#' Inspect a legacy FileGDB staging Study Area without modifying it
#'
#' Inventories directories and driver-reported vector/table layer metadata under
#' one Study Area root. Compares catalog and event-folder presence with the legacy
#' staging review draft 0.1. Paths are locators, not inferred scientific identities.
#' No feature values, rasters, acquisition dates or source snapshots are validated.
#'
#' @param root Existing Study Area directory in FileGDB staging, not the archive,
#'   Collection root, or a geodatabase. Directory links are not followed and
#'   traversal stops at geodatabases and at eight directory levels.
#' @return A list tagged FLUVGEO_LEGACY_STAGING_REVIEW_1 with geodatabases,
#'   layers, catalogs and assessment data frames; relative directory locators;
#'   inspection time; and explicit NOT_ASSESSED conversion/FGDB readiness.
#'   Missing structure, unreadable sources and skipped paths remain findings.
#'   Catalog presence does not validate its fields, values, geometry or links.
#'   The inspector never creates IDs, infers dates, selects clean sources, writes
#'   files, converts data or loads FGDB. Driver feature counts may be unknown.
#' @export
inspect_legacy_staging <- function(root) {
  root <- .fg_manifest_root(root)
  if (grepl("\\.gdb$", root, ignore.case = TRUE))
    .fg_abort("Supply a Study Area directory, not a geodatabase.")
  assessment <- data.frame(code = character(), entity_id = character(),
    entity_label = character(), stage = character(), status = character(),
    requires_input = logical(), next_action = character())
  add <- function(code, path, status, input, action) {
    assessment <<- rbind(assessment, data.frame(code = code,
      entity_id = paste0("staging:", path), entity_label = path,
      stage = "LEGACY_STAGING", status = status, requires_input = input,
      next_action = action))
  }
  visit <- .fg_legacy_walk(root, add)
  dirs <- visit$directories; gdbs <- visit$geodatabases
  skipped <- assessment$entity_label[assessment$code %in% c(
    "STAGING_DIRECTORY_UNREADABLE", "STAGING_PATH_SKIPPED", "STAGING_DEPTH_LIMIT")]
  location_unknown <- function(path) any(vapply(skipped, function(p)
    p == "." || tolower(path) == tolower(p) ||
      startsWith(tolower(path), paste0(tolower(p), "/")), logical(1)))

  geodatabases <- data.frame(path = gdbs, placement = vapply(gdbs,
    .fg_legacy_placement, character(1)), listing_status = rep("UNREADABLE", length(gdbs)),
    layer_count = rep(NA_integer_, length(gdbs)), diagnostic = rep(NA_character_, length(gdbs)))
  layers <- data.frame(path = character(), layer = character(),
    geometry_type = character(), features = double(), fields = integer(), crs = character())
  listings <- setNames(vector("list", length(gdbs)), gdbs)
  for (i in seq_along(gdbs)) {
    warnings <- character()
    listed <- tryCatch(withCallingHandlers(.fg_legacy_layers(file.path(root, gdbs[i])),
      warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }),
      error = function(e) e)
    if (inherits(listed, "error")) {
      geodatabases$diagnostic[i] <- conditionMessage(listed)
      add("STAGING_GDB_UNREADABLE", gdbs[i], "BLOCKED", TRUE,
        "Review the unreadable geodatabase or driver access; its content remains unknown.")
      next
    }
    listings[[gdbs[i]]] <- listed
    geodatabases$listing_status[i] <- if (length(warnings)) "LISTED_WITH_WARNINGS" else "LISTED"
    geodatabases$layer_count[i] <- nrow(listed)
    if (length(warnings)) {
      geodatabases$diagnostic[i] <- paste(unique(warnings), collapse = " | ")
      add("STAGING_DRIVER_WARNING", gdbs[i], "REVIEW_REQUIRED", TRUE,
        "Review driver warnings in the source inventory before relying on its layer listing.")
    }
    if (nrow(listed)) layers <- rbind(layers, cbind(path = gdbs[i], listed))
    add("STAGING_LAYER_LISTED", gdbs[i], "VERIFIED", FALSE,
      "Vector/table layer metadata listed only; feature values, rasters and cleanliness are not checked.")
  }
  if (!length(gdbs)) add("STAGING_NO_GDB", ".", "REVIEW_REQUIRED", TRUE,
    "Select a Study Area staging root containing the analyst-copied FileGDB sources.")

  catalogs <- data.frame(path = character(), record = character(), present = logical())
  check_catalog <- function(path, records, kind) {
    idx <- match(tolower(path), tolower(gdbs))
    listing <- if (is.na(idx)) NULL else listings[[idx]]
    present <- if (is.na(idx)) rep(if (location_unknown(path)) NA else FALSE, length(records)) else if (is.null(listing))
      rep(NA, length(records)) else records %in% listing$layer
    catalogs <<- rbind(catalogs, data.frame(path = path, record = records, present = present))
    if (any(!present, na.rm = TRUE)) add(paste0("STAGING_", kind, "_CATALOG_MISSING"),
      path, "REVIEW_REQUIRED", TRUE, if (kind == "STUDY")
        "Reconstruct the Study Area catalog, confirmed boundary and source inventory in staging; retain the archive unchanged."
      else "Reconstruct the Stream catalog with explicit Reach/event identities, supported acquisition dates and source associations; do not infer dates from filenames.")
  }
  check_catalog("StudyArea.gdb", c("study_area", "study_area_geometry", "stream_catalog",
    "migration_source", "migration_item"), "STUDY")
  stream_dirs <- dirs[grepl("^Streams/[^/]+$", dirs, ignore.case = TRUE)]
  if (!length(stream_dirs) && !location_unknown("Streams")) add("STAGING_STREAM_FOLDERS_MISSING", "Streams", "REVIEW_REQUIRED", TRUE,
    "Identify the selected Streams and configure their staging folders; folder names alone do not establish identity.")
  for (s in stream_dirs) check_catalog(paste0(s, "/Stream.gdb"),
    c("stream", "reach", "survey_event", "event_source"), "STREAM")
  for (i in which(geodatabases$placement == "LEGACY_REACH_LOCATION"))
    add("STAGING_EVENT_FOLDER_MISSING", gdbs[i], "REVIEW_REQUIRED", TRUE,
      "After confirming the acquisition and parent Reach, place the selected staged copy in its explicit SurveyEvents/event folder; this inspector does not move it.")
  for (i in which(geodatabases$placement == "OTHER"))
    add("STAGING_UNCLASSIFIED_LOCATION", gdbs[i], "REVIEW_REQUIRED", TRUE,
      "Assign this source an explicit role; its location does not match the inspected staging layout.")
  add("STAGING_VALIDATION_LIMIT", ".", "NOT_ASSESSED", FALSE,
    "Full catalog values, identities, dates, source selection, rasters and conversion mappings still require validation; presence is not conversion readiness.")
  list(schema = "FLUVGEO_LEGACY_STAGING_REVIEW_1", profile = "legacy-staging-draft-0.1",
    root = root, generated_at = Sys.time(), directories = dirs,
    directory_inventory_complete = !length(skipped),
    geodatabases = geodatabases, layers = layers, catalogs = catalogs,
    assessment = assessment, conversion_readiness = "NOT_ASSESSED",
    fgdb_readiness = "NOT_ASSESSED")
}

.fg_legacy_children <- function(path) {
  if (file.access(path, 4L) != 0L) stop("Directory is not readable.")
  paths <- list.files(path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  sort(basename(paths[dir.exists(paths)]))
}

.fg_legacy_walk <- function(root, add) {
  dirs <- gdbs <- character()
  visit <- function(relative = "", depth = 0L) {
    parent <- if (nzchar(relative)) file.path(root, relative) else root
    children <- tryCatch(.fg_legacy_children(parent), error = function(e) e)
    if (inherits(children, "error")) {
      add("STAGING_DIRECTORY_UNREADABLE", if (nzchar(relative)) relative else ".",
        "BLOCKED", TRUE, "Restore directory read access and repeat inspection; its contents are unknown.")
      return(invisible(NULL))
    }
    for (name in children) {
      rel <- if (nzchar(relative)) paste(relative, name, sep = "/") else name
      path <- file.path(root, rel)
      link <- Sys.readlink(path)
      safe <- tryCatch({ .fg_manifest_path(root, rel); TRUE }, error = function(e) FALSE)
      if (!safe || (!is.na(link) && nzchar(link))) {
        add("STAGING_PATH_SKIPPED", rel, "BLOCKED", TRUE,
          "Supply an ordinary in-root directory; links and unsafe paths are not inspected.")
        next
      }
      if (grepl("\\.gdb$", name, ignore.case = TRUE)) gdbs <<- c(gdbs, rel)
      else {
        dirs <<- c(dirs, rel)
        if (depth >= 7L) add("STAGING_DEPTH_LIMIT", rel, "BLOCKED", TRUE,
          "Review nested content beyond the eight-level inspection limit; inventory is incomplete.")
        else visit(rel, depth + 1L)
      }
    }
    invisible(NULL)
  }
  visit()
  list(directories = dirs, geodatabases = gdbs)
}

.fg_legacy_placement <- function(path) {
  if (tolower(path) == "studyarea.gdb") return("STUDY_CATALOG_LOCATION")
  if (grepl("^Streams/[^/]+/Stream\\.gdb$", path, ignore.case = TRUE)) return("STREAM_CATALOG_LOCATION")
  if (grepl("^Streams/[^/]+/[^/]+\\.gdb$", path, ignore.case = TRUE)) return("STREAM_SOURCE_LOCATION")
  if (grepl("^Streams/[^/]+/Reaches/[^/]+/SurveyEvents/[^/]+/[^/]+\\.gdb$", path, ignore.case = TRUE)) return("EVENT_LOCATION")
  if (grepl("^Streams/[^/]+/Reaches/[^/]+/[^/]+\\.gdb$", path, ignore.case = TRUE)) return("LEGACY_REACH_LOCATION")
  "OTHER"
}

.fg_legacy_layers <- function(path) {
  x <- sf::st_layers(path, do_count = FALSE)
  data.frame(layer = x$name,
    geometry_type = vapply(x$geomtype, paste, collapse = ", ", FUN.VALUE = character(1)),
    features = ifelse(x$features < 0, NA_real_, as.numeric(x$features)),
    fields = as.integer(x$fields),
    crs = vapply(x$crs, function(crs) if (is.na(crs)) NA_character_ else crs$Name, character(1)))
}
