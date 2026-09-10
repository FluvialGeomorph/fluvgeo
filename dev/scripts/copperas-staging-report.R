# Run from fluvgeo with one NEW output directory outside the staged Study root.
# No source edits, hierarchy persistence, dates, clean-source approval or conversion.
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || file.exists(args[1])) stop("Supply a new output directory.")
root <- normalizePath("../FG-filedata/FileGDB/Collections/FluvialGeomorph/MVR_Copperas_Creek",
  winslash = "/", mustWork = TRUE)
out <- file.path(normalizePath(dirname(args[1]), winslash = "/", mustWork = TRUE), basename(args[1]))
stopifnot(!startsWith(tolower(out), paste0(tolower(root), "/")), tolower(out) != tolower(root))
stopifnot(dir.create(out))
snapshot <- function() {
  files <- sort(list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE))
  data.frame(path = substring(files, nchar(root) + 2L),
    sha256 = vapply(files, .fg_file_sha256, character(1)), row.names = NULL)
}
before <- snapshot()
source_gdb <- file.path(root, "Streams/Copperas_Creek/Copperas_Creek_Stream.gdb")
boundary <- sf::st_read(source_gdb, layer = "StudyArea_Stream", quiet = TRUE)
flowline <- sf::st_read(source_gdb, layer = "flowline", quiet = TRUE)
stopifnot(nrow(boundary) == 1L)
# Explicit demonstration identities, never written into staging or reconciled with FGDB.
study_id <- "11111111-1111-4111-8111-111111111111"
stream_id <- "22222222-2222-4222-8222-222222222222"
study <- sf::st_sf(study_area_id = study_id, study_area_name = "MVR_Copperas_Creek",
  geometry = sf::st_geometry(boundary))
stream <- sf::st_sf(stream_id = stream_id, study_area_id = study_id,
  stream_name = "Copperas Creek", geometry = sf::st_geometry(boundary))
repeated <- unique(flowline$ReachName[duplicated(flowline$ReachName)])
cases <- data.frame(
  case_id = c("copperas-boundary", "copperas-acquisitions", "copperas-reach-labels"),
  source_ref = c("Copperas_Creek_Stream.gdb / StudyArea_Stream; user clarification 2026-09-10",
    "CC_R1.gdb through CC_R15.gdb; user clarification 2026-09-10",
    "Copperas_Creek_Stream.gdb / flowline / ReachName"),
  proposed_structure = c("One Study Area and its sole Stream share this polygon, with distinct identities.",
    NA_character_, NA_character_),
  evidence = c("The user confirmed coincident Study Area and Stream boundaries for this single-Stream project.",
    "The user confirmed that the artifacts do not establish acquisition dates; an analyst must specify them.",
    paste("Repeated Reach labels:", paste(repeated, collapse = ", "),
      ". Repeated labels are not proof of duplicate geometry or permission to merge/delete.")),
  status = c("CONFIRMED", "UNKNOWN", "UNKNOWN"),
  analyst = c("User (project geography confirmation)", NA_character_, NA_character_),
  decision_notes = c("Use the supplied polygon for both report AOIs. Demonstration UUIDs are provisional, not staged or enterprise identities.",
    NA_character_, NA_character_))
summary <- terrain_development_summary(study_area = study, streams = stream,
  reconstruction = cases, legacy_staging = root,
  analyst_notes = paste(
    "The Study Area and sole Stream share the user-confirmed boundary. Report UUIDs are provisional demonstration labels; no hierarchy catalog has been written.",
    "Fifteen Reach-named source geodatabases are staged. Folder labels do not establish fifteen confirmed Survey Events. Acquisition dates require analyst input; no date or Reach identity has been inferred.",
    "Next: document supported acquisition dates and review the repeated R2 label, then reconstruct the explicit Study Area/Stream catalogs and event associations. Conversion is not yet assessed.",
    sep = "\n\n"))
report <- study_staging_report(summary, file.path(out, "copperas-staging-review.html"))
after <- snapshot()
stopifnot(identical(before, after))
saveRDS(summary, file.path(out, "staging-summary.rds"))
utils::write.csv(before, file.path(out, "source-snapshot-sha256.csv"), row.names = FALSE)
jsonlite::write_json(list(source_files_unchanged = TRUE, source_files = nrow(before),
  geodatabases = nrow(summary$staging_inventory$geodatabases),
  listed = sum(summary$staging_inventory$geodatabases$listing_status != "UNREADABLE"),
  vector_table_layers = nrow(summary$staging_inventory$layers),
  repeated_reach_labels = repeated, acquisition_dates = "UNKNOWN_REQUIRES_ANALYST",
  conversion_readiness = summary$staging_inventory$conversion_readiness,
  sf_version = as.character(utils::packageVersion("sf")), drivers = as.list(sf::sf_extSoftVersion())),
  file.path(out, "inspection-checks.json"), auto_unbox = TRUE, pretty = TRUE)
cat(report, "\n")
