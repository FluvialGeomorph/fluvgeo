#' Rename a saved Stream or Reach without changing identity or geometry
#' @param dsn Existing study context GeoPackage.
#' @param output_file New context GeoPackage beside dsn.
#' @param level Either stream or reach.
#' @param feature_id Exact saved Stream or Reach identity.
#' @param name Nonempty replacement name, unique within its parent ignoring case.
#' @param report_purpose Revision purpose: definition, terrain or staging.
#' @return Revision paths. Geometry, identities, relationships, source evidence
#'   and earlier revisions are unchanged. Unchanged names are not a new revision.
#' @export
rename_study_feature <- function(dsn, output_file, level, feature_id, name,
                                 report_purpose = "definition") {
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  level <- .fg_choice(level, c("stream", "reach"), "level")
  purpose <- .fg_choice(report_purpose, c("definition","terrain","staging"), "report_purpose")
  if (dirname(dsn) != dirname(output_file) || file.exists(output_file))
    .fg_abort("Supply a new context destination beside the original.")
  id <- .fg_required_text(feature_id, "feature_id")
  name <- .fg_required_text(name, "name")
  args <- read_study_context(dsn)
  table <- if (level == "stream") "streams" else "reaches"
  id_col <- paste0(level,"_id"); name_col <- paste0(level,"_name")
  parent <- if (level == "stream") "study_area_id" else "stream_id"
  x <- args[[table]]; j <- match(id,x[[id_col]])
  if (is.na(j)) .fg_abort("Choose an existing identity in this study.")
  others <- seq_len(nrow(x)) != j & x[[parent]] == x[[parent]][j]
  if (any(tolower(trimws(x[[name_col]][others])) == tolower(name)))
    .fg_abort("That name already exists within this parent.")
  if (identical(x[[name_col]][j], name)) .fg_abort("Name is unchanged.")
  x[[name_col]][j] <- name; args[[table]] <- x
  .fg_save_study_revision(args, dsn, output_file, NULL, purpose)
}
