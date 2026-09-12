#' Render a selected view of saved Study Area context
#'
#' Reopens the same saved context and renders an existing report view. Purpose
#' is an explicit presentation choice, not a persisted project-origin flag,
#' identity change or acceptance action. The staging view describes only saved
#' context/interpretations; it does not inspect an unsaved legacy staging path.
#'
#' @param dsn Existing Study Area context GeoPackage.
#' @param output_file New .html destination; existing files are refused.
#' @param purpose One of terrain (the compatible combined Terrain Development
#'   view), definition (prospective Define Study Area), or staging (Staging Report).
#' @return Normalized report path invisibly. Requires the selected renderer's
#'   Pandoc and hard-link filesystem support. Source context and linked evidence
#'   are read-only; missing/changed pinned links retain the existing reader errors.
#' @export
study_context_report <- function(dsn, output_file, purpose = "terrain") {
  purpose <- .fg_choice(purpose, c("terrain", "definition", "staging"), "purpose")
  render <- switch(purpose, terrain = terrain_development_report,
    definition = define_study_area_report, staging = study_staging_report)
  render(read_study_context_summary(dsn), output_file)
}
