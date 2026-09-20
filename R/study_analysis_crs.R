#' Validate a projected horizontal analysis CRS
#'
#' Resolves an analyst choice to WKT using sf/PROJ. When a boundary is supplied,
#' checks a complete transformation without ballpark operations. This does not
#' certify projection distortion, transform elevation, or modify source geometry.
#' Epoch-dependent reference frames are rejected until an explicit coordinate-epoch
#' workflow is supported.
#' @param crs EPSG number, authority string, or WKT for a projected 2D CRS.
#' @param boundary Optional nonempty sf or sfc Study Area polygon.
#' @return List with wkt, name, epsg and unit.
#' @export
validate_study_analysis_crs <- function(crs, boundary = NULL) {
  if (length(crs) != 1L || is.na(crs) ||
      !(is.character(crs) || is.numeric(crs))) .fg_abort("Enter an EPSG code or projected CRS WKT.")
  if (is.character(crs) && grepl("^[0-9]+$", trimws(crs))) crs <- paste0("EPSG:", trimws(crs))
  resolved <- tryCatch(suppressWarnings(sf::st_crs(crs)), error = function(e) NULL)
  if (is.null(resolved) || is.na(resolved)) .fg_abort("The CRS could not be resolved. Check its EPSG code or WKT.")
  # GDAL/PROJ WKT is UTF-8; sf can return it unmarked under Windows' C locale.
  # Mark it before sf's writer converts character fields to UTF-8.
  wkt <- resolved$wkt
  Encoding(wkt) <- "UTF-8"
  if (!grepl("^PROJCRS\\[", resolved$wkt) || !isFALSE(resolved$IsGeographic) ||
      !grepl("CS\\[Cartesian,2\\]", resolved$wkt))
    .fg_abort("Choose a two-dimensional projected horizontal CRS, not geographic, geocentric or compound coordinates.")
  unit <- resolved$units_gdal
  if (is.null(unit) || is.na(unit) || !nzchar(unit) || unit == "unknown")
    .fg_abort("The projected CRS must define its horizontal linear unit.")
  if (grepl("DYNAMIC\\[|NATRF2022|PATRF2022|CATRF2022|MATRF2022",wkt,ignore.case=TRUE))
    .fg_abort("This reference frame requires an explicit coordinate-epoch workflow. It can be explored, but is not yet supported for saving an analysis choice. No substitute was selected.")
  if (!is.null(boundary)) {
    if (!(inherits(boundary, "sf") || inherits(boundary, "sfc")) ||
        !length(sf::st_geometry(boundary)) || any(sf::st_is_empty(boundary)) ||
        is.na(sf::st_crs(boundary)) ||
        !all(sf::st_geometry_type(boundary) %in% c("POLYGON", "MULTIPOLYGON")))
      .fg_abort("Save a nonempty Study Area polygon with a known CRS first.")
    projected <- tryCatch(sf::st_transform(boundary, resolved, partial = FALSE,
      allow_ballpark = FALSE), error = function(e) NULL)
    if (is.null(projected) || any(sf::st_is_empty(projected)) ||
        any(!is.finite(sf::st_coordinates(projected)[, 1:2])) ||
        !all(sf::st_is_valid(projected)) || any(as.numeric(sf::st_area(projected)) <= 0))
      .fg_abort("The Study Area cannot be fully transformed to this CRS with the available coordinate operations.")
  }
  list(wkt = wkt, name = resolved$Name, epsg = resolved$epsg, unit = unit)
}

#' Save a validated Study Area analysis CRS
#'
#' Writes the canonical WKT as the horizontal analysis_reference component in a
#' new context revision. Retains the source context, geometry and other references.
#' Existing processing records block changes pending a product migration workflow.
#' @param dsn Existing context GeoPackage containing a Study Area polygon.
#' @param output_file New sibling context GeoPackage.
#' @param crs Analyst-selected projected 2D CRS.
#' @param evidence Analyst rationale including local projection suitability.
#' @param analyst Recorder attribution.
#' @param report_purpose Optional report purpose passed to the context writer.
#' @return Context/report paths from record_study_analysis_reference.
#' @export
set_study_analysis_crs <- function(dsn, output_file, crs, evidence, analyst,
    report_purpose = "definition") {
  context <- read_study_context(dsn)
  if (!inherits(context$study_area, "sf")) .fg_abort("Save the Study Area boundary before choosing the analysis CRS.")
  choice <- validate_study_analysis_crs(crs, context$study_area)
  if (!is.null(context$terrain_processing) && NROW(context$terrain_processing) > 0L)
    .fg_abort("This study has terrain processing records. Changing its analysis CRS requires a product migration workflow.")
  record_study_analysis_reference(dsn, output_file, component = "horizontal",
    value = choice$wkt, basis = "PROJECT_RECORD", evidence = evidence,
    analyst = analyst, report_purpose = report_purpose)
}
