#' Validate a Study Area vertical target specification
#'
#' Resolves definitions through sf/PROJ without transforming coordinates or raster
#' values. Unknown, local and unresolved declarations are retained as specifications,
#' never certified as compatible with source data or qualified for processing.
#' @param kind vertical_crs, ellipsoidal, declared, local, or unknown.
#' @param definition EPSG/WKT for resolved kinds; name for declared/local kinds.
#' @param height_type orthometric, normal, tidal, ellipsoidal, local, other, or unknown.
#' @param elevation_unit metre, international_foot, us_survey_foot, or unknown.
#' @param epoch_status known, unknown, or not_applicable.
#' @param coordinate_epoch Decimal year when known; otherwise NA_real_.
#' @param epoch_evidence Source establishing the coordinate epoch, required when known.
#' @param model_name,model_version,model_reference Optional intended model metadata.
#' @return One-row data frame suitable for the schema-7 vertical_reference table.
#' @export
validate_study_vertical_reference <- function(kind = "unknown", definition = "",
    height_type = "unknown", elevation_unit = "unknown", epoch_status = "unknown",
    coordinate_epoch = NA_real_, epoch_evidence = "", model_name = "",
    model_version = "", model_reference = "") {
  kind <- .fg_choice(kind,c("vertical_crs","ellipsoidal","declared","local","unknown"),"kind")
  height_type <- .fg_choice(height_type,c("orthometric","normal","tidal","ellipsoidal","local","other","unknown"),"height_type")
  elevation_unit <- .fg_choice(elevation_unit,c("metre","international_foot","us_survey_foot","unknown"),"elevation_unit")
  epoch_status <- .fg_choice(epoch_status,c("known","unknown","not_applicable"),"epoch_status")
  clean <- function(x,nm) {
    if (!is.character(x) || length(x)!=1L || is.na(x)) .fg_abort(paste(nm,"must be text."))
    trimws(x)
  }
  definition <- clean(definition,"definition")
  epoch_evidence <- clean(epoch_evidence,"epoch_evidence")
  model_name <- clean(model_name,"model_name"); model_version <- clean(model_version,"model_version")
  model_reference <- clean(model_reference,"model_reference")
  if (!is.numeric(coordinate_epoch) || length(coordinate_epoch)!=1L)
    .fg_abort("Coordinate epoch must be one decimal year or NA.")
  coordinate_epoch <- as.double(coordinate_epoch)
  if (epoch_status=="known") {
    if (!is.finite(coordinate_epoch) || coordinate_epoch<=0 || coordinate_epoch>=10000 || !nzchar(epoch_evidence))
      .fg_abort("A known coordinate epoch requires a valid decimal year and its source evidence.")
  } else if (!is.na(coordinate_epoch) || nzchar(epoch_evidence))
    .fg_abort("An unknown or inapplicable epoch cannot carry a numeric epoch or epoch evidence.")
  if (!nzchar(model_name) && (nzchar(model_version) || nzchar(model_reference)))
    .fg_abort("Model version/reference requires a model name.")
  wkt <- authority <- ""; frame_epoch <- NA_real_; dynamic <- FALSE
  name <- if (kind=="unknown") "Unknown" else definition
  if (kind!="unknown" && !nzchar(definition)) .fg_abort("Specify the reference definition or name.")
  if (kind=="unknown" && nzchar(definition)) .fg_abort("Unknown reference cannot carry a definition.")
  if (kind %in% c("vertical_crs","ellipsoidal")) {
    crs <- if (grepl("^[0-9]+$",definition)) paste0("EPSG:",definition) else definition
    resolved <- tryCatch(suppressWarnings(sf::st_crs(crs)),error=function(e) NULL)
    if (is.null(resolved) || is.na(resolved)) .fg_abort("Definition could not be resolved. Use a declared reference for unavailable definitions.")
    wkt <- resolved$wkt; Encoding(wkt) <- "UTF-8"
    if (kind=="vertical_crs" && (!startsWith(wkt,"VERTCRS[") || !grepl("CS\\[vertical,1\\]",wkt) || !grepl(",up,",gsub("[[:space:]]","",wkt))))
      .fg_abort("Choose a standalone vertical height CRS with an upward axis, not horizontal, depth or compound coordinates.")
    if (kind=="ellipsoidal" && (!startsWith(wkt,"GEOGCRS[") || !grepl("CS\\[ellipsoidal,3\\]",wkt)))
      .fg_abort("Ellipsoidal heights require a three-dimensional geographic CRS.")
    name <- resolved$Name; Encoding(name) <- "UTF-8"
    if (!is.na(resolved$epsg)) authority <- paste0("EPSG:",resolved$epsg)
    match <- regmatches(wkt,regexpr("FRAMEEPOCH\\[[0-9.]+\\]",wkt))
    if (length(match) && nzchar(match)) frame_epoch <- as.double(gsub("[^0-9.]","",match))
    dynamic <- grepl("DYNAMIC\\[|NATRF2022|NAPGD2022|PATRF2022|CATRF2022|MATRF2022",wkt,ignore.case=TRUE)
  }
  if ((kind=="ellipsoidal" && height_type!="ellipsoidal") ||
      (kind=="local" && height_type!="local") || (kind=="unknown" && height_type!="unknown") ||
      (kind=="vertical_crs" && height_type %in% c("ellipsoidal","local")))
    .fg_abort("Height type is inconsistent with the reference kind.")
  dynamic <- dynamic || grepl("NATRF2022|NAPGD2022|PATRF2022|CATRF2022|MATRF2022",name,ignore.case=TRUE)
  if (dynamic && epoch_status=="not_applicable") .fg_abort("An epoch-dependent frame needs a known or explicitly unknown coordinate epoch.")
  factor <- switch(elevation_unit,metre=1,international_foot=0.3048,us_survey_foot=1200/3937,unknown=NA_real_)
  status <- if (kind %in% c("unknown","declared","local") || elevation_unit=="unknown" || height_type=="unknown")
    "INCOMPLETE_OR_UNQUALIFIED" else if (dynamic) "EPOCH_WORKFLOW_REQUIRED" else "SPECIFIED_NOT_TRANSFORMED"
  data.frame(spec_version="1",kind=kind,reference_name=name,crs_authority=authority,crs_wkt=wkt,
    height_type=height_type,elevation_unit=elevation_unit,unit_to_metre=as.double(factor),
    epoch_status=epoch_status,coordinate_epoch=coordinate_epoch,epoch_evidence=epoch_evidence,
    frame_epoch=frame_epoch,model_name=model_name,model_version=model_version,
    model_reference=model_reference,support_status=status,stringsAsFactors=FALSE)
}

.fg_vertical_reference_fields <- function() c("spec_version","kind","reference_name","crs_authority","crs_wkt",
  "height_type","elevation_unit","unit_to_metre","epoch_status","coordinate_epoch","epoch_evidence",
  "frame_epoch","model_name","model_version","model_reference","support_status")

.fg_vertical_reference_check <- function(x) {
  if (is.null(x)) return(invisible(NULL))
  if (!is.data.frame(x) || inherits(x,"sf") || nrow(x)!=1L || anyDuplicated(names(x)) ||
      !setequal(names(x),.fg_vertical_reference_fields())) .fg_abort("Malformed vertical target specification.")
  numeric_fields <- c("unit_to_metre","coordinate_epoch","frame_epoch")
  for (k in names(x)) if (is.object(x[[k]]) || !is.null(dim(x[[k]])) ||
      typeof(x[[k]]) != if (k %in% numeric_fields) "double" else "character")
    .fg_abort("Vertical specification fields have incorrect types.")
  definition <- if (x$kind %in% c("vertical_crs","ellipsoidal")) x$crs_wkt else if (x$kind=="unknown") "" else x$reference_name
  expected <- validate_study_vertical_reference(x$kind,definition,x$height_type,x$elevation_unit,
    x$epoch_status,x$coordinate_epoch,x$epoch_evidence,x$model_name,x$model_version,x$model_reference)
  # Retain the saved WKT serialization across PROJ formatter upgrades. Parsing
  # above validates its coordinate-space type; do not rewrite it during reading.
  if (!all(vapply(setdiff(names(expected),"crs_wkt"),function(k) identical(x[[k]],expected[[k]]),logical(1))))
    .fg_abort("Vertical specification fields disagree with the validated definition.")
  invisible(NULL)
}

#' Save a vertical target specification without changing terrain
#' @param dsn Existing context GeoPackage containing a Study Area.
#' @param output_file New sibling context GeoPackage.
#' @param specification One-row result of validate_study_vertical_reference().
#' @param report_purpose Optional report purpose passed to the context writer.
#' @return Context/report paths. Writes schema 7 and paired descriptive vertical/unit
#'   choices atomically. Older contexts remain readable; older software must not
#'   rewrite schema 7. Source files, geometry and horizontal choice are preserved.
#' @export
set_study_vertical_reference <- function(dsn,output_file,specification,report_purpose="definition") {
  dsn <- .fg_network_dsn(dsn); output_file <- .fg_network_dsn(output_file)
  if (dirname(dsn)!=dirname(output_file) || file.exists(output_file)) .fg_abort("Use a new sibling context path.")
  .fg_vertical_reference_check(specification)
  if (is.null(specification)) .fg_abort("Supply a vertical specification.")
  before <- .fg_file_sha256(dsn); args <- read_study_context(dsn)
  if (is.null(args$study_area)) .fg_abort("A saved Study Area is required.")
  if (NROW(args$terrain_processing)>0L) .fg_abort("Existing terrain processing requires an explicit product migration before changing the vertical target.")
  if (identical(args$vertical_reference,specification)) .fg_abort("The vertical specification is unchanged.")
  args$vertical_reference <- specification
  old <- args$analysis_reference
  if (!is.null(old)) old <- old[!old$component %in% c("vertical","elevation_unit"),,drop=FALSE]
  rows <- data.frame(component=c("vertical","elevation_unit"),
    value=c(specification$reference_name,specification$elevation_unit),basis="PROJECT_RECORD",
    evidence=paste("Study Area target specification only; source compatibility and coordinate operations are not qualified. No elevations or source declarations changed.",
      "Definition resolved with",paste(names(sf::sf_extSoftVersion()),sf::sf_extSoftVersion(),collapse="; ")),
    analyst="Vertical specification writer (identity not collected)",recorded_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  args$analysis_reference <- rbind(old,rows)
  if (!identical(before,.fg_file_sha256(dsn))) .fg_abort("Context changed while recording the specification.")
  .fg_save_study_revision(args,dsn,output_file,NULL,report_purpose)
}

#' Discover locally installed vertical height CRS candidates
#' @param boundary CRS-defined Study Area polygon.
#' @return Candidate data frame screened by area-of-use overlap, with EPSG code,
#'   name, datum, unit, frame epoch, scope and coarse coverage. No default choice.
#' @export
study_vertical_crs_candidates <- function(boundary) {
  info <- study_crs_candidates(boundary)
  paths <- file.path(strsplit(sf::sf_proj_info(type="path"),.Platform$path.sep,fixed=TRUE)[[1]],"proj.db")
  db <- paths[file.exists(paths)][1]
  x <- tryCatch(sf::st_read(db,quiet=TRUE,query=paste(
    "SELECT v.code,v.name,d.name AS datum,d.frame_reference_epoch AS frame_epoch,m.name AS unit,",
    "s.scope,e.description AS area,e.west_lon AS west,e.east_lon AS east,e.south_lat AS south,e.north_lat AS north",
    "FROM vertical_crs v JOIN vertical_datum d ON d.auth_name=v.datum_auth_name AND d.code=v.datum_code",
    "JOIN axis a ON a.coordinate_system_auth_name=v.coordinate_system_auth_name AND a.coordinate_system_code=v.coordinate_system_code",
    "JOIN unit_of_measure m ON m.auth_name=a.uom_auth_name AND m.code=a.uom_code",
    "JOIN usage u ON u.object_table_name='vertical_crs' AND u.object_auth_name=v.auth_name AND u.object_code=v.code",
    "JOIN extent e ON e.auth_name=u.extent_auth_name AND e.code=u.extent_code",
    "JOIN scope s ON s.auth_name=u.scope_auth_name AND s.code=u.scope_code",
    "WHERE v.auth_name='EPSG' AND v.deprecated=0 AND a.orientation='up' AND m.type='length'")),error=function(e) NULL)
  if (is.null(x)) .fg_abort("The local vertical CRS catalog is unavailable; use an authoritative definition or a declared reference.")
  for (k in names(x)) if (is.character(x[[k]])) Encoding(x[[k]]) <- "UTF-8"
  x$code <- as.character(x$code)
  .fg_crs_area_filter(x,info$bounds)
}
