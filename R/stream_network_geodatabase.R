#' Save local Stream Network relations to a new GeoPackage
#'
#' Writes every supplied relation, including unresolved draft evidence. This
#' first binding supports GEOPACKAGE/CREATE only: existing destinations are never
#' overwritten. A staged sibling file must pass an exact value/geometry round
#' trip before publication by a non-replacing hard link. A filesystem without
#' hard-link support fails safely. File-geodatabase and UPDATE modes are reserved
#' for future bindings, not silently emulated.
#'
#' UUIDs remain text, geometry remains in the native CRS, and a versioned
#' `fluvgeo_network_fields` table records scalar types and column order. UTC
#' timestamps are ISO-8601 text with nine fractional digits, avoiding GDAL
#' timestamp rounding. Readers restore POSIXct/Date/logical types, including
#' empty tables. Unsupported columns fail instead of being silently dropped.
#' Accepted observations are revalidated before writing; draft readiness is not
#' required. This is local persistence, not FGDB enterprise loading.
#'
#' @param relations Named list of Stream Network relations.
#' @param dsn Path to a new local .gpkg file in an existing directory.
#' @param format GEOPACKAGE (implemented) or FILE_GEODATABASE (reserved).
#' @param mode CREATE (implemented) or UPDATE (reserved).
#' @param overwrite Must be FALSE; replacing existing data is not implemented.
#' @param reaches Optional governed Reach-Stream mappings for acceptance checks.
#' @return The normalized destination path, invisibly.
#' @export
write_stream_network_geodatabase <- function(relations, dsn,
    format = c("GEOPACKAGE", "FILE_GEODATABASE"), mode = c("CREATE", "UPDATE"),
    overwrite = FALSE, reaches = NULL) {
  format <- match.arg(format)
  mode <- match.arg(mode)
  if (format != "GEOPACKAGE" || mode != "CREATE" || !identical(overwrite, FALSE)) {
    .fg_abort("Only GEOPACKAGE CREATE with overwrite = FALSE is implemented.")
  }
  dsn <- .fg_network_dsn(dsn)
  if (file.exists(dsn)) .fg_abort("Destination already exists; choose a new GeoPackage path.")
  .fg_check_network_bundle(relations)
  .fg_check_accepted_bundle(relations, reaches)
  fields <- .fg_network_field_manifest(relations)
  stage <- tempfile(".fluvgeo-network-", tmpdir = dirname(dsn), fileext = ".gpkg")
  on.exit(unlink(stage), add = TRUE)
  for (nm in names(relations)) {
    tab <- relations[[nm]]
    spec <- fields[fields$relation == nm, ]
    for (j in seq_len(nrow(spec))) {
      field <- spec$field[j]
      kind <- spec$kind[j]
      if (kind == "datetime") tab[[field]] <- .fg_encode_network_time(tab[[field]])
      if (kind == "date") tab[[field]] <- as.character(tab[[field]])
      if (kind == "logical") tab[[field]] <- as.integer(tab[[field]])
    }
    sf::st_write(tab, stage, layer = nm, driver = "GPKG", append = NA, quiet = TRUE)
  }
  sf::st_write(fields, stage, layer = "fluvgeo_network_fields", driver = "GPKG", append = NA, quiet = TRUE)
  restored <- read_stream_network_geodatabase(stage, validate = FALSE)
  for (nm in names(relations)) {
    a <- relations[[nm]]
    b <- restored[[nm]]
    if (inherits(a, "sf")) {
      if (!isTRUE(sf::st_crs(a) == sf::st_crs(b)) ||
          !identical(sf::st_as_binary(sf::st_geometry(a)), sf::st_as_binary(sf::st_geometry(b)))) {
        .fg_abort(paste("Geometry round-trip mismatch in", nm))
      }
      a <- sf::st_drop_geometry(a)
      b <- sf::st_drop_geometry(b)
    }
    same <- identical(names(a), names(b)) && nrow(a) == nrow(b) && all(vapply(names(a), function(k) {
      av <- a[[k]]; bv <- b[[k]]
      if (inherits(av, c("POSIXt", "Date"))) return(identical(as.numeric(av), as.numeric(bv)))
      # R vector element names are not fields or persistent row identities.
      identical(unname(av), unname(bv))
    }, logical(1)))
    if (!same) .fg_abort(paste("Attribute round-trip mismatch in", nm,
      paste(names(a)[!vapply(names(a), function(k) identical(a[[k]], b[[k]]), logical(1))], collapse = ", ")))
  }
  # Unlike a replacing rename/copy, this atomically fails if another writer has
  # claimed the destination. The closed staged file and target share a volume.
  if (!isTRUE(suppressWarnings(file.link(stage, dsn)))) {
    .fg_abort("Could not publish without replacement; destination exists or filesystem lacks hard-link support.")
  }
  invisible(dsn)
}

#' Read a locally saved Stream Network GeoPackage
#'
#' Reads this package's versioned GeoPackage binding, not arbitrary legacy GIS
#' data. Container integrity is always checked. With validate = TRUE, current
#' WORKING checks (draft) or ACCEPTANCE checks (accepted) are returned in the
#' `validation` attribute, without overwriting persisted history. Invalid accepted
#' state raises an error. Draft findings do not prevent inspection/recovery.
#'
#' @param dsn Local .gpkg file produced by write_stream_network_geodatabase().
#' @param validate Whether to additionally rerun scientific validation.
#' @param reaches Optional governed Reach-Stream mappings for validation.
#' @return Named relation list; timestamps restored in UTC and nonspatial tables
#'   returned as tibbles. validate = FALSE is for inspection, not acceptance.
#' @export
read_stream_network_geodatabase <- function(dsn, validate = TRUE, reaches = NULL) {
  dsn <- .fg_network_dsn(dsn)
  if (!file.exists(dsn) || dir.exists(dsn)) .fg_abort("GeoPackage file does not exist.")
  if (!is.logical(validate) || length(validate) != 1L || is.na(validate)) .fg_abort("validate must be TRUE or FALSE.")
  layers <- sf::st_layers(dsn)$name
  if (!"fluvgeo_network_fields" %in% layers) .fg_abort("Missing fluvgeo Stream Network binding manifest.")
  fields <- sf::st_read(dsn, layer = "fluvgeo_network_fields", quiet = TRUE)
  .fg_require_table(fields, c("binding_version", "relation", "field", "position", "kind", "geometry_type"), "binding manifest")
  if (!nrow(fields) || anyNA(fields) || any(fields$binding_version != "FLUVGEO_NETWORK_GPKG_1") ||
      any(!fields$relation %in% .fg_network_relation_names()) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", fields$field)) ||
      anyDuplicated(fields[c("relation", "field")]) ||
      any(!fields$kind %in% c("character", "integer", "double", "logical", "date", "datetime", "geometry")) ||
      any(!fields$geometry_type %in% c("NONE", "POINT", "LINESTRING")) ||
      any((fields$kind == "geometry") != (fields$geometry_type != "NONE")) ||
      !setequal(layers, c(unique(fields$relation), "fluvgeo_network_fields"))) {
    .fg_abort("Unsupported or inconsistent Stream Network binding manifest.")
  }
  out <- list()
  for (nm in unique(fields$relation)) {
    spec <- fields[fields$relation == nm, ]
    if (!identical(as.integer(spec$position), seq_len(nrow(spec)))) .fg_abort("Invalid manifest field order.")
    tab <- sf::st_read(dsn, layer = nm, quiet = TRUE, stringsAsFactors = FALSE)
    geom <- spec$field[spec$kind == "geometry"]
    if (length(geom) > 1L || inherits(tab, "sf") != (length(geom) == 1L)) .fg_abort("Manifest geometry mismatch.")
    if (length(geom)) {
      old <- attr(tab, "sf_column")
      names(tab)[names(tab) == old] <- geom
      sf::st_geometry(tab) <- geom
      geometry_type <- spec$geometry_type[spec$kind == "geometry"]
      if (!nrow(tab)) {
        prototype <- if (geometry_type == "POINT") sf::st_point() else sf::st_linestring()
        sf::st_geometry(tab) <- sf::st_sfc(prototype, crs = sf::st_crs(tab))[0]
      } else if (any(sf::st_geometry_type(tab) != geometry_type)) .fg_abort("Stored geometry type disagrees with manifest.")
    }
    if (!setequal(names(tab), spec$field)) .fg_abort(paste("Manifest field mismatch in", nm))
    tab <- tab[, spec$field, drop = FALSE]
    for (j in seq_len(nrow(spec))) {
      field <- spec$field[j]; kind <- spec$kind[j]; value <- tab[[field]]
      if (kind %in% c("datetime", "date")) {
        parsed <- if (kind == "datetime") as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC") else as.Date(value)
        encoded <- if (kind == "datetime") .fg_encode_network_time(parsed) else as.character(parsed)
        if (!identical(as.character(value), encoded)) .fg_abort(paste("Invalid", kind, "values in", nm, field))
        tab[[field]] <- parsed
      } else if (kind == "logical") {
        if (any(!is.na(value) & !value %in% c(0L, 1L))) .fg_abort("Invalid stored logical value.")
        tab[[field]] <- as.logical(value)
      } else if (kind == "integer") {
        converted <- suppressWarnings(as.integer(value))
        if (!is.numeric(value) || !isTRUE(all.equal(as.numeric(value), as.numeric(converted), tolerance = 0))) .fg_abort("Invalid stored integer value.")
        tab[[field]] <- converted
      } else if (kind == "double") {
        if (!is.numeric(value)) .fg_abort("Invalid stored double value.")
        tab[[field]] <- as.double(value)
      } else if (kind == "character" && !is.character(value)) .fg_abort("Invalid stored text value.")
    }
    out[[nm]] <- if (inherits(tab, "sf")) tab else tibble::as_tibble(tab)
  }
  .fg_check_network_bundle(out)
  if (validate) {
    checked <- .fg_check_accepted_bundle(out, reaches)
    if (is.null(checked)) checked <- .fg_validate_network_bundle(out, "WORKING", "local-geodatabase-reader", reaches)
    attr(out, "validation") <- checked
  }
  out
}

.fg_network_dsn <- function(dsn) {
  dsn <- .fg_required_text(dsn, "dsn")
  if (!grepl("\\.gpkg$", dsn, ignore.case = TRUE) || !dir.exists(dirname(dsn))) {
    .fg_abort("dsn must be a local .gpkg path in an existing directory.")
  }
  file.path(normalizePath(dirname(dsn), winslash = "/", mustWork = TRUE), basename(dsn))
}

.fg_encode_network_time <- function(x) {
  if (!length(x)) return(character())
  seconds <- as.numeric(x)
  base <- as.POSIXct(floor(seconds), origin = "1970-01-01", tz = "UTC")
  fraction <- substring(sprintf("%.9f", seconds - floor(seconds)), 3L)
  out <- paste0(format(base, "%Y-%m-%dT%H:%M:%S", tz = "UTC"), ".", fraction, "Z")
  out[is.na(x)] <- NA_character_
  out
}

.fg_network_field_manifest <- function(x) {
  rows <- list()
  for (nm in names(x)) {
    tab <- x[[nm]]
    if (!ncol(tab) || anyDuplicated(tolower(names(tab))) ||
        any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", names(tab))) || any(tolower(names(tab)) %in% c("fid", "geom"))) {
      .fg_abort(paste("Unsupported or reserved field names in", nm))
    }
    kinds <- vapply(tab, function(v) {
      if (inherits(v, "sfc")) {
        if (!class(v)[1] %in% c("sfc_POINT", "sfc_LINESTRING") || is.na(sf::st_crs(v)) ||
            !all(vapply(v, inherits, logical(1), "XY"))) .fg_abort("Only CRS-defined XY POINT/LINESTRING geometry is supported.")
        return("geometry")
      }
      if (inherits(v, c("POSIXct", "Date"))) {
        if (any(is.infinite(as.numeric(v)) | is.nan(as.numeric(v)))) .fg_abort("Nonfinite timestamps/dates cannot be persisted.")
        return(if (inherits(v, "POSIXct")) "datetime" else "date")
      }
      if (is.object(v) || !is.null(dim(v)) || !typeof(v) %in% c("character", "integer", "double", "logical")) {
        .fg_abort("Unsupported field type; supply plain scalar columns, Date, POSIXct, or sf geometry.")
      }
      if (is.double(v) && any(is.infinite(v) | is.nan(v))) .fg_abort("Nonfinite numeric values other than NA cannot be persisted.")
      typeof(v)
    }, character(1))
    rows[[nm]] <- data.frame(binding_version = "FLUVGEO_NETWORK_GPKG_1", relation = nm,
      field = names(tab), position = seq_along(tab), kind = kinds,
      geometry_type = vapply(tab, function(v) if (inherits(v, "sfc")) sub("^sfc_", "", class(v)[1]) else "NONE", character(1)))
  }
  dplyr::bind_rows(rows)
}

.fg_check_accepted_bundle <- function(x, reaches) {
  obs <- x$stream_network_observation
  if (!isTRUE(obs$review_status == "ACCEPTED")) return(NULL)
  if (!inherits(obs$reviewed_at, "POSIXt") || length(obs$reviewed_at) != 1L || anyNA(obs$reviewed_at) ||
      !is.character(obs$reviewed_by) || length(obs$reviewed_by) != 1L || anyNA(obs$reviewed_by) ||
      !nzchar(trimws(obs$reviewed_by)) || !all(x$stream_network$review_status %in% "ACCEPTED")) {
    .fg_abort("Accepted observation requires reviewer/time and accepted segment states.")
  }
  runs <- x$stream_network_validation_run
  .fg_require_table(runs, c("validation_level", "result", "validated_at", "validated_by"), "validation history")
  if (!inherits(runs$validated_at, "POSIXt") ||
      !any(runs$validation_level == "ACCEPTANCE" & runs$result == "PASS" &
             runs$validated_at == obs$reviewed_at & runs$validated_by == obs$reviewed_by, na.rm = TRUE) ||
      any(c(x$stream_network_review$decision_at, x$stream_network$modified_at,
            x$stream_network_operation$performed_at, obs$modified_at) > obs$reviewed_at, na.rm = TRUE)) {
    .fg_abort("Accepted state requires its recorded acceptance run and no later modifications or inspections.")
  }
  checked <- .fg_validate_network_bundle(x, "ACCEPTANCE", "local-geodatabase-validator", reaches)
  if (checked$stream_network_validation_run$result != "PASS") {
    .fg_abort(paste("Stored acceptance is no longer valid:",
      paste(unique(checked$stream_network_validation_issue$issue_code), collapse = ", ")))
  }
  checked
}
