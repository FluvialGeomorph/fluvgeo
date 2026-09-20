#' Discover locally installed projected EPSG systems for a Study Area
#'
#' Reads the installed sf PROJ database through GDAL without network access.
#' Area-of-use bounding boxes are a coarse coverage screen, not a distortion or
#' transformation-accuracy certification. No CRS is automatically selected.
#' @param boundary A nonempty CRS-defined sf or sfc polygon Study Area.
#' @return List with candidates (one row per EPSG code), geographic bounds,
#'   explorer_url, catalog metadata and software versions. Candidates include
#'   coverage, datum, unit, method, scope, frame epoch and an NSRS2022 flag.
#' @export
study_crs_candidates <- function(boundary) {
  if (!(inherits(boundary,"sf") || inherits(boundary,"sfc")) ||
      !length(sf::st_geometry(boundary)) || is.na(sf::st_crs(boundary)) ||
      any(sf::st_is_empty(boundary)) || !all(sf::st_is_valid(boundary)) ||
      !all(sf::st_geometry_type(boundary) %in% c("POLYGON","MULTIPOLYGON")))
    .fg_abort("Save a valid Study Area polygon with a known CRS first.")
  bounds <- tryCatch(sf::st_transform(sf::st_bbox(boundary), "OGC:CRS84", densify=101),
    error=function(e) NULL)
  if (is.null(bounds) || any(!is.finite(bounds)) || bounds["xmax"]-bounds["xmin"] > 180)
    .fg_abort("This Study Area needs explicit dateline or global-extent CRS review; automatic recommendations are unavailable.")
  paths <- strsplit(sf::sf_proj_info(type="path"), .Platform$path.sep, fixed=TRUE)[[1]]
  paths <- file.path(paths,"proj.db")
  paths <- paths[file.exists(paths)]
  if (!length(paths)) .fg_abort("The local PROJ catalog could not be found. Use the reference links or an advanced definition.")
  db <- paths[[1]]
  read <- function(sql) {
    x <- tryCatch(sf::st_read(db,query=sql,quiet=TRUE,stringsAsFactors=FALSE),error=function(e) NULL)
    if (is.null(x)) .fg_abort("This PROJ catalog layout is not supported by the CRS picker.")
    for (k in names(x)) if (is.character(x[[k]])) Encoding(x[[k]]) <- "UTF-8"
    x
  }
  metadata <- read("SELECT key, value FROM metadata")
  if (!identical(metadata$value[metadata$key=="DATABASE.LAYOUT.VERSION.MAJOR"],"1"))
    .fg_abort("This PROJ catalog layout is not supported by the CRS picker.")
  x <- read(paste(
    "SELECT p.code, p.name, COALESCE(d.name,g.name) AS datum, d.frame_reference_epoch AS frame_epoch,",
    "m.name AS unit, m.conv_factor AS metres_per_unit, c.method_name AS method,",
    "e.description AS area, s.scope, e.west_lon AS west, e.south_lat AS south,",
    "e.east_lon AS east, e.north_lat AS north FROM projected_crs p",
    "JOIN coordinate_system cs ON cs.auth_name=p.coordinate_system_auth_name AND cs.code=p.coordinate_system_code",
    "JOIN axis a ON a.coordinate_system_auth_name=cs.auth_name AND a.coordinate_system_code=cs.code AND a.coordinate_system_order=1",
    "JOIN unit_of_measure m ON m.auth_name=a.uom_auth_name AND m.code=a.uom_code",
    "JOIN geodetic_crs g ON g.auth_name=p.geodetic_crs_auth_name AND g.code=p.geodetic_crs_code",
    "LEFT JOIN geodetic_datum d ON d.auth_name=g.datum_auth_name AND d.code=g.datum_code",
    "JOIN usage u ON u.object_table_name='projected_crs' AND u.object_auth_name=p.auth_name AND u.object_code=p.code",
    "JOIN extent e ON e.auth_name=u.extent_auth_name AND e.code=u.extent_code",
    "JOIN scope s ON s.auth_name=u.scope_auth_name AND s.code=u.scope_code",
    "LEFT JOIN conversion c ON c.auth_name=p.conversion_auth_name AND c.code=p.conversion_code",
    "WHERE p.auth_name='EPSG' AND p.deprecated=0 AND cs.type='Cartesian' AND cs.dimension=2 AND m.type='length'"))
  x$code <- as.character(x$code)
  x <- .fg_crs_area_filter(x,bounds)
  x$nsrs2022 <- grepl("NATRF2022|PATRF2022|CATRF2022|MATRF2022|SPCS2022",paste(x$name,x$datum),ignore.case=TRUE)
  x$epoch_required <- !is.na(x$frame_epoch) | x$nsrs2022
  x$url <- paste0("https://spatialreference.org/ref/epsg/",x$code,"/")
  lonlat <- paste(formatC(as.numeric(bounds[c("ymax","xmax","ymin","xmin")]),format="f",digits=6,decimal.mark="."),collapse=",")
  list(candidates=x,bounds=bounds,
    explorer_url=paste0("https://spatialreference.org/explorer.html?authorities=EPSG&activeTypes=PROJECTED_CRS&allowDeprecated=false&latlng=",lonlat),
    metadata=metadata,software=sf::sf_extSoftVersion())
}

.fg_crs_area_filter <- function(x,bounds) {
  w <- unname(bounds["xmin"]); e <- unname(bounds["xmax"])
  s <- unname(bounds["ymin"]); n <- unname(bounds["ymax"])
  wrap <- x$west > x$east
  intersects <- x$south <= n & x$north >= s &
    ifelse(wrap, e >= x$west | w <= x$east, x$west <= e & x$east >= w)
  contains <- x$south <= s & x$north >= n &
    ifelse(wrap, w >= x$west | e <= x$east, x$west <= w & x$east >= e)
  x$coverage <- ifelse(contains,"Full bounds","Partial overlap")
  # Global visualization projections are not engineering recommendations.
  x <- x[which(intersects & x$code != "3857"),,drop=FALSE]
  span <- ifelse(x$west > x$east,360-x$west+x$east,x$east-x$west)
  x <- x[order(x$coverage != "Full bounds",span*(x$north-x$south),x$name,x$code),,drop=FALSE]
  x <- x[!duplicated(x$code),,drop=FALSE]
  rownames(x) <- NULL
  x
}
