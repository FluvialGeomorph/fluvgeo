#' Review locally available coordinate operation candidates
#'
#' @param source_crs,target_crs Full source and target CRS definitions.
#' @param aoi Longitude/latitude bounds: west, south, east, north.
#' @param source_epoch,target_epoch Known coordinate epochs, or NULL when unknown.
#' @return A serializable catalog with exact pipelines, grid evidence, reference
#'   definitions and software/database identity. No operation is selected or run.
#' @details Uses PROJ candidate discovery with strict area containment and local
#'   grid availability. Missing grids, ballpark operations and epoch-dependent
#'   operations are retained with explanations but are not selectable. Available
#'   candidates are plans, not qualification of DEM execution. Complete combined
#'   horizontal/vertical pipelines are retained. No resources are downloaded.
#' @export
terrain_transform_candidates <- function(source_crs,target_crs,aoi,
                                         source_epoch=NULL,target_epoch=NULL) {
  source <- sf::st_crs(source_crs); target <- sf::st_crs(target_crs)
  if(is.na(source) || is.na(target)) stop("Resolve both source and target references first.")
  if(!is.numeric(aoi) || length(aoi)!=4L || any(!is.finite(aoi)) ||
     aoi[1]< -180 || aoi[3]>180 || aoi[2]< -90 || aoi[4]>90 ||
     aoi[1]>=aoi[3] || aoi[2]>=aoi[4]) stop("Supply west, south, east, north search bounds.")
  for(epoch in list(source_epoch,target_epoch))
    if(!is.null(epoch) && (!is.numeric(epoch) || length(epoch)!=1L || !is.finite(epoch)))
      stop("Coordinate epochs must be finite decimal years or NULL.")
  # The caller runs discovery in a worker. Restore process settings even on error.
  network <- sf::sf_proj_network()
  on.exit(sf::sf_proj_network(network),add=TRUE)
  sf::sf_proj_network(FALSE)
  paths <- sf::sf_proj_search_paths()
  databases <- file.path(paths,"proj.db")
  databases <- databases[file.exists(databases)]
  database <- lapply(databases,function(p) list(file=basename(p),sha256=.fg_file_sha256(p)))
  p <- sf::sf_proj_pipelines(source,target,AOI=aoi,grid_availability="USED",
    strict_containment=TRUE,axis_order_authority_compliant=FALSE)
  grid_lists <- attr(p,"grids")
  dynamic <- grepl("DYNAMIC\\[|FRAMEEPOCH\\[",paste(source$wkt,target$wkt))
  candidates <- lapply(seq_len(if(is.null(p)) 0L else nrow(p)),function(i) {
    row <- as.list(as.data.frame(p)[i,,drop=FALSE])
    grids <- lapply(grid_lists[[i]],function(g) {
      possible <- unique(c(g$out_full_name,file.path(paths,g$out_short_name)))
      possible <- possible[nzchar(possible) & file.exists(possible) & !dir.exists(possible)]
      g$sha256 <- if(length(possible)) .fg_file_sha256(possible[1L]) else NULL
      g$locally_verified <- isTRUE(g$out_available==1L) && !is.null(g$sha256)
      g
    })
    reasons <- character()
    if(!isTRUE(row$instantiable)) reasons <- c(reasons,"Required operation resources are unavailable.")
    if(any(!vapply(grids,`[[`,logical(1),"locally_verified")))
      reasons <- c(reasons,"Required grids are missing or cannot be identified locally.")
    if(grepl("ballpark",row$description,ignore.case=TRUE))
      reasons <- c(reasons,"Ballpark fallback; not offered for selection.")
    if(dynamic || grepl("\\+t_epoch=|\\+proj=deformation",row$definition))
      reasons <- c(reasons,"Epoch-dependent operation requires a qualified epoch workflow.")
    row$grids <- grids
    row$selectable <- !length(reasons)
    row$reason <- paste(unique(reasons),collapse=" ")
    row$key <- .fg_transform_fingerprint(list(source$wkt,target$wkt,row$definition,grids))
    row
  })
  evidence <- list(schema="FLUVGEO_TRANSFORM_CANDIDATES_1",
    source=list(name=source$Name,wkt=source$wkt,epoch=source_epoch),
    target=list(name=target$Name,wkt=target$wkt,epoch=target_epoch),
    aoi=as.numeric(aoi),area_policy="Operation area contains the entire search bounding box",
    axis_order="x/y; longitude/latitude",network=FALSE,
    software=list(sf=as.character(utils::packageVersion("sf")),
      geospatial=as.list(sf::sf_extSoftVersion()),databases=database),candidates=candidates,
    execution_status="Planning only; selected DEM transformation execution is not integrated")
  evidence$fingerprint <- .fg_transform_fingerprint(evidence)
  evidence
}

.fg_transform_fingerprint <- function(x) unclass(as.character(openssl::sha256(
  charToRaw(as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA))))))

.fg_same_terrain_datum <- function(source,target,vertical=FALSE) {
  a <- sf::st_crs(source);b <- sf::st_crs(target)
  if(isTRUE(a==b)) return(TRUE)
  # Resolve datum identity through the existing local PROJ catalog route. CRS
  # names (e.g. NAD83) are not sufficient to equate distinct realizations.
  if(is.na(a$epsg) || is.na(b$epsg)) return(FALSE)
  paths <- file.path(sf::sf_proj_search_paths(),"proj.db")
  paths <- paths[file.exists(paths)]
  if(!length(paths)) return(FALSE)
  query <- if(vertical) paste0("SELECT code, datum_auth_name AS authority, datum_code AS datum FROM vertical_crs WHERE auth_name='EPSG' AND code IN (",
    a$epsg,",",b$epsg,")") else paste0(
      "SELECT p.code, g.datum_auth_name AS authority, g.datum_code AS datum FROM projected_crs p ",
      "JOIN geodetic_crs g ON g.auth_name=p.geodetic_crs_auth_name AND g.code=p.geodetic_crs_code ",
      "WHERE p.auth_name='EPSG' AND p.code IN (",a$epsg,",",b$epsg,")")
  rows <- sf::st_read(paths[1L],query=query,quiet=TRUE,stringsAsFactors=FALSE)
  if(nrow(rows)!=2L || anyNA(rows[c("authority","datum")])) return(FALSE)
  identical(as.character(rows$authority[1L]),as.character(rows$authority[2L])) &&
    identical(as.character(rows$datum[1L]),as.character(rows$datum[2L]))
}

#' Discover coordinate operation plans for saved source DEM references
#' @param sources Local source GeoTIFF paths, in saved selection order.
#' @param target_horizontal,target_vertical Saved horizontal and vertical WKT.
#' @param area Analysis area as sf/sfc; used only for candidate area screening.
#' @param target_epoch Known target coordinate epoch, or NULL.
#' @return Source bindings and one operation catalog per distinct full source CRS.
#' @details Reads embedded reference declarations, not elevation pixels. Unknown
#'   vertical declarations require resolution before combined operation planning.
#'   Catalog availability does not certify source declarations or DEM execution.
#' @export
review_terrain_transformations <- function(sources,target_horizontal,target_vertical,area,target_epoch=NULL) {
  if(!is.character(sources) || !length(sources) || anyNA(sources)) stop("Supply saved source GeoTIFF paths.")
  sources <- normalizePath(sources,winslash="/",mustWork=TRUE)
  target_name <- gsub('"','""',paste(sf::st_crs(target_horizontal)$Name,
    sf::st_crs(target_vertical)$Name,sep=" + "),fixed=TRUE)
  target <- sf::st_crs(paste0('COMPOUNDCRS["',target_name,'",',
    sf::st_crs(target_horizontal)$wkt,',',sf::st_crs(target_vertical)$wkt,']'))$wkt
  aoi <- as.numeric(sf::st_bbox(sf::st_transform(area,"OGC:CRS84")))
  observations <- lapply(sources,function(path) {
    messages <- character()
    observation <- withCallingHandlers(.fg_vertical_observe(path,internal=TRUE),warning=function(w) {
      messages <<- c(messages,conditionMessage(w));invokeRestart("muffleWarning")
    })
    observation$messages <- messages
    observation
  })
  definitions <- vapply(observations,`[[`,character(1),"wkt")
  groups <- lapply(unique(definitions),function(definition) {
    indices <- which(definitions==definition)
    source <- observations[[indices[1L]]]
    if(is.null(source$vertical_crs)) stop("A source DEM lacks an explicit vertical reference. Resolve its metadata before transformation review.")
    catalog <- terrain_transform_candidates(definition,target,aoi,target_epoch=target_epoch)
    # Projection, cell-grid and unit changes alone are not datum changes.
    h <- Filter(function(x) identical(x$type,"ProjectedCRS"),source$projjson$components)
    source_horizontal <- if(length(h)==1L) as.character(jsonlite::toJSON(h[[1L]],auto_unbox=TRUE,digits=NA,null="null")) else NULL
    same_horizontal <- !is.null(source_horizontal) && .fg_same_terrain_datum(source_horizontal,target_horizontal)
    same_vertical <- .fg_same_terrain_datum(as.character(jsonlite::toJSON(source$vertical_crs,
      auto_unbox=TRUE,digits=NA,null="null")),target_vertical,vertical=TRUE)
    dynamic <- grepl("DYNAMIC\\[|FRAMEEPOCH\\[",paste(definition,target))
    no_datum_change <- same_horizontal && same_vertical && !dynamic
    unit_only <- !is.null(source_horizontal) && isTRUE(sf::st_crs(source_horizontal)==sf::st_crs(target_horizontal)) &&
      identical(as.character(source$vertical_crs$id$code),"5703") &&
      identical(source$vertical_crs$id$authority,"EPSG") &&
      isTRUE(sf::st_crs(target_vertical)==sf::st_crs("EPSG:8228")) &&
      source$band_unit %in% c("m","metre","meter")
    identity <- isTRUE(sf::st_crs(definition)==sf::st_crs(target))
    list(key=.fg_transform_fingerprint(definition),source_indices=indices,catalog=catalog,
      requires_choice=!no_datum_change,
      explanation=if(unit_only) "No datum change. NAVD88 metres to international feet is an exact unit conversion (metres / 0.3048)." else
        if(identity && !dynamic) "Source and target references match; no coordinate or datum change is required." else
          if(no_datum_change) "Horizontal and vertical datums match. Projection, raster-grid and unit changes do not require a datum transformation choice." else
          "Choose the complete source-to-target operation. Its description includes horizontal and vertical steps.")
  })
  info <- file.info(sources)
  result <- list(schema="FLUVGEO_TERRAIN_TRANSFORM_REVIEW_1",
    sources=data.frame(path=sources,bytes=info$size,modified=as.numeric(info$mtime)),
    observations=observations,groups=groups,target=target,aoi=aoi)
  result$fingerprint <- .fg_transform_fingerprint(result)
  result
}
