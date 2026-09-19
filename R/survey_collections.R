#' Discover reported lidar Survey Collections for a Study Area
#'
#' Queries the USGS 3DEP published lidar index and NOAA USIEI topographic lidar
#' inventory. A catalog record is not a unique acquisition, acquired data, or a
#' Reach Survey Event. Status/date/link labels are provider reports, not verified
#' availability. No downloads, cross-catalog identity inference or repairs occur.
#' @param study_area One valid polygon sf with study_area_id.
#' @param max_records Maximum records per catalog (1 to 500). Truncation is PARTIAL.
#' @return List with study_area, records (sf), and searches (query evidence).
#' @export
discover_survey_collections <- function(study_area, max_records = 200L) {
  .fg_survey_boundary(study_area)
  if (length(max_records) != 1L || !is.numeric(max_records) || is.na(max_records) ||
      max_records < 1 || max_records > 500 || max_records != as.integer(max_records))
    stop("max_records must be an integer from 1 to 500.", call. = FALSE)
  boundary <- sf::st_transform(study_area, 4326)
  bbox <- as.numeric(sf::st_bbox(boundary))
  if (bbox[3]-bbox[1] > 6 || bbox[4]-bbox[2] > 6)
    stop("Search one regional Study Area (at most 6 degrees wide/high).", call. = FALSE)
  services <- c(`USGS 3DEP` = "https://index.nationalmap.gov/arcgis/rest/services/3DEPElevationIndex/MapServer/24",
    USIEI = "https://maps.coast.noaa.gov/arcgis/rest/services/USInteragencyElevationInventory/USIEIv2/MapServer/2")
  records <- list(); searches <- list()
  for (catalog in names(services)) {
    endpoint <- services[[catalog]]
    snapshot <- .fg_generate_uuid(1L)
    retrieved <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    search <- data.frame(catalog=catalog, snapshot_id=snapshot, retrieved_at=retrieved,
      outcome="FAILED", matched=NA_integer_, returned=0L, endpoint=endpoint,
      scope_note="Study Area envelope query, then exact sf intersection. One catalog layer only.",
      message="Request failed; not evidence of no collections.")
    tryCatch({
      ids <- .fg_survey_get(endpoint, list(f="json", where="1=1", returnIdsOnly="true",
        geometry=paste(bbox,collapse=","), geometryType="esriGeometryEnvelope",
        inSR="4326", spatialRel="esriSpatialRelIntersects"))
      if (!"objectIds" %in% names(ids)) stop("Catalog did not return an ID inventory.")
      all_ids <- sort(unique(as.numeric(unlist(ids$objectIds))))
      if (anyNA(all_ids) || any(!is.finite(all_ids))) stop("Invalid catalog identifiers.")
      search$matched <- length(all_ids)
      selected <- utils::head(all_ids, max_records)
      chunks <- split(selected, ceiling(seq_along(selected)/25))
      rows <- lapply(chunks, function(chunk) {
        payload <- .fg_survey_get(endpoint, list(f="geojson", objectIds=paste(chunk,collapse=","),
          outFields="*", outSR="4326", returnGeometry="true"))
        if (!identical(payload$type,"FeatureCollection") ||
            isTRUE(payload$exceededTransferLimit) || length(payload$features) != length(chunk))
          stop("Catalog response was incomplete or changed during the query.")
        shape <- sf::st_read(jsonlite::toJSON(payload,auto_unbox=TRUE,null="null",digits=NA),quiet=TRUE)
        .fg_survey_normalize(shape,catalog,snapshot,retrieved,endpoint)
      })
      x <- if (length(rows)) do.call(rbind,rows) else .fg_survey_empty()
      if (nrow(x)) {
        if (any(!sf::st_is_valid(x)) || any(sf::st_is_empty(x))) stop("Invalid catalog footprint; no repair attempted.")
        x <- x[lengths(sf::st_intersects(x,boundary)) > 0L,]
        x$coverage <- vapply(seq_len(nrow(x)), function(i)
          if (lengths(sf::st_covered_by(boundary,x[i,]))[1] > 0L) "Covers Study Area" else "Intersects Study Area", character(1))
      }
      records[[catalog]] <- x
      search$outcome <- if (length(all_ids) > length(selected)) "PARTIAL" else "COMPLETE"
      search$returned <- nrow(x)
      search$message <- if (search$outcome == "PARTIAL") "Record limit reached; narrow the Study Area or review provider catalog." else
        if (!nrow(x)) "Query succeeded; no intersecting records in this layer." else "Query succeeded; provider reports require review."
    }, error=function(e) { search$message <<- paste("Catalog query failed:",conditionMessage(e)) })
    searches[[catalog]] <- search
  }
  records <- Filter(function(x) nrow(x)>0L,records)
  list(study_area=study_area,records=if(length(records)) do.call(rbind,records) else .fg_survey_empty(),
    searches=do.call(rbind,searches))
}

.fg_survey_get <- function(endpoint, query) {
  request <- httr2::request(paste0(endpoint,"/query"))
  request <- do.call(httr2::req_url_query,c(list(request),query))
  # NOAA's public service rejected encoded outFields=%2A in the live qualification
  # while the equivalent literal wildcard succeeded. Only this fixed token changes.
  request$url <- sub("outFields=%2A", "outFields=*", request$url, fixed=TRUE)
  response <- httr2::req_perform(httr2::req_timeout(request,30))
  value <- httr2::resp_body_json(response,simplifyVector=FALSE)
  if (!is.null(value$error)) stop("Catalog returned a service error.",call.=FALSE)
  value
}

.fg_survey_empty <- function() {
  fields <- c("candidate_key","catalog","record_id","snapshot_id","retrieved_at","title",
    "date_label","status","availability","metadata_url","access_url","raw_metadata","coverage")
  sf::st_sf(stats::setNames(as.data.frame(rep(list(character()),length(fields))),fields),
    geometry=sf::st_sfc(crs=4326))
}

.fg_survey_normalize <- function(x,catalog,snapshot,retrieved,endpoint) {
  field <- function(n) {
    if (!n %in% names(x)) return(rep("Unknown",nrow(x)))
    v <- as.character(x[[n]]); v[is.na(v)|!nzchar(trimws(v))] <- "Unknown"; v
  }
  usgs <- catalog == "USGS 3DEP"
  id <- field(if(usgs) "workunit_id" else "ID")
  if (any(id=="Unknown") || anyDuplicated(id)) stop("Missing/duplicate catalog record identity.")
  date <- if(usgs) {
    epoch <- function(n) {
      v <- suppressWarnings(as.numeric(field(n)))
      out <- as.character(as.Date(as.POSIXct(v/1000,origin="1970-01-01",tz="UTC")))
      out[is.na(out)] <- "Unknown"; out
    }
    paste(epoch("collect_start"),epoch("collect_end"),sep=" to ")
  } else field("collectiondate")
  metadata <- if(usgs) field("metadata_link") else rep(endpoint,nrow(x))
  access <- if(usgs) field("lpc_link") else rep("Unknown",nrow(x))
  if (!usgs && "Links" %in% names(x)) for(i in seq_len(nrow(x))) {
    links <- tryCatch(jsonlite::fromJSON(x$Links[i])$links,error=function(e) NULL)
    if(is.data.frame(links) && all(c("link","linktype") %in% names(links))) {
      valid <- grepl("^https?://",links$link)
      m <- which(valid & grepl("metadata",links$linktype,ignore.case=TRUE))
      a <- which(valid & grepl("download|data access",links$linktype,ignore.case=TRUE))
      if(length(m)) metadata[i] <- links$link[m[1]]
      if(length(a)) access[i] <- links$link[a[1]]
    }
  }
  raw <- sf::st_drop_geometry(x)
  sf::st_sf(candidate_key=paste(catalog,id,sep=":"),catalog=catalog,record_id=id,
    snapshot_id=snapshot,retrieved_at=retrieved,title=field(if(usgs) "workunit" else "Title"),
    date_label=date,status=if(usgs) rep("Published lidar index",nrow(x)) else field("Status"),
    availability=if(usgs) ifelse(grepl("^https?://",access),"Access link reported; not verified","Unknown") else field("productsavailable"),
    metadata_url=metadata,access_url=access,
    raw_metadata=vapply(seq_len(nrow(x)),function(i) as.character(jsonlite::toJSON(raw[i,,drop=FALSE],auto_unbox=TRUE,na="null")),character(1)),
    coverage="Not assessed",geometry=sf::st_geometry(sf::st_transform(x,4326)))
}

.fg_survey_boundary <- function(x) {
  if (!inherits(x,"sf") || nrow(x)!=1L || !"study_area_id" %in% names(x) ||
      is.na(x$study_area_id) || !nzchar(x$study_area_id) || is.na(sf::st_crs(x)) ||
      !as.character(sf::st_geometry_type(x)) %in% c("POLYGON","MULTIPOLYGON") ||
      !isTRUE(sf::st_is_valid(x)) || sf::st_is_empty(x)) stop("Save a valid Study Area boundary first.",call.=FALSE)
}

#' Save Survey Collection selection intent without creating Survey Events
#' @param discovery Output of discover_survey_collections(). May include
#'   acquisition_plan, a data frame of candidate_key and product (DEM or
#'   POINT_CLOUD). Product choices must refer to selected records; they are intent.
#' @param selected Character candidate keys explicitly selected by the analyst.
#'   Empty selection is allowed; the query evidence remains recorded.
#' @param dsn New local GeoPackage path. Existing files are never replaced.
#' @return Path invisibly. Separate immutable snapshot; not an FGDB schema change.
#' @export
write_survey_collection_selection <- function(discovery, selected, dsn) {
  .fg_survey_boundary(discovery$study_area)
  x <- discovery$records; s <- discovery$searches
  if (!inherits(x,"sf") || !all(setdiff(names(.fg_survey_empty()),"geometry") %in% names(x)) ||
      anyNA(x$candidate_key) || anyDuplicated(x$candidate_key) ||
      !is.character(selected) || anyNA(selected) || anyDuplicated(selected) || !all(selected %in% x$candidate_key))
    stop("Select only catalog records from the reviewed discovery.",call.=FALSE)
  if (!is.data.frame(s) || !all(c("catalog","snapshot_id","outcome") %in% names(s)) || !nrow(s) ||
      any(!s$outcome %in% c("COMPLETE","PARTIAL","FAILED"))) stop("Query evidence is required.",call.=FALSE)
  at <- match(paste(x$catalog,x$snapshot_id),paste(s$catalog,s$snapshot_id))
  if (anyNA(at) || any(s$outcome[at]=="FAILED")) stop("Records lack successful query evidence.",call.=FALSE)
  plan <- discovery$acquisition_plan
  if (is.null(plan)) plan <- data.frame(candidate_key=character(),product=character())
  if (!is.data.frame(plan) || !identical(names(plan),c("candidate_key","product")) ||
      anyNA(plan) || anyDuplicated(plan) || !all(plan$candidate_key %in% selected) ||
      !all(plan$product %in% c("DEM","POINT_CLOUD")))
    stop("Acquisition plan must contain unique selected candidate/product pairs (DEM or POINT_CLOUD).",call.=FALSE)
  dsn <- .fg_network_dsn(dsn)
  if(file.exists(dsn)) stop("Selection destination already exists.",call.=FALSE)
  stage <- tempfile("survey-selection-",tmpdir=dirname(dsn),fileext=".gpkg")
  on.exit(unlink(stage),add=TRUE)
  x$selected <- as.integer(x$candidate_key %in% selected)
  sf::st_write(discovery$study_area,stage,layer="study_area",quiet=TRUE)
  sf::st_write(x,stage,layer="survey_collections",quiet=TRUE)
  sf::st_write(s,stage,layer="searches",quiet=TRUE)
  sf::st_write(plan,stage,layer="acquisition_plan",quiet=TRUE)
  sf::st_write(data.frame(schema="SURVEY_COLLECTION_SELECTION_2",saved_at=format(Sys.time(),tz="UTC",usetz=TRUE)),
    stage,layer="selection_metadata",quiet=TRUE)
  check <- read_survey_collection_selection(stage)
  if (!setequal(check$selected,selected) || nrow(check$records)!=nrow(x) ||
      !identical(as.data.frame(check$acquisition_plan),as.data.frame(plan))) stop("Selection round trip failed.")
  if (!isTRUE(suppressWarnings(file.link(stage,dsn)))) stop("Could not publish selection without replacement.")
  invisible(dsn)
}

#' Read a retained Survey Collection selection snapshot
#' @param dsn Existing snapshot GeoPackage.
#' @return Discovery list plus selected candidate keys and acquisition_plan.
#'   Version 1 snapshots return an empty plan. Does not query services.
#' @export
read_survey_collection_selection <- function(dsn) {
  m <- sf::st_read(dsn,layer="selection_metadata",quiet=TRUE)
  if(nrow(m)!=1L || !m$schema %in% c("SURVEY_COLLECTION_SELECTION_1","SURVEY_COLLECTION_SELECTION_2")) stop("Unknown Survey Collection schema.")
  x <- sf::st_read(dsn,layer="survey_collections",quiet=TRUE)
  list(study_area=sf::st_read(dsn,layer="study_area",quiet=TRUE), records=x[,setdiff(names(x),"selected"),drop=FALSE],
    searches=sf::st_read(dsn,layer="searches",quiet=TRUE),selected=x$candidate_key[x$selected==1L],
    acquisition_plan=if(m$schema=="SURVEY_COLLECTION_SELECTION_2") sf::st_read(dsn,layer="acquisition_plan",quiet=TRUE) else
      data.frame(candidate_key=character(),product=character()))
}

#' Review product evidence for discovered Survey Collections
#' @param discovery Output of discover_survey_collections() or its saved snapshot.
#' @return Nonspatial table with two product rows per catalog record. Links are
#'   reported, not tested. General catalog access is not product-specific evidence.
#'   dem_pixel_size_m uses explicit USGS dem_gsd_meters only; missing/nonpositive
#'   values are unknown. A pixel size at most 1 m passes resolution screening only,
#'   not scientific suitability. Point spacing is never treated as raster size.
#' @export
survey_collection_products <- function(discovery) {
  x <- discovery$records
  rows <- lapply(seq_len(nrow(x)),function(i) {
    raw <- tryCatch(jsonlite::fromJSON(x$raw_metadata[i]),error=function(e) NULL)
    value <- function(name) {
      if(!is.list(raw)) return("Unknown")
      v <- raw[[name]]
      if(is.null(v) || length(v)!=1L || is.na(v) || !nzchar(trimws(as.character(v)))) "Unknown" else as.character(v)
    }
    usgs <- identical(x$catalog[i],"USGS 3DEP")
    pixel <- if(usgs) suppressWarnings(as.numeric(value("dem_gsd_meters"))) else NA_real_
    if(!is.finite(pixel) || pixel<=0) pixel <- NA_real_
    screen <- if(is.na(pixel)) "Unknown — verify pixel size" else if(pixel<=1)
      "Meets resolution only; suitability unreviewed" else "Too coarse — acquire point clouds for a suitable DEM"
    link <- if(usgs) c(value("sourcedem_link"),value("lpc_link")) else rep("Unknown",2)
    link[!grepl("^https?://[^[:space:]]+$",link)] <- "Unknown"
    product_rows <- data.frame(candidate_key=x$candidate_key[i],product=c("DEM","POINT_CLOUD"),
      reported_status=if(usgs) c(value("sourcedem_category"),value("lpc_category")) else rep(x$status[i],2),
      reported_products=if(usgs) c(value("sourcedem_reason"),value("lpc_reason")) else rep(value("productsavailable"),2),
      access_url=link,access_evidence=ifelse(link=="Unknown","Product-specific access unresolved","Product link reported; not verified"),
      catalog_access_url=x$access_url[i],snapshot_id=x$snapshot_id[i],stringsAsFactors=FALSE)
    product_rows$dem_pixel_size_m <- c(pixel,NA_real_)
    product_rows$resolution_screen <- c(screen,"Not a raster; DEM production required")
    product_rows
  })
  if(length(rows)) do.call(rbind,rows) else data.frame(candidate_key=character(),product=character(),
    reported_status=character(),reported_products=character(),access_url=character(),access_evidence=character(),
    catalog_access_url=character(),snapshot_id=character(),dem_pixel_size_m=numeric(),resolution_screen=character())
}
