#' Find reported source DEM files for a Stream and Survey Collection
#' @param stream One valid polygon sf with stream_id. This is the acquisition AOI.
#' @param collection One reviewed Survey Collection sf record.
#' @param max_records Catalog page size (1 to 500), retained under its existing
#'   argument name for compatibility. All pages are fetched; this is not a total limit.
#' @return List with stream, collection, files (sf bounding boxes), outcome,
#'   message, retrieved_at and endpoint. File bounds are not valid-data footprints.
#'   Only supported USGS source-directory links are resolved; no raster download,
#'   CRS assignment, mosaicking, suitability acceptance or Event creation occurs.
#' @export
discover_stream_dem_files <- function(stream, collection, max_records=200L) {
  if(!inherits(stream,"sf") || nrow(stream)!=1L || !"stream_id" %in% names(stream) ||
      is.na(stream$stream_id) || !nzchar(stream$stream_id) || is.na(sf::st_crs(stream)) ||
      !as.character(sf::st_geometry_type(stream)) %in% c("POLYGON","MULTIPOLYGON") ||
      !isTRUE(sf::st_is_valid(stream)) || sf::st_is_empty(stream))
    stop("Choose one saved Stream polygon with identity and CRS.",call.=FALSE)
  if(!inherits(collection,"sf") || nrow(collection)!=1L ||
      !all(c("candidate_key","catalog","raw_metadata","snapshot_id") %in% names(collection)))
    stop("Choose one reviewed Survey Collection.",call.=FALSE)
  if(length(max_records)!=1L || !is.numeric(max_records) || is.na(max_records) ||
      max_records<1 || max_records>500 || max_records!=as.integer(max_records))
    stop("max_records must be an integer from 1 to 500.",call.=FALSE)
  aoi <- sf::st_transform(stream,4326); box <- as.numeric(sf::st_bbox(aoi))
  out <- list(stream=stream,collection=collection,files=.fg_dem_files_empty(),outcome="UNSUPPORTED",
    message="This collection's product links require manual review; automatic file resolution is not supported.",
    retrieved_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
    endpoint="https://tnmaccess.nationalmap.gov/api/v1/products")
  if(!identical(collection$catalog,"USGS 3DEP")) return(out)
  product <- survey_collection_products(list(records=collection))
  source <- product$access_url[product$product=="DEM"]
  prefix <- .fg_dem_source_prefix(source)
  if(is.null(prefix)) return(out)
  dataset <- if(grepl("/OPR/",prefix,fixed=TRUE))
    "Original Product Resolution (OPR) Digital Elevation Model (DEM)" else "Digital Elevation Model (DEM) 1 meter"
  out$outcome <- "FAILED"
  tryCatch({
    offset <- 0; total <- NULL; rows <- list(); seen <- character()
    repeat {
      page <- .fg_dem_products_get(list(datasets=dataset,bbox=paste(box,collapse=","),max=max_records,offset=offset))
      if(length(page$errors) || length(page$total)!=1L || !is.numeric(page$total) ||
          !is.finite(page$total) || page$total<0 || page$total!=floor(page$total) ||
          is.null(page$items) || !is.list(page$items)) stop("Invalid product catalog response.")
      if(!is.null(total) && total!=page$total) stop("Catalog changed during paging; repeat the search.")
      total <- page$total
      if(length(page$items)!=min(total-offset,max_records)) stop("Incomplete product catalog response.")
      ids <- vapply(page$items,function(item) {
        id <- item$sourceId
        if(length(id)!=1L || is.na(id) || !nzchar(id)) stop("Catalog product lacks identity.")
        as.character(id)
      },character(1))
      # Deduplicate stable product identities, including overlap between pages.
      fresh <- !duplicated(ids) & !ids %in% seen
      if(length(ids) && !any(fresh)) stop("Catalog paging made no progress; repeat the search.")
      rows <- c(rows,lapply(page$items[fresh],function(item)
        .fg_dem_file_row(item,prefix,product$dem_pixel_size_m[1],dataset)))
      seen <- c(seen,ids[fresh]);offset <- offset+length(page$items)
      if(offset>=total) break
    }
    rows <- Filter(Negate(is.null),rows)
    files <- if(length(rows)) do.call(rbind,rows) else .fg_dem_files_empty()
    if(nrow(files)) {
      if(anyDuplicated(files$file_id) || anyDuplicated(files$download_url)) stop("Duplicate product identity.")
      files <- files[lengths(sf::st_intersects(files,aoi))>0L,]
    }
    out$files <- files
    out$outcome <- "COMPLETE"
    out$message <- if(!nrow(files)) "No DEM tiles from this collection intersect the selected Stream in the queried catalog. The collection may cover other parts of the Study Area." else
        "Source-directory matched; file bounding boxes intersect the Stream. Raster coverage, resolution and suitability still require verification."
  },error=function(e) {out$outcome <<- "FAILED"; out$files <<- .fg_dem_files_empty(); out$message <<- paste("File query failed; not evidence of absence:",conditionMessage(e))})
  out
}

.fg_dem_source_prefix <- function(url) {
  if(length(url)!=1L || is.na(url) || !grepl("^https://prd-tnm[.]s3[.]amazonaws[.]com/index[.]html[?]prefix=",url)) return(NULL)
  prefix <- utils::URLdecode(sub("&.*$","",sub("^.*[?]prefix=","",url)))
  if(!grepl("^StagedProducts/Elevation/(OPR|1m)/Projects/[^/]+/",prefix) ||
      grepl("[?\\\\#]",prefix) || any(strsplit(prefix,"/",fixed=TRUE)[[1]] %in% c(".",".."))) return(NULL)
  paste0("https://prd-tnm.s3.amazonaws.com/",sub("/+$","",prefix),"/")
}

.fg_dem_products_get <- function(query) {
  request <- do.call(httr2::req_url_query,c(list(httr2::request("https://tnmaccess.nationalmap.gov/api/v1/products")),query))
  httr2::resp_body_json(httr2::req_perform(httr2::req_timeout(request,45)),simplifyVector=FALSE)
}

.fg_dem_files_empty <- function() {
  sf::st_sf(file_id=character(),title=character(),download_url=character(),metadata_url=character(),
    size_bytes=numeric(),pixel_size_m=numeric(),resolution_evidence=character(),format=character(),
    publication_date=character(),raw_metadata=character(),geometry=sf::st_sfc(crs=4326))
}

.fg_dem_file_row <- function(item,prefix,pixel,dataset) {
  field <- function(name) {
    v <- item[[name]]
    if(is.null(v) || length(v)!=1L || is.na(v)) "Unknown" else as.character(v)
  }
  url <- field("downloadURL")
  if(!startsWith(url,prefix)) return(NULL)
  b <- item$boundingBox
  coords <- suppressWarnings(as.numeric(unlist(b[c("minX","minY","maxX","maxY")])))
  if(length(coords)!=4L || any(!is.finite(coords)) || coords[1]>=coords[3] || coords[2]>=coords[4] ||
      coords[1]< -180 || coords[3]>180 || coords[2]< -90 || coords[4]>90) stop("Matched file has invalid bounds.")
  id <- field("sourceId"); if(id=="Unknown" || !nzchar(id)) stop("Matched file lacks identity.")
  size <- suppressWarnings(as.numeric(field("sizeInBytes")))
  if(!is.finite(size) || size<0) size <- NA_real_
  resolution <- if(is.na(pixel)) "Unknown; inspect file metadata" else "Collection-reported; verify raster"
  if(identical(dataset,"Digital Elevation Model (DEM) 1 meter")) {
    pixel <- 1; resolution <- "1 m product series; verify raster"
  }
  sf::st_sf(file_id=id,title=field("title"),download_url=url,metadata_url=field("metaUrl"),size_bytes=size,
    pixel_size_m=pixel,resolution_evidence=resolution,format=field("format"),publication_date=field("publicationDate"),
    raw_metadata=as.character(jsonlite::toJSON(item,auto_unbox=TRUE,null="null",digits=NA)),
    geometry=sf::st_as_sfc(sf::st_bbox(stats::setNames(coords,c("xmin","ymin","xmax","ymax")),crs=4326)))
}
