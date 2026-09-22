# Source transport receipts are separate from analytical terrain and Event schemas.
.fg_dem_time <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz="UTC")
.fg_dem_hash <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  unclass(as.character(openssl::sha256(con)))
}
.fg_dem_inside <- function(path, root) {
  root <- as.character(fs::path_real(root))
  existing <- path
  while(!file.exists(existing) && !dir.exists(existing)) existing <- dirname(existing)
  real <- as.character(fs::path_real(existing))
  if(!identical(tolower(real),tolower(root)) &&
      !startsWith(tolower(real),paste0(tolower(root),"/")))
    stop("Download path is outside its storage directory.",call.=FALSE)
  path
}
.fg_dem_json <- function(x, path, immutable=TRUE) {
  .fg_dem_inside(path,dirname(path))
  stage <- tempfile("receipt-",tmpdir=dirname(path)); on.exit(unlink(stage))
  jsonlite::write_json(x,stage,auto_unbox=TRUE,pretty=TRUE,na="null",null="null",digits=NA)
  jsonlite::read_json(stage) # Reject incomplete serialization before publication.
  ok <- if(immutable) suppressWarnings(file.link(stage,path)) else file.copy(stage,path,overwrite=TRUE)
  if(!isTRUE(ok)) stop("Could not publish download evidence.",call.=FALSE)
  invisible(path)
}
.fg_dem_limits <- function(limits) {
  defaults <- list(connect_seconds=30,idle_seconds=120)
  # Read old requests without reintroducing retired size/duration restrictions.
  legacy <- c("file_bytes","attempt_bytes","file_seconds","attempt_seconds")
  if(!is.list(limits) || (length(limits) && (is.null(names(limits)) || anyDuplicated(names(limits)) ||
      !all(names(limits) %in% c(names(defaults),legacy))))) stop("Unknown download limits.")
  limits <- limits[intersect(names(limits),names(defaults))]
  defaults[names(limits)] <- limits
  if(!all(vapply(defaults,function(x) is.numeric(x) && length(x)==1L && is.finite(x) && x>0,logical(1))))
    stop("Download limits must be positive finite numbers.")
  defaults
}
.fg_dem_request <- function(attempt) {
  supplied_root <- as.character(fs::path_real(dirname(dirname(attempt))))
  .fg_dem_inside(attempt,supplied_root)
  attempt <- as.character(fs::path_real(attempt))
  root <- dirname(dirname(attempt))
  if(basename(dirname(attempt))!="attempts" || !grepl("^[0-9a-f]{32}$",basename(attempt)))
    stop("Invalid download attempt directory.")
  for(p in c("request.json","selection.gpkg","receipts","incomplete"))
    .fg_dem_inside(file.path(attempt,p),root)
  m <- jsonlite::read_json(file.path(attempt,"request.json"),simplifyVector=TRUE)
  if(!identical(m$schema,"STREAM_DEM_DOWNLOAD_1") || !identical(m$id,basename(attempt)))
    stop("Unknown download attempt schema.")
  if(!identical(.fg_dem_hash(file.path(attempt,"selection.gpkg")),m$selection_sha256))
    stop("Saved download selection failed its checksum.")
  r <- read_stream_dem_selection(file.path(attempt,"selection.gpkg"))
  list(path=attempt,root=root,manifest=m,selection=r,
    files=r$files[match(r$selected,r$files$file_id),,drop=FALSE])
}

#' Prepare an immutable local source DEM download attempt
#' @param selection_path Saved Stream DEM selection GeoPackage.
#' @param destination Existing or new source-asset directory within an existing
#'   parent directory. The caller chooses and validates the study destination.
#' @param limits Named list overriding connect_seconds (30) and idle_seconds (120).
#'   Positive finite numbers. Legacy file_bytes, attempt_bytes, file_seconds and
#'   attempt_seconds entries are ignored. Healthy transfers have no size/duration cap.
#' @return New attempt directory. No network request is made.
#' @export
prepare_stream_dem_download <- function(selection_path,destination,limits=list()) {
  limits <- .fg_dem_limits(limits)
  r <- read_stream_dem_selection(selection_path)
  if(!r$outcome %in% c("COMPLETE","PARTIAL") || !length(r$selected) ||
      anyNA(r$selected) || anyDuplicated(r$selected) || anyDuplicated(r$files$file_id) ||
      !all(r$selected %in% r$files$file_id)) stop("Save a nonempty valid file selection first.")
  f <- r$files[match(r$selected,r$files$file_id),,drop=FALSE]
  if(!all(c("size_bytes","format","download_url","raw_metadata") %in% names(f)))
    stop("Saved file metadata are incomplete.")
  parent <- as.character(fs::path_real(dirname(destination)))
  .fg_dem_inside(destination,parent)
  if(!dir.exists(destination) && !dir.create(destination)) stop("Cannot create source DEM storage.")
  root <- as.character(fs::path_real(destination))
  for(d in c("assets","attempts")) {
    path <- .fg_dem_inside(file.path(root,d),root)
    if(!dir.exists(path) && !dir.create(path)) stop("Cannot create download directory.")
  }
  id <- paste(format(openssl::rand_bytes(16)),collapse="")
  attempt <- file.path(root,"attempts",id)
  if(!dir.create(attempt)) stop("Cannot create download attempt.")
  for(d in c("receipts","incomplete")) if(!dir.create(file.path(attempt,d))) stop("Cannot stage download attempt.")
  snapshot <- file.path(attempt,"selection.gpkg")
  hash <- .fg_dem_hash(selection_path)
  if(!file.copy(selection_path,snapshot,overwrite=FALSE) || !identical(.fg_dem_hash(snapshot),hash))
    stop("Cannot preserve the exact saved file selection.")
  .fg_dem_json(list(schema="STREAM_DEM_DOWNLOAD_1",id=id,created_at=.fg_dem_time(),
    selection_file=basename(selection_path),selection_sha256=hash,
    context_revision=r$context_revision,stream_id=r$stream$stream_id,
    candidate_key=r$collection$candidate_key,limits=limits),file.path(attempt,"request.json"))
  attempt
}

.fg_dem_source_key <- function(r,f) {
  unclass(as.character(openssl::sha256(serialize(list(collection=sf::st_drop_geometry(r$collection),
    file=sf::st_drop_geometry(f)),NULL,version=2))))
}
.fg_dem_url <- function(r,f) {
  p <- survey_collection_products(list(records=r$collection))
  prefix <- .fg_dem_source_prefix(p$access_url[p$product=="DEM"])
  url <- f$download_url
  if(!identical(r$collection$catalog,"USGS 3DEP") || is.null(prefix) || length(url)!=1L ||
      is.na(url) || !startsWith(url,prefix) || grepl("[%?#\\\\[:space:][:cntrl:]]",url) ||
      any(strsplit(sub("^https://","",url),"/",fixed=TRUE)[[1]] %in% c(".","..","")) ||
      !grepl("\\.tiff?$",url,ignore.case=TRUE) ||
      !tolower(f$format) %in% c("geotiff","tiff","geotiff / bigtiff"))
    stop("Unsupported source URL or format; this adapter downloads direct USGS GeoTIFF objects.")
  url
}

# Stream to disk; progress callbacks also detect cancellation and stalled transfers.
.fg_dem_transfer <- function(url,path,limits,cancelled,progress) {
  con <- file(path,"wb"); on.exit(close(con))
  received <- 0; last_bytes <- 0; last_change <- Sys.time(); reason <- NULL
  h <- curl::new_handle(followlocation=FALSE,connecttimeout_ms=ceiling(1000*limits$connect_seconds),
    timeout_ms=0,noprogress=FALSE,http_content_decoding=FALSE,
    progressfunction=function(down,up) {
      if(down[2]>last_bytes) {last_bytes <<- down[2];last_change <<- Sys.time()}
      reason <<- if(cancelled()) "Download cancelled." else
        if(as.numeric(difftime(Sys.time(),last_change,units="secs"))>limits$idle_seconds) "Download idle timeout." else NULL
      progress(received,if(down[1]>0) down[1] else NA_real_)
      is.null(reason)
    })
  curl::handle_setheaders(h,"Accept-Encoding"="identity")
  transport_warning <- NULL
  response <- tryCatch(withCallingHandlers(curl::curl_fetch_stream(url,function(chunk) {
    if(cancelled()) stop("Download cancelled.")
    writeBin(chunk,con); received <<- received+length(chunk)
  },handle=h),warning=function(w) {
    transport_warning <<- conditionMessage(w);invokeRestart("muffleWarning")
  }),error=function(e) stop(if(!is.null(reason)) reason else if(!is.null(transport_warning))
    transport_warning else conditionMessage(e),call.=FALSE))
  if(!is.null(transport_warning)) stop(transport_warning,call.=FALSE)
  response
}
.fg_dem_headers <- function(raw) {
  text <- rawToChar(raw)
  blocks <- strsplit(text,"\r\n\r\n",fixed=TRUE)[[1]]
  blocks <- blocks[grepl("^HTTP/",blocks)]
  if(!length(blocks)) return(list())
  lines <- strsplit(tail(blocks,1),"\r\n",fixed=TRUE)[[1]][-1]
  out <- list()
  allowed <- c("content-length","content-type","content-encoding","etag","last-modified",
    "x-amz-checksum-sha256","x-amz-checksum-sha1","x-amz-checksum-crc32","x-amz-checksum-crc32c")
  for(line in lines) {
    name <- tolower(sub(":.*$","",line))
    if(name %in% allowed) {
      if(!is.null(out[[name]])) stop("Duplicate verification response header.")
      out[[name]] <- trimws(sub("^[^:]+:","",line))
    }
  }
  out
}
.fg_dem_verify <- function(path,reported,response) {
  if(!identical(as.integer(response$status_code),200L))
    stop(paste("HTTP",response$status_code,"did not return a complete source object; redirects are unsupported."))
  h <- .fg_dem_headers(response$headers)
  if(!is.null(h[["content-encoding"]]) && h[["content-encoding"]]!="identity") stop("Encoded source response is unsupported.")
  n <- file.info(path)$size
  if(!is.finite(n) || n<=0) stop("Downloaded file is empty.")
  length_header <- h[["content-length"]]
  if(!is.null(length_header) && (!grepl("^[0-9]+$",length_header) ||
      !isTRUE(as.numeric(length_header)==n))) stop("HTTP Content-Length verification failed.")
  if(is.finite(reported) && reported>=0 && reported!=n) stop("Catalog size verification failed; refresh source evidence before retrying.")
  con <- file(path,"rb"); signature <- readBin(con,"raw",8); close(con)
  hex <- paste(format(signature),collapse="")
  if(!any(startsWith(hex,c("49492a00","4d4d002a","49492b0008000000","4d4d002b00080000"))) || n<8)
    stop("Source body is not a supported TIFF/BigTIFF signature.")
  list(bytes=n,sha256=.fg_dem_hash(path),headers=h,
    verification=list(http="200",http_length=if(is.null(length_header)) "unknown" else "matched",
      catalog_length=if(is.finite(reported) && reported>=0) "matched" else "unknown",
      signature="TIFF/BigTIFF",suitability="not reviewed"))
}
.fg_dem_receipt_valid <- function(receipt,root,verify=TRUE) {
  tryCatch({
    if(!receipt$outcome %in% c("DOWNLOADED","REUSED") ||
        !grepl("^assets/[0-9a-f]{32}-[0-9]{6}-[0-9a-f]{64}\\.tif$",receipt$asset) ||
        !grepl("^[0-9a-f]{64}$",receipt$sha256)) return(FALSE)
    path <- .fg_dem_inside(file.path(root,receipt$asset),root)
    isTRUE(file.info(path)$size==receipt$bytes) && (!verify || identical(.fg_dem_hash(path),receipt$sha256))
  },error=function(e) FALSE)
}

#' Read local DEM download outcomes
#' @param attempt Attempt directory returned by prepare_stream_dem_download().
#' @param verify Rehash completed assets (default TRUE). FALSE checks existence
#'   and size only and labels successful outcomes RECORDED, not verified downloads.
#' @return List with manifest, files table and transient progress. No network access.
#'   Incomplete attempts are interrupted unless the caller knows their worker is active.
#' @export
read_stream_dem_download <- function(attempt,verify=TRUE) {
  a <- .fg_dem_request(attempt)
  progress <- tryCatch(suppressWarnings(jsonlite::read_json(file.path(attempt,"progress.json"),simplifyVector=TRUE)),error=function(e) NULL)
  cancelled <- file.exists(file.path(attempt,"cancelled.json"))
  rows <- lapply(seq_len(nrow(a$files)),function(i) {
    receipt_path <- .fg_dem_inside(file.path(attempt,"receipts",sprintf("%06d.json",i)),a$root)
    receipt <- tryCatch(suppressWarnings(jsonlite::read_json(receipt_path,simplifyVector=TRUE)),error=function(e) NULL)
    state <- if(!is.null(progress) && identical(as.integer(progress$index),as.integer(i)))
      if(cancelled) "CANCELLED" else "INTERRUPTED" else "NOT_STARTED"
    reason <- ""; bytes <- NA_real_; asset <- NA_character_
    if(!is.null(receipt) && identical(receipt$schema,"STREAM_DEM_RECEIPT_1")) {
      state <- receipt$outcome;reason <- receipt$message;bytes <- receipt$bytes
      if(is.null(bytes)) bytes <- NA_real_
      if(state %in% c("DOWNLOADED","REUSED")) {
        valid <- identical(receipt$selection_sha256,a$manifest$selection_sha256) &&
          identical(receipt$file_id,a$files$file_id[i]) &&
          identical(receipt$source_key,.fg_dem_source_key(a$selection,a$files[i,,drop=FALSE])) &&
          .fg_dem_receipt_valid(receipt,a$root,verify)
        state <- if(!valid) "UNAVAILABLE" else if(!verify) "RECORDED" else state
        if(!valid) reason <- "Receipt or local asset failed integrity verification. Retry explicitly."
        asset <- receipt$asset
      }
    }
    data.frame(file_id=a$files$file_id[i],title=a$files$title[i],outcome=state,
      bytes=bytes,asset=asset,message=reason,stringsAsFactors=FALSE)
  })
  list(manifest=a$manifest,files=do.call(rbind,rows),progress=progress,
    finished=file.exists(file.path(attempt,"finished.json")),cancelled=cancelled)
}

#' Record cancellation after a source DEM worker has stopped
#' @param attempt Attempt directory. The caller must stop and join its worker first.
#' @return Offline outcomes invisibly. Removes only this attempt's incomplete files.
#' @export
cancel_stream_dem_download <- function(attempt) {
  a <- .fg_dem_request(attempt)
  path <- .fg_dem_inside(file.path(attempt,"cancelled.json"),a$root)
  if(!file.exists(path)) .fg_dem_json(list(at=.fg_dem_time()),path)
  incomplete <- .fg_dem_inside(file.path(attempt,"incomplete"),a$root)
  for(p in list.files(incomplete,pattern="^[0-9]{6}\\.part$",full.names=TRUE)) {
    .fg_dem_inside(p,a$root)
    if(unlink(p)!=0L) stop("Worker stopped, but an incomplete file could not be removed.")
  }
  invisible(read_stream_dem_download(attempt,verify=FALSE))
}

#' Execute a prepared source DEM download attempt
#' @param attempt Prepared attempt directory; an attempt can execute only once.
#' @return Offline receipt summary. Completed source files are not accepted terrain.
#'   Reusable files are rehashed before reuse. Transfer failures are per-file outcomes.
#' @export
run_stream_dem_download <- function(attempt) {
  a <- .fg_dem_request(attempt); m <- a$manifest; r <- a$selection; f <- a$files
  limits <- .fg_dem_limits(m$limits)
  running <- .fg_dem_inside(file.path(attempt,"started.json"),a$root)
  .fg_dem_json(list(at=.fg_dem_time()),running) # Exclusive one-shot execution.
  total <- 0; last_progress <- as.POSIXct(0,origin="1970-01-01")
  cancelled <- function() file.exists(file.path(attempt,"cancelled.json"))
  completed <- 0L
  # Only registered source/content associations are candidates for reuse.
  receipts <- list.files(file.path(a$root,"attempts"),pattern="^[0-9]{6}\\.json$",recursive=TRUE,full.names=TRUE)
  old <- lapply(receipts,function(p) tryCatch({.fg_dem_inside(p,a$root);jsonlite::read_json(p,simplifyVector=TRUE)},error=function(e) NULL))
  old <- Filter(function(x) !is.null(x) && identical(x$schema,"STREAM_DEM_RECEIPT_1"),old)
  for(i in seq_len(nrow(f))) {
    if(cancelled()) break
    part <- .fg_dem_inside(file.path(attempt,"incomplete",sprintf("%06d.part",i)),a$root)
    file_start <- .fg_dem_time(); received <- 0; response <- NULL
    progress <- function(bytes=0,length=NA_real_,force=FALSE) {
      received <<- bytes
      if(force || as.numeric(difftime(Sys.time(),last_progress,units="secs"))>=0.5) {
        .fg_dem_json(list(index=i,title=f$title[i],bytes=bytes,total_bytes=length,
          completed=completed,files=nrow(f),at=.fg_dem_time()),file.path(attempt,"progress.json"),FALSE)
        last_progress <<- Sys.time()
      }
    }
    progress(force=TRUE)
    key <- .fg_dem_source_key(r,f[i,,drop=FALSE])
    receipt <- list(schema="STREAM_DEM_RECEIPT_1",selection_sha256=m$selection_sha256,
      context_revision=m$context_revision,stream_id=m$stream_id,candidate_key=m$candidate_key,
      file_id=f$file_id[i],source_key=key,url=NA_character_,source_filename=NA_character_,
      reported_bytes=f$size_bytes[i],started_at=file_start,outcome="FAILED",message="",bytes=NA_real_)
    tryCatch({
      url <- .fg_dem_url(r,f[i,,drop=FALSE])
      receipt$url <- url;receipt$source_filename <- basename(url)
      matches <- Filter(function(x) {
        same_source <- identical(x$source_key,key) ||
          (identical(x$candidate_key,m$candidate_key) && identical(x$file_id,f$file_id[i]) &&
            identical(x$url,url) && (!is.finite(f$size_bytes[i]) || isTRUE(x$bytes==f$size_bytes[i])))
        same_source && .fg_dem_receipt_valid(x,a$root)
      },old)
      if(length(matches)) {
        reused <- matches[[1]]
        for(n in c("asset","sha256","bytes","headers","verification")) receipt[[n]] <- reused[[n]]
        receipt$outcome <- "REUSED"; receipt$message <- "Local source bytes rehashed and reused; remote freshness not checked."
      } else {
        response <- .fg_dem_transfer(url,part,limits,cancelled,progress)
        checked <- .fg_dem_verify(part,f$size_bytes[i],response)
        if(cancelled()) stop("Download cancelled.")
        asset <- paste0("assets/",m$id,"-",sprintf("%06d",i),"-",checked$sha256,".tif")
        target <- .fg_dem_inside(file.path(a$root,asset),a$root)
        if(!isTRUE(suppressWarnings(file.link(part,target)))) stop("Cannot publish source asset without replacement.")
        receipt$asset <- asset
        for(n in names(checked)) receipt[[n]] <- checked[[n]]
        receipt$http_status <- response$status_code
        receipt$outcome <- "DOWNLOADED"; receipt$message <- "Transfer verified; terrain suitability not reviewed."
      }
      completed <- completed+1L
    },error=function(e) {
      receipt$outcome <<- if(cancelled()) "CANCELLED" else "FAILED"
      receipt$message <<- conditionMessage(e)
      receipt$bytes <<- if(file.exists(part)) file.info(part)$size else received
      if(!is.null(response)) {
        receipt$http_status <<- response$status_code
        receipt$headers <<- tryCatch(.fg_dem_headers(response$headers),error=function(e) list())
      }
    })
    total <- total+max(received,if(file.exists(part)) file.info(part)$size else 0,na.rm=TRUE)
    receipt$finished_at <- .fg_dem_time()
    .fg_dem_json(receipt,file.path(attempt,"receipts",sprintf("%06d.json",i)))
    unlink(part)
    if(receipt$outcome %in% c("DOWNLOADED","REUSED")) old <- c(old,list(receipt))
    progress(received,force=TRUE)
  }
  .fg_dem_json(list(at=.fg_dem_time(),transferred_bytes=total,
    outcome=if(cancelled()) "CANCELLED" else "FINISHED"),
    file.path(attempt,"finished.json"))
  read_stream_dem_download(attempt,verify=TRUE)
}
