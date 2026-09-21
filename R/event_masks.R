.fg_mask_checkpoint <- function(directory) {
  if(file.exists(file.path(directory,"CANCEL"))) stop("Mask creation cancelled.")
}

.fg_mask_template <- function(plan, wkt) {
  e <- plan$extent
  terra::rast(nrows=plan$rows,ncols=plan$columns,xmin=e[1],xmax=e[3],ymin=e[2],ymax=e[4],crs=wkt)
}

.fg_mask_blocks <- function(r, fun) {
  rows <- max(1L,floor(65536/ncol(r)))
  for(start in seq(1,nrow(r),by=rows)) fun(start,min(rows,nrow(r)-start+1))
}

.fg_mask_parent <- function(path, r) {
  p <- terra::rast(path)
  offset <- c((terra::xmin(r)-terra::xmin(p))/terra::res(r)[1],
    (terra::ymax(p)-terra::ymax(r))/terra::res(r)[2])
  if(!isTRUE(sf::st_crs(terra::crs(p))==sf::st_crs(terra::crs(r))) ||
      any(abs(terra::res(p)-terra::res(r))>1e-10*terra::res(r)) ||
      any(abs(offset-round(offset))>1e-7) || any(offset<0) ||
      offset[1]+ncol(r)>ncol(p) || offset[2]+nrow(r)>nrow(p)) stop("Parent mask is not aligned.")
  list(raster=p,col=round(offset[1])+1,row=round(offset[2]))
}

.fg_mask_parent_values <- function(parent,start,nrows,columns) {
  terra::readValues(parent$raster,row=parent$row+start,nrows=nrows,col=parent$col,ncols=columns)
}

.fg_mask_write <- function(area, plan, wkt, path, parent=NULL) {
  r <- .fg_mask_template(plan,wkt)
  area <- sf::st_transform(area,wkt,partial=FALSE,allow_ballpark=FALSE)
  if(!is.null(parent)) {
    parent <- .fg_mask_parent(parent,r)
    terra::readStart(parent$raster); on.exit(terra::readStop(parent$raster),add=TRUE)
  }
  terra::writeStart(r,path,overwrite=FALSE,datatype="INT1U",NAflag=255,
    gdal=c("COMPRESS=DEFLATE","TILED=YES"))
  opened <- TRUE
  on.exit(if(opened) try(terra::writeStop(r),silent=TRUE),add=TRUE)
  expected_count <- 0
  .fg_mask_blocks(r,function(start,nrows) {
    .fg_mask_checkpoint(dirname(path))
    cells <- seq((start-1)*ncol(r)+1,(start+nrows-1)*ncol(r))
    xy <- terra::xyFromCell(r,cells)
    points <- sf::st_as_sf(data.frame(x=xy[,1],y=xy[,2]),coords=c("x","y"),crs=wkt)
    # GEOS strict interior makes exact exterior/hole-edge centers NoData.
    inside <- lengths(sf::st_within(points,area))>0L
    if(!is.null(parent)) inside <- inside & !is.na(.fg_mask_parent_values(parent,start,nrows,ncol(r)))
    expected_count <<- expected_count+sum(inside)
    terra::writeValues(r,ifelse(inside,1,NA_real_),start,nrows)
  })
  terra::writeStop(r); opened <- FALSE
  invisible(expected_count)
}

.fg_mask_verify <- function(path, plan, wkt, parent=NULL) {
  r <- terra::rast(path)
  if(ncol(r)>65536) stop("Mask exceeds the supported row-width budget.")
  if(!isTRUE(terra::compareGeom(r,.fg_mask_template(plan,wkt),stopOnError=FALSE)) ||
      any(abs(as.vector(terra::ext(r))-plan$extent[c(1,3,2,4)])>1e-8) ||
      !identical(terra::datatype(r),"INT1U")) stop("Reopened mask grid or datatype differs.")
  terra::readStart(r); on.exit(terra::readStop(r),add=TRUE)
  if(!is.null(parent)) {
    parent <- .fg_mask_parent(parent,r)
    terra::readStart(parent$raster); on.exit(terra::readStop(parent$raster),add=TRUE)
  }
  count <- 0
  .fg_mask_blocks(r,function(start,nrows) {
    .fg_mask_checkpoint(dirname(path))
    v <- terra::readValues(r,row=start,nrows=nrows)
    if(any(!is.na(v) & v!=1)) stop("Mask contains values other than One/NoData.")
    if(!is.null(parent) && any(!is.na(v) & is.na(.fg_mask_parent_values(parent,start,nrows,ncol(r)))))
      stop("Child mask extends beyond its parent.")
    count <<- count+sum(!is.na(v))
  })
  count
}

#' Write a verified Study Area, Stream and Reach mask family
#' @param context Current Study Area context GeoPackage.
#' @param selection Current Survey Collection selection GeoPackage.
#' @param group Reviewed acquisition-group GeoPackage.
#' @param stream_id One Stream in the group.
#' @param directory New, nonexistent attempt directory. Existing paths are never replaced.
#' @param max_cells Maximum total cells admitted across the family; default 50 million.
#' @return Manifest with input hashes, grid, mask hashes, cell counts and software evidence.
#' @details Uses strict cell-center interior membership (edge centers are NoData),
#'   shared zero anchor and Event spacing. Children intersect their parent masks.
#'   Missing Reach polygons block the entire family. Disk-backed compressed Byte
#'   GeoTIFFs contain only 1 and NoData. Processing uses at most 65536 centers per
#'   block and rejects wider grids. Admission requires available disk space of at
#'   least four times the uncompressed payload plus 256 MiB; this is an estimate,
#'   not a reservation. A verified manifest is written last. Interrupted attempts
#'   are incomplete. Applications should stage the directory and publish it only
#'   after checking that the original request is still current. No DEM is read.
#' @export
write_event_masks <- function(context, selection, group, stream_id, directory, max_cells=5e7) {
  x <- .fg_event_grid_inputs(context,selection,group,stream_id)
  if(length(directory)!=1L || is.na(directory) || file.exists(directory) || !dir.exists(dirname(directory)))
    stop("Supply a new mask attempt directory under an existing parent.")
  if(length(max_cells)!=1L || !is.finite(max_cells) || max_cells<=0) stop("Invalid mask cell budget.")
  ctx <- x$ctx; wkt <- x$crs$wkt; size <- x$settings$cell_size
  stream <- ctx$streams[ctx$streams$stream_id==stream_id,,drop=FALSE]
  reaches <- if(is.null(ctx$reaches)) data.frame(reach_id=character(),stream_id=character()) else
    ctx$reaches[ctx$reaches$stream_id==stream_id,,drop=FALSE]
  if(nrow(reaches)>0 && !inherits(reaches,"sf")) stop("Save Reach polygons before creating this mask family.")
  areas <- c(list(ctx$study_area,stream),lapply(seq_len(nrow(reaches)),function(i) reaches[i,,drop=FALSE]))
  levels <- c("Study Area","Stream",rep("Reach",nrow(reaches)))
  ids <- c(x$settings$study_area_id,stream_id,reaches$reach_id)
  parents <- c(NA_integer_,1L,rep(2L,nrow(reaches)))
  plans <- list()
  for(i in seq_along(areas)) plans[[i]] <- .fg_dem_grid_plan(areas[[i]],wkt,size,
    if(!is.na(parents[i])) plans[[parents[i]]]$index else NULL)
  cells <- sum(vapply(plans,function(p) p$cells,numeric(1)))
  if(cells>max_cells || any(vapply(plans,function(p) p$columns>65536,logical(1))))
    stop("Mask family exceeds the cell or row-width budget. Review Event spacing or study extent.")
  required <- 4*cells+256*1024^2
  available <- ps::ps_disk_usage(dirname(directory))$available[1]
  if(!is.finite(available) || available<required) stop("Insufficient available disk space for mask attempt.")
  if(!dir.create(directory)) stop("Could not create a new mask attempt.")
  products <- list()
  for(i in seq_along(areas)) {
    .fg_mask_checkpoint(directory)
    file <- sprintf("mask-%04d.tif",i)
    parent <- if(!is.na(parents[i])) file.path(directory,products[[parents[i]]]$file) else NULL
    path <- file.path(directory,file)
    expected_count <- .fg_mask_write(areas[[i]],plans[[i]],wkt,path,parent)
    count <- .fg_mask_verify(path,plans[[i]],wkt,parent)
    if(count!=expected_count) stop("Reopened mask membership count differs from computed centers.")
    products[[i]] <- list(level=levels[i],id=ids[i],parent=parents[i],file=file,plan=plans[[i]],
      valid_cells=count,sha256=.fg_dem_hash(path))
  }
  if(!identical(x$hashes,vapply(x$paths,.fg_dem_hash,character(1)))) stop("Mask inputs changed during processing.")
  manifest <- list(schema="EVENT_MASKS_1",group_id=x$settings$group_id,stream_id=stream_id,
    created_at=.fg_dem_time(),inputs=as.list(x$hashes),
    revisions=as.list(stats::setNames(basename(x$paths),names(x$paths))),
    grid=list(wkt=wkt,unit=x$crs$unit,cell_size=size,anchor=c(0,0)),
    boundary_rule="cell center strictly inside polygon; exterior and hole boundaries excluded",
    datatype="INT1U",nodata=255,products=products,
    admission=list(cells=cells,max_cells=max_cells,required_bytes=required,available_bytes=available),
    software=list(fluvgeo=as.character(utils::packageVersion("fluvgeo")),
      terra=as.character(utils::packageVersion("terra")),sf=as.character(utils::packageVersion("sf")),
      geospatial=as.list(sf::sf_extSoftVersion())))
  .fg_mask_checkpoint(directory)
  .fg_dem_json(manifest,file.path(directory,"verified.json"))
  manifest
}

#' Reopen and verify a saved mask family
#' @param directory Directory produced by write_event_masks.
#' @return Verified manifest. Missing manifests, corrupt files and invalid masks error.
#' @export
read_event_masks <- function(directory) {
  path <- file.path(directory,"verified.json")
  if(!file.exists(path)) stop("Mask attempt is incomplete.")
  m <- jsonlite::read_json(path,simplifyVector=TRUE)
  # Preserve the nested product/plan records rather than simplifying to a table.
  m$products <- jsonlite::read_json(path,simplifyVector=FALSE)$products
  if(!identical(m$schema,"EVENT_MASKS_1") || length(m$products)<2L) stop("Unsupported mask manifest.")
  size <- m$grid$cell_size
  if(!is.numeric(size) || length(size)!=1L || !is.finite(size) || size<=0 ||
      !identical(as.numeric(m$grid$anchor),c(0,0)) || !identical(m$datatype,"INT1U") ||
      !identical(as.numeric(m$nodata),255)) stop("Invalid saved mask grid definition.")
  crs <- validate_study_analysis_crs(m$grid$wkt)
  if(!identical(m$grid$unit,crs$unit) || !identical(m$boundary_rule,
      "cell center strictly inside polygon; exterior and hole boundaries excluded"))
    stop("Unsupported mask units or boundary rule.")
  for(i in seq_along(m$products)) {
    p <- m$products[[i]]
    if(!identical(p$file,sprintf("mask-%04d.tif",i))) stop("Invalid mask filename.")
    expected_parent <- if(i==2L) 1L else 2L
    expected_level <- if(i==1L) "Study Area" else if(i==2L) "Stream" else "Reach"
    if(!identical(p$level,expected_level) || (i==1L && !is.null(p$parent)) ||
        (i>1L && (length(p$parent)!=1L || is.na(p$parent) || p$parent!=expected_parent)))
      stop("Invalid saved mask hierarchy.")
    plan <- lapply(p$plan,unlist)
    index <- plan$index
    if(length(index)!=4L || any(!is.finite(index)) || any(index!=round(index)) ||
        length(plan$extent)!=4L || any(!is.finite(plan$extent)) ||
        any(abs(plan$extent-index*size)>1e-8) ||
        !isTRUE(plan$columns==index[3]-index[1]) || !isTRUE(plan$rows==index[4]-index[2]) ||
        !isTRUE(plan$cells==plan$columns*plan$rows)) stop("Saved mask plan differs from the Event grid.")
    file <- file.path(directory,p$file); .fg_dem_inside(file,directory)
    if(!identical(.fg_dem_hash(file),p$sha256)) stop("Mask checksum differs from verified manifest.")
    parent <- if(i==1L) NULL else file.path(directory,sprintf("mask-%04d.tif",if(i==2L) 1L else 2L))
    count <- .fg_mask_verify(file,plan,m$grid$wkt,parent)
    if(!identical(as.numeric(count),as.numeric(p$valid_cells))) stop("Mask valid-cell count differs.")
  }
  m
}
