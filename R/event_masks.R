.fg_mask_checkpoint <- function(directory) {
  if(file.exists(file.path(directory,"CANCEL"))) stop("Mask creation cancelled.")
}

.fg_mask_recipe <- function(x,stream_id) {
  stream <- x$ctx$streams[x$ctx$streams$stream_id==stream_id,,drop=FALSE]
  reaches <- x$ctx$reaches
  if(!is.null(reaches)) {
    reaches <- reaches[reaches$stream_id==stream_id,,drop=FALSE]
    reaches <- reaches[order(reaches$reach_id),,drop=FALSE]
  }
  geometry <- function(z) if(inherits(z,"sf")) list(crs=sf::st_crs(z)$wkt,
    wkb=sf::st_as_binary(sf::st_geometry(z))) else NULL
  recipe <- list(method="TERRA_MASK_1",group_id=x$settings$group_id,
    stream_id=stream_id,reach_ids=if(is.null(reaches)) character() else reaches$reach_id,
    study=geometry(x$ctx$study_area),stream=geometry(stream),reaches=geometry(reaches),
    wkt=x$crs$wkt,cell_size=x$settings$cell_size,anchor=c(0,0),
    terra=as.character(utils::packageVersion("terra")),gdal=unname(sf::sf_extSoftVersion()["GDAL"]))
  unclass(as.character(openssl::sha256(serialize(recipe,NULL,version=2))))
}

#' Identify the raster-processing dependencies of saved Survey Event masks
#' @inheritParams write_event_masks
#' @return Stable recipe digest covering geometry, grid, identities, native method
#'   and GIS versions. Unrelated labels, dates and metadata revisions are excluded.
#'   This is a reuse key, not a checksum of a raster or scientific approval.
#' @export
event_mask_key <- function(context,selection,group,stream_id) {
  .fg_mask_recipe(.fg_event_grid_inputs(context,selection,group,stream_id),stream_id)
}

.fg_mask_template <- function(plan, wkt) {
  e <- plan$extent
  terra::rast(nrows=plan$rows,ncols=plan$columns,xmin=e[1],xmax=e[3],ymin=e[2],ymax=e[4],crs=wkt)
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

.fg_mask_write <- function(area, plan, wkt, path, parent=NULL) {
  r <- .fg_mask_template(plan,wkt)
  area <- sf::st_transform(area,wkt,partial=FALSE,allow_ballpark=FALSE)
  temporary <- vapply(seq_len(3),function(i) tempfile("mask-",tmpdir=dirname(path),fileext=".tif"),character(1))
  on.exit(unlink(temporary),add=TRUE)
  options <- list(datatype="INT1U",NAflag=255,
    gdal=c("COMPRESS=DEFLATE","TILED=YES","BIGTIFF=IF_SAFER"))
  .fg_mask_checkpoint(dirname(path))
  raw <- terra::rasterize(terra::vect(area),r,field=1,background=0,touches=FALSE,
    filename=temporary[1],wopt=options)
  # Reclassify zero background (including polygon holes) to the One/NoData contract.
  burned <- terra::classify(raw,matrix(c(0,NA_real_),ncol=2),
    filename=if(is.null(parent)) path else temporary[2],wopt=options)
  if(!is.null(parent)) {
    .fg_mask_checkpoint(dirname(path))
    aligned <- .fg_mask_parent(parent,r)$raster
    cropped <- terra::crop(aligned,terra::ext(r),snap="near",filename=temporary[3],wopt=options)
    burned <- terra::mask(burned,cropped,filename=path,wopt=options)
  }
  invisible(NULL)
}

.fg_mask_verify <- function(path, plan, wkt, parent=NULL) {
  r <- terra::rast(path)
  if(!isTRUE(terra::compareGeom(r,.fg_mask_template(plan,wkt),stopOnError=FALSE)) ||
      any(abs(as.vector(terra::ext(r))-plan$extent[c(1,3,2,4)])>1e-8) ||
      !identical(terra::datatype(r),"INT1U")) stop("Reopened mask grid or datatype differs.")
  .fg_mask_checkpoint(dirname(path))
  summary <- terra::global(r,c("min","max","sum"),na.rm=TRUE)
  if((is.finite(summary$min) && summary$min!=1) || (is.finite(summary$max) && summary$max!=1))
    stop("Mask contains values other than One/NoData.")
  if(!is.null(parent)) .fg_mask_parent(parent,r)
  if(is.na(summary$sum)) 0 else as.numeric(summary$sum)
}
#' Write verified Study Area, Stream and Reach masks
#' @param context Current Study Area context GeoPackage.
#' @param selection Current Survey Collection selection GeoPackage.
#' @param group Reviewed acquisition-group GeoPackage.
#' @param stream_id One Stream in the group.
#' @param directory New, nonexistent attempt directory. Existing paths are never replaced.
#' @param study_mask_source Optional previously published masks for the same saved
#'   inputs. Reuse its Study Area mask instead of rasterizing that boundary again.
#' @param ... Compatibility argument for earlier development previews. New callers
#'   use study_mask_source.
#' @return Manifest with input hashes, grid, mask hashes, cell counts and software evidence.
#' @details Uses terra polygon rasterization with touches=FALSE (cell centers),
#'   shared zero anchor and Event spacing. Children intersect their parent masks.
#'   Missing Reach polygons block mask creation. Disk-backed compressed Byte
#'   GeoTIFFs contain only 1 and NoData. Standard terra rasterize, crop and mask
#'   operations write compressed disk-backed rasters with BigTIFF support.
#'   No application cell-count, row-width or estimated-disk admission limit is
#'   imposed. Actual I/O failures stop publication. A verified manifest is
#'   written last. Interrupted attempts
#'   are incomplete. Applications should stage the directory and publish it only
#'   after checking that the original request is still current. No DEM is read.
#' @export
write_event_masks <- function(context, selection, group, stream_id, directory, study_mask_source=NULL, ...) {
  legacy <- list(...)
  if(length(legacy)) {
    if(!identical(names(legacy),"study_family") || !is.null(study_mask_source))
      stop("Unused or conflicting mask arguments.")
    study_mask_source <- legacy[[1L]]
  }
  x <- .fg_event_grid_inputs(context,selection,group,stream_id)
  if(length(directory)!=1L || is.na(directory) || file.exists(directory) || !dir.exists(dirname(directory)))
    stop("Supply a new mask attempt directory under an existing parent.")
  ctx <- x$ctx; wkt <- x$crs$wkt; size <- x$settings$cell_size
  stream <- ctx$streams[ctx$streams$stream_id==stream_id,,drop=FALSE]
  reaches <- if(is.null(ctx$reaches)) data.frame(reach_id=character(),stream_id=character()) else
    ctx$reaches[ctx$reaches$stream_id==stream_id,,drop=FALSE]
  if(nrow(reaches)>0 && !inherits(reaches,"sf")) stop("Save Reach polygons before creating masks for this Stream.")
  areas <- c(list(ctx$study_area,stream),lapply(seq_len(nrow(reaches)),function(i) reaches[i,,drop=FALSE]))
  levels <- c("Study Area","Stream",rep("Reach",nrow(reaches)))
  ids <- c(x$settings$study_area_id,stream_id,reaches$reach_id)
  parents <- c(NA_integer_,1L,rep(2L,nrow(reaches)))
  plans <- list()
  for(i in seq_along(areas)) plans[[i]] <- .fg_dem_grid_plan(areas[[i]],wkt,size,
    if(!is.na(parents[i])) plans[[parents[i]]]$index else NULL)
  cells <- sum(vapply(plans,function(p) p$cells,numeric(1)))
  if(!dir.create(directory)) stop("Could not create a new mask attempt.")
  products <- list()
  shared <- if(!is.null(study_mask_source)) read_event_masks(study_mask_source,verify=FALSE) else NULL
  if(!is.null(shared) && ((!identical(shared$inputs,as.list(x$hashes)) &&
      (is.null(shared$study_key) || !identical(shared$study_key,.fg_mask_recipe(x,NULL)))) ||
      !identical(shared$grid$wkt,wkt) || !isTRUE(shared$grid$cell_size==size)))
    stop("Shared Study Area mask uses different saved inputs.")
  for(i in seq_along(areas)) {
    .fg_mask_checkpoint(directory)
    file <- sprintf("mask-%04d.tif",i)
    parent <- if(!is.na(parents[i])) file.path(directory,products[[parents[i]]]$file) else NULL
    path <- file.path(directory,file)
    if(i==1L && !is.null(shared)) {
      previous <- shared$products[[1L]]
      if(!isTRUE(all.equal(lapply(previous$plan,unlist),plans[[1L]],check.attributes=FALSE)))
        stop("Shared Study Area mask uses a different grid.")
      source <- file.path(study_mask_source,previous$file)
      # Immutable editions can share bytes on one filesystem. Copy only when a
      # native hard link is unavailable; no extra rasterization or value scan.
      if(!suppressWarnings(file.link(source,path)) && !file.copy(source,path))
        stop("Could not reuse the Study Area mask.")
      previous$plan <- plans[[i]]
      products[[i]] <- previous
      next
    }
    .fg_mask_write(areas[[i]],plans[[i]],wkt,path,parent)
    count <- .fg_mask_verify(path,plans[[i]],wkt,parent)
    products[[i]] <- list(level=levels[i],id=ids[i],parent=parents[i],file=file,plan=plans[[i]],
      valid_cells=count,bytes=unname(file.info(path)$size),sha256=.fg_dem_hash(path))
  }
  if(!identical(x$hashes,vapply(x$paths,.fg_dem_hash,character(1)))) stop("Mask inputs changed during processing.")
  manifest <- list(schema="EVENT_MASKS_1",group_id=x$settings$group_id,stream_id=stream_id,
    recipe_key=.fg_mask_recipe(x,stream_id),
    study_key=.fg_mask_recipe(x,NULL),
    created_at=.fg_dem_time(),inputs=as.list(x$hashes),
    revisions=as.list(stats::setNames(basename(x$paths),names(x$paths))),
    grid=list(wkt=wkt,unit=x$crs$unit,cell_size=size,anchor=c(0,0)),
    boundary_rule="terra rasterize touches=FALSE (native cell-center rule)",
    datatype="INT1U",nodata=255,products=products,
    admission=list(cells=cells),
    software=list(fluvgeo=as.character(utils::packageVersion("fluvgeo")),
      terra=as.character(utils::packageVersion("terra")),sf=as.character(utils::packageVersion("sf")),
      geospatial=as.list(sf::sf_extSoftVersion())))
  .fg_mask_checkpoint(directory)
  .fg_dem_json(manifest,file.path(directory,"verified.json"))
  manifest
}

#' Reopen and verify saved masks
#' @param directory Directory produced by write_event_masks.
#' @param verify Verify product checksums (default TRUE). FALSE performs metadata-only reopening of managed products; it does not establish byte integrity.
#' @return Manifest. Missing manifests and invalid metadata error; verify=TRUE also detects changed bytes.
#' @export
read_event_masks <- function(directory,verify=TRUE) {
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
  if(!identical(m$grid$unit,crs$unit) || !m$boundary_rule %in% c(
      "terra rasterize touches=FALSE (native cell-center rule)",
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
    if(isTRUE(verify) && !identical(.fg_dem_hash(file),p$sha256)) stop("Mask checksum differs from verified manifest.")
    parent <- if(i==1L) NULL else file.path(directory,sprintf("mask-%04d.tif",if(i==2L) 1L else 2L))
    r <- terra::rast(file)
    if(!isTRUE(terra::compareGeom(r,.fg_mask_template(plan,m$grid$wkt),stopOnError=FALSE)) ||
        !identical(terra::datatype(r),"INT1U") ||
        (!is.null(p$bytes) && !isTRUE(file.info(file)$size==p$bytes)))
      stop("Saved mask metadata differs from verified manifest.")
    if(!is.null(parent)) .fg_mask_parent(parent,r)
  }
  m
}
