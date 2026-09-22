.fg_event_grid_inputs <- function(context, selection, group, stream_id) {
  paths <- c(context=context,selection=selection,group=group)
  hashes <- vapply(paths,.fg_dem_hash,character(1))
  ctx <- read_study_context(context); d <- read_survey_collection_selection(selection)
  g <- read_survey_acquisition_group(group); settings <- g$settings
  if(!identical(settings$study_area_id,ctx$study_area$study_area_id) ||
      !identical(d$study_area$study_area_id,settings$study_area_id)) stop("Group and selection must belong to this Study Area.")
  if(!identical(settings$context_revision,basename(context)) ||
      !identical(settings$selection_revision,basename(selection)))
    stop("Study or collection selection changed. Review and save Event settings again.")
  ref <- ctx$analysis_reference
  at <- which(ref$component=="horizontal" & ref$basis=="PROJECT_RECORD")
  if(length(at)!=1L || !identical(ref$value[at],settings$wkt)) stop("Analysis CRS changed. Review Event settings.")
  crs <- validate_study_analysis_crs(settings$wkt,ctx$study_area)
  if(!identical(crs$unit,settings$unit)) stop("Event grid unit differs from the saved CRS.")
  members <- g$members$candidate_key
  if(!length(members) || anyDuplicated(members) || !all(members %in% d$selected)) stop("Group membership is stale.")
  current_members <- d$records[match(members,d$records$candidate_key),]
  if(!identical(current_members$raw_metadata,g$members$raw_metadata) ||
      !identical(current_members$snapshot_id,g$members$snapshot_id)) stop("Collection evidence changed. Review Event settings.")
  if(length(stream_id)!=1L || !stream_id %in% g$streams$stream_id || !stream_id %in% ctx$streams$stream_id)
    stop("Choose a current Stream in this Event group.")
  if(!all(g$streams$stream_id %in% ctx$streams$stream_id) ||
      !all(g$event_links$survey_event_id %in% ctx$survey_events$survey_event_id)) stop("Group hierarchy links are stale.")
  if(nrow(g$event_links)) {
    events <- ctx$survey_events[match(g$event_links$survey_event_id,ctx$survey_events$survey_event_id),,drop=FALSE]
    if(!all(events$reach_id %in% ctx$reaches$reach_id[ctx$reaches$stream_id %in% g$streams$stream_id]) ||
        any(events$survey_year!=settings$year) ||
        any(!is.na(events$survey_month) & (is.na(settings$month) | events$survey_month!=settings$month)))
      stop("Reach Event parentage or dates changed. Review Event settings.")
  }
  list(paths=paths,hashes=hashes,ctx=ctx,d=d,g=g,settings=settings,crs=crs,members=members,current_members=current_members)
}

.fg_dem_grid_plan <- function(area, crs, cell_size, parent=NULL) {
  if(!inherits(area,"sf") || !nrow(area) || any(sf::st_is_empty(area)) ||
      any(!sf::st_is_valid(area)) || !all(sf::st_geometry_type(area) %in% c("POLYGON","MULTIPOLYGON")))
    stop("A saved valid polygon is required for each planned grid.")
  area <- sf::st_transform(area,crs,partial=FALSE,allow_ballpark=FALSE)
  b <- as.numeric(sf::st_bbox(area))
  if(any(!is.finite(b)) || !is.finite(cell_size) || cell_size<=0) stop("Invalid grid extent or spacing.")
  # Integer indices about the shared (0, 0) anchor, never a source tile origin.
  index <- c(floor(b[1:2]/cell_size),ceiling(b[3:4]/cell_size))
  if(!is.null(parent)) {
    index[1:2] <- pmax(index[1:2],parent[1:2])
    index[3:4] <- pmin(index[3:4],parent[3:4])
  }
  dimensions <- index[3:4]-index[1:2]
  cells <- prod(dimensions)
  if(any(!is.finite(index)) || any(abs(index)>2^53-1) || any(dimensions<=0) ||
      any(dimensions>.Machine$integer.max) || !is.finite(cells) || cells>2^53-1)
    stop("Grid dimensions exceed supported exact sizing or have no parent overlap.")
  list(index=index,extent=index*cell_size,columns=dimensions[1],rows=dimensions[2],cells=cells,
    mask_bytes=cells,float32_bytes=4*cells,float64_bytes=8*cells)
}

.fg_dem_source_screen <- function(observation, target, cell_size) {
  a <- observation$default_reader; b <- observation$internal_compound
  g <- a$grid; t <- g$geotransform
  issues <- character()
  crs <- tryCatch(sf::st_crs(a$wkt),error=function(e) sf::NA_crs_)
  projected <- !is.na(crs) && isFALSE(crs$IsGeographic) && grepl("^PROJCRS\\[",crs$wkt)
  factor <- if(projected) tryCatch(as.numeric(units::set_units(crs$ud_unit,"m",mode="standard")),
    error=function(e) NA_real_) else NA_real_
  if(length(factor)!=1L || !is.finite(factor) || factor<=0) factor <- NA_real_
  spacing <- g$spacing
  if(length(spacing)!=2L || any(!is.finite(spacing)) || any(spacing<=0)) spacing <- rep(NA_real_,2)
  metres <- spacing*factor
  if(!projected || is.na(factor)) issues <- c(issues,"Projected 2D source CRS and linear units require review")
  if(anyNA(metres)) issues <- c(issues,"Physical source spacing unresolved") else
    if(any(metres>1+1e-9)) issues <- c(issues,"Source spacing exceeds the 1 m screen")
  affine <- length(t)==6L && all(is.finite(t)) && t[2]>0 && t[6]<0 && t[3]==0 && t[5]==0
  if(!affine) issues <- c(issues,"Rotated, reversed or missing source affine grid requires review")
  if(all(is.finite(spacing)) && abs(spacing[1]-spacing[2])>1e-9*max(spacing))
    issues <- c(issues,"Anisotropic source spacing requires review")
  if(!identical(a$wkt,b$wkt) || !identical(a$grid,b$grid) || !identical(a$band_unit,b$band_unit))
    issues <- c(issues,"Ordinary and embedded metadata differ")
  same <- projected && isTRUE(crs==sf::st_crs(target))
  near_integer <- function(v) all(abs(v-round(v))<1e-7)
  aligned <- affine && same && all(is.finite(spacing)) &&
    all(abs(spacing-cell_size)<1e-9*cell_size) && near_integer(t[c(1,4)]/cell_size)
  list(source_unit=if(is.null(g$horizontal_unit)) "Unknown" else g$horizontal_unit,
    spacing_x=spacing[1],spacing_y=spacing[2],metres_x=metres[1],metres_y=metres[2],
    alignment=if(aligned) "Aligned: no interpolation indicated" else "Reprojection/resampling needs qualification",
    grid_screen=if(length(issues)) "REVIEW" else "PASS",
    issues=paste(issues,collapse="; "),
    band_unit=if(nzchar(a$band_unit)) a$band_unit else "Unknown",
    vertical="Source/target vertical reference and prior conversions require review")
}

#' Preflight a Stream acquisition group without creating terrain products
#' @param context Current Study Area context GeoPackage.
#' @param selection Current Survey Collection selection GeoPackage.
#' @param group Reviewed acquisition-group GeoPackage.
#' @param stream_id One Stream in the group.
#' @param sources Data frame with candidate_key, selection_path and attempt, one
#'   row for every group member. Missing saved selections/attempts may be NA.
#' @return Read-only report with grid estimates, per-source screening, receipt
#'   observations, input hashes and software versions. No processing approval.
#' @details Uses the fixed zero anchor and saved Event spacing. Byte estimates
#'   are uncompressed single-layer payloads, excluding overhead, intermediates and
#'   free-space checks. No raster values, masks or output files are allocated.
#'   Hash verification reads original source bytes. Full pixel readability,
#'   observed coverage and vertical operations remain unqualified.
#' @export
preflight_stream_dem <- function(context, selection, group, stream_id, sources) {
  checked <- .fg_event_grid_inputs(context,selection,group,stream_id)
  paths <- checked$paths; hashes <- checked$hashes; ctx <- checked$ctx; d <- checked$d
  g <- checked$g; settings <- checked$settings; crs <- checked$crs
  members <- checked$members; current_members <- checked$current_members
  if(!is.data.frame(sources) || !all(c("candidate_key","selection_path","attempt") %in% names(sources)) ||
      anyNA(sources$candidate_key) || anyDuplicated(sources$candidate_key) || !setequal(sources$candidate_key,members))
    stop("Supply exactly one source request per group collection.")
  stream <- ctx$streams[ctx$streams$stream_id==stream_id,,drop=FALSE]
  parent <- .fg_dem_grid_plan(ctx$study_area,crs$wkt,settings$cell_size)
  child <- .fg_dem_grid_plan(stream,crs$wkt,settings$cell_size,parent$index)
  grids <- list(); append_grid <- function(level,id,plan) {
    grids[[length(grids)+1L]] <<- data.frame(level=level,id=id,xmin=plan$extent[1],ymin=plan$extent[2],
      xmax=plan$extent[3],ymax=plan$extent[4],columns=plan$columns,rows=plan$rows,cells=plan$cells,
      mask_bytes=plan$mask_bytes,float32_bytes=plan$float32_bytes,float64_bytes=plan$float64_bytes)
  }
  append_grid("Study Area",settings$study_area_id,parent); append_grid("Stream",stream_id,child)
  notes <- character()
  reaches <- ctx$reaches
  if(!is.null(reaches)) for(i in which(reaches$stream_id==stream_id)) {
    if(!inherits(reaches,"sf")) { notes <- c(notes,"Reach polygons are absent; their grids remain unresolved."); break }
    p <- .fg_dem_grid_plan(reaches[i,,drop=FALSE],crs$wkt,settings$cell_size,child$index)
    append_grid("Reach",reaches$reach_id[i],p)
  }
  rows <- list(); observations <- list(); source_hashes <- list()
  for(k in members) {
    request <- sources[sources$candidate_key==k,,drop=FALSE]
    problem <- tryCatch({
      if(!any(d$acquisition_plan$candidate_key==k & d$acquisition_plan$product=="DEM")) stop("Save a DEM acquisition plan for this collection.")
      if(is.na(request$selection_path) || is.na(request$attempt)) stop("Saved file choices and a local download attempt are required.")
      saved_hash <- .fg_dem_hash(request$selection_path)
      saved <- read_stream_dem_selection(request$selection_path)
      a <- .fg_dem_request(request$attempt)
      if(!identical(saved_hash,a$manifest$selection_sha256) ||
          !identical(saved$stream$stream_id,stream_id) || !identical(saved$collection$candidate_key,k) ||
          !identical(a$manifest$stream_id,stream_id) || !identical(a$manifest$candidate_key,k)) stop("Download does not match current file choices or group membership.")
      if(!isTRUE(lengths(sf::st_equals(stream,sf::st_transform(saved$stream,sf::st_crs(stream))))[1]==1L))
        stop("Stream geometry changed since file selection.")
      collection <- current_members[current_members$candidate_key==k,]
      if(!identical(saved$collection$raw_metadata,collection$raw_metadata) ||
          !identical(saved$collection$snapshot_id,collection$snapshot_id)) stop("File selection uses different collection evidence.")
      if(!identical(saved$outcome,"COMPLETE") || !length(saved$selected)) stop("File discovery must be complete and have saved selections.")
      source_hashes[[k]] <- saved_hash
      for(file_id in saved$selected) {
        inspected <- tryCatch(inspect_stream_dem_download(request$attempt,file_id),error=function(e) e)
        if(inherits(inspected,"error")) {
          rows[[length(rows)+1L]] <- data.frame(collection=k,file_id=file_id,sha256=NA_character_,
            source_unit=NA_character_,spacing_x=NA_real_,spacing_y=NA_real_,metres_x=NA_real_,metres_y=NA_real_,
            alignment="Unknown",grid_screen="BLOCKED",issues=conditionMessage(inspected),band_unit="Unknown",vertical="Unresolved")
        } else {
          observations[[paste(k,file_id,sep="/")]] <- inspected
          rows[[length(rows)+1L]] <- cbind(data.frame(collection=k,file_id=file_id,sha256=inspected$sha256),
            as.data.frame(.fg_dem_source_screen(inspected$observation,crs$wkt,settings$cell_size)))
        }
      }
      if(!identical(saved_hash,.fg_dem_hash(request$selection_path))) stop("File selection changed during preflight.")
      NULL
    },error=function(e) conditionMessage(e))
    if(!is.null(problem)) rows[[length(rows)+1L]] <- data.frame(collection=k,file_id="",sha256=NA_character_,
      source_unit=NA_character_,spacing_x=NA_real_,spacing_y=NA_real_,metres_x=NA_real_,metres_y=NA_real_,
      alignment="Unknown",grid_screen="BLOCKED",issues=problem,band_unit="Unknown",vertical="Unresolved")
  }
  if(!identical(hashes,vapply(paths,.fg_dem_hash,character(1)))) stop("Study, selection or Event settings changed during preflight.")
  table <- do.call(rbind,rows)
  list(schema="STREAM_DEM_PREFLIGHT_1",group_id=settings$group_id,stream_id=stream_id,
    checked_at=.fg_dem_time(),inputs=as.list(hashes),source_selection_hashes=source_hashes,
    grid=list(wkt=crs$wkt,unit=crs$unit,cell_size=settings$cell_size,anchor=c(0,0)),
    grids=do.call(rbind,grids),sources=table,observations=observations,
    grid_source_screen=if(any(table$grid_screen=="BLOCKED")) "BLOCKED" else
      if(length(notes) || any(table$grid_screen=="REVIEW")) "REVIEW" else "PASS",
    processing_authorized=FALSE,
    notes=unique(c(notes,"Extents are grid envelopes, not rasterized masks or observed coverage.",
      "Byte estimates exclude overhead, compression, intermediates and free-space checks.",
      "No vertical operations, elevation-unit conversions or source suitability acceptance are authorized.")),
    software=list(fluvgeo=as.character(utils::packageVersion("fluvgeo")),sf=as.character(utils::packageVersion("sf")),
      geospatial=as.list(sf::sf_extSoftVersion())))
}
