.fg_horizontal_info <- function(path) {
  observation <- inspect_terrain_vertical_reference(path)
  stem <- tools::file_path_sans_ext(path)
  sidecars <- c(paste0(path,c(".aux.xml",".ovr",".msk")),paste0(stem,c(".tfw",".tifw",".wld")))
  if(any(file.exists(sidecars))) stop("External raster sidecars require review before horizontal processing.")
  raw <- sf::gdal_utils("info",path,options=c("-json","-norat","-oo","GEOREF_SOURCES=INTERNAL"),
    config_options=c(GDAL_PAM_ENABLED="NO",GTIFF_REPORT_COMPD_CS="TRUE"),quiet=TRUE)
  info <- jsonlite::fromJSON(raw,simplifyVector=FALSE)
  if(!is.null(info$coordinateSystem$coordinateEpoch) || !is.null(info$coordinateEpoch))
    stop("Coordinate-epoch rasters require explicit operation qualification.")
  band <- info$bands[[1L]]
  if((!is.null(band$scale) && band$scale!=1) || (!is.null(band$offset) && band$offset!=0))
    stop("Nonidentity scale/offset requires an explicit decoding workflow.")
  if(!band$type %in% c("Byte","Int8","UInt16","Int16","UInt32","Int32","Float32","Float64") ||
      !band$colorInterpretation %in% c("Gray","Undefined") || !is.null(band$colorTable) ||
      (!is.null(band$mask) && !identical(unlist(band$mask$flags),"NODATA")))
    stop("Unsupported terrain band or mask representation.")
  t <- unlist(info$geoTransform); size <- unlist(info$size)
  if(length(t)!=6L || any(!is.finite(t)) || t[2]<=0 || t[6]>=0 || t[3]!=0 || t[5]!=0 ||
      length(size)!=2L || any(!is.finite(size)) || any(size<=0) || size[1]>65536 ||
      abs(t[2]+t[6])>1e-10*t[2] || !is.null(info$gcps))
    stop("Only north-up square-cell affine rasters within the row-width budget are qualified.")
  if(!identical(observation$default_reader$grid$geotransform,t) ||
      !identical(observation$default_reader$grid$size,size) ||
      !identical(observation$default_reader$band_unit,observation$internal_compound$band_unit))
    stop("Ordinary and embedded raster metadata disagree.")
  list(observation=observation,info=info,transform=t,size=size)
}

.fg_horizontal_crs <- function(x,compound=FALSE) {
  j <- x$info$stac[["proj:projjson"]]
  original <- jsonlite::toJSON(j,auto_unbox=TRUE,digits=NA,null="null")
  if(grepl("Dynamic|frame_reference_epoch|coordinate_epoch",original)) stop("Dynamic/epoch-dependent sources are not qualified.")
  if(identical(j$type,"CompoundCRS") && compound) {
    types <- vapply(j$components,function(z) z$type,character(1))
    if(!identical(types,c("ProjectedCRS","VerticalCRS"))) stop("Unsupported compound source CRS.")
    j <- j$components[[1L]]
  }
  if(!identical(j$type,"ProjectedCRS")) stop("A projected 2D horizontal CRS is required.")
  axes <- vapply(j$coordinate_system$axis,function(z) z$direction,character(1))
  if(!identical(axes,c("east","north"))) stop("Only east/north projected axes are qualified.")
  if(!identical(j$base_crs$type,"GeographicCRS") || length(j$base_crs$coordinate_system$axis)!=2L)
    stop("A two-dimensional geodetic base is required.")
  crs <- validate_study_analysis_crs(sf::st_crs(as.character(jsonlite::toJSON(j,auto_unbox=TRUE,digits=NA,null="null")))$wkt)
  if(is.null(j$base_crs$id) || is.null(j$base_crs$datum) || !identical(j$base_crs$datum$type,"GeodeticReferenceFrame"))
    stop("An identified static geodetic reference is required.")
  list(wkt=crs$wkt,base=j$base_crs)
}

.fg_horizontal_plan <- function(source,template,max_cells,output_type="Float32") {
  a <- .fg_horizontal_info(source); b <- .fg_horizontal_info(template)
  s <- .fg_horizontal_crs(a,compound=TRUE); t <- .fg_horizontal_crs(b)
  base_crs <- function(j) sf::st_crs(as.character(jsonlite::toJSON(j,auto_unbox=TRUE,digits=NA,null="null")))
  if(!identical(s$base$id,t$base$id) || !isTRUE(base_crs(s$base)==base_crs(t$base)))
    stop("Geodetic reference changes require separate operation qualification.")
  if(length(max_cells)!=1L || !is.finite(max_cells) || max_cells<=0 ||
      prod(a$size)+prod(b$size)>max_cells) stop("Horizontal processing exceeds the cell budget.")
  candidates <- as.data.frame(sf::sf_proj_pipelines(s$wkt,t$wkt,desired_accuracy=0,
    grid_availability="DISCARD",axis_order_authority_compliant=TRUE))
  acceptable <- which(candidates$instantiable & candidates$accuracy==0 & candidates$grid_count==0 &
    !grepl("ballpark|helmert|gridshift|deformation|z_in|z_out",paste(candidates$description,candidates$definition),ignore.case=TRUE))
  if(length(acceptable)!=1L) stop("A unique exact, grid-free horizontal operation is required.")
  operation <- candidates[acceptable,,drop=FALSE]
  at <- a$transform; bt <- b$transform
  same_crs <- isTRUE(sf::st_crs(s$wkt)==sf::st_crs(t$wkt))
  offsets <- (bt[c(1,4)]-at[c(1,4)])/at[c(2,6)]
  aligned <- same_crs && all(abs(at[c(2,6)]-bt[c(2,6)])<1e-10*abs(at[c(2,6)])) &&
    all(abs(offsets-round(offsets))<1e-7)
  extent <- c(bt[1],bt[4]+b$size[2]*bt[6],bt[1]+b$size[1]*bt[2],bt[4])
  number <- function(x) format(x,digits=17,scientific=FALSE,trim=TRUE)
  options <- c("-of","GTiff","-s_srs",s$wkt,"-t_srs",t$wkt,"-ct",operation$definition,
    "-novshift","-r",if(aligned) "near" else "bilinear","-ot",output_type,"-wt","Float64",
    "-te",number(extent),"-ts",as.character(b$size),"-et","0","-ovr","NONE",
    "-dstnodata","nan","-wm","64","-wo","NUM_THREADS=1",
    "-co","COMPRESS=DEFLATE","-co","TILED=YES","-co","BIGTIFF=IF_SAFER",
    "-oo","GEOREF_SOURCES=INTERNAL")
  list(source=a,template=b,source_horizontal=s$wkt,target_horizontal=t$wkt,
    operation=operation,aligned=aligned,output_type=output_type,
    resampling=if(aligned) "near" else "bilinear",options=options)
}

.fg_horizontal_scan <- function(path) {
  r <- terra::rast(path)
  terra::readStart(r);on.exit(terra::readStop(r),add=TRUE)
  count <- 0;minimum <- Inf;maximum <- -Inf
  .fg_mask_blocks(r,function(start,nrows) {
    v <- terra::readValues(r,row=start,nrows=nrows)
    if(any(is.infinite(v))) stop("Infinite terrain samples require review.")
    valid <- v[!is.na(v)]
    count <<- count+length(valid)
    if(length(valid)) {minimum <<- min(minimum,valid);maximum <<- max(maximum,valid)}
  })
  list(valid_cells=count,minimum=if(count) minimum else NA_real_,maximum=if(count) maximum else NA_real_)
}

.fg_horizontal_compare_aligned <- function(source,output,plan) {
  a <- terra::rast(source);b <- terra::rast(output)
  terra::readStart(a);on.exit(terra::readStop(a),add=TRUE)
  terra::readStart(b);on.exit(terra::readStop(b),add=TRUE)
  offset <- round((plan$template$transform[c(1,4)]-plan$source$transform[c(1,4)])/plan$source$transform[c(2,6)])
  .fg_mask_blocks(b,function(start,nrows) {
    expected <- matrix(NA_real_,nrow=nrows,ncol=ncol(b))
    source_rows <- seq(start,length.out=nrows)+offset[2]
    source_cols <- seq_len(ncol(b))+offset[1]
    rows <- which(source_rows>=1 & source_rows<=nrow(a))
    cols <- which(source_cols>=1 & source_cols<=ncol(a))
    if(length(rows) && length(cols)) expected[rows,cols] <- matrix(terra::readValues(a,
      row=source_rows[rows[1]],nrows=length(rows),col=source_cols[cols[1]],ncols=length(cols)),
      nrow=length(rows),byrow=TRUE)
    expected <- as.vector(t(expected))
    if(plan$output_type=="Float32")
      expected <- readBin(writeBin(expected,raw(),size=4),"double",n=length(expected),size=4)
    actual <- terra::readValues(b,row=start,nrows=nrows)
    if(any(is.na(actual)!=is.na(expected)) || any(actual[!is.na(actual)]!=expected[!is.na(expected)]))
      stop("Aligned output samples differ from source pixels.")
  })
  invisible(TRUE)
}

#' Warp terrain on a qualified static horizontal grid without elevation conversion
#' @param source Local single-band native GeoTIFF, with identity scale/offset.
#' @param template Local projected 2D GeoTIFF defining the exact output grid.
#'   Its pixel values are ignored; this function does not apply a mask.
#' @param directory New nonexistent output directory under an existing parent.
#' @param max_cells Combined source/output cell budget, default 50 million.
#' @param output_type Storage type: Float32 by default, or explicit Float64.
#'   Float32 rounds samples to single precision, including higher-precision inputs.
#' @return Verification manifest, also written last as verified.json beside terrain.tif.
#' @details This bounded backend primitive accepts static east/north projected
#'   CRSs with identical identified geodetic references and a unique exact,
#'   grid-free PROJ operation. Compound source vertical declarations are retained
#'   in evidence; an extracted horizontal definition and explicit GDAL -novshift
#'   prevent implicit height operations. Dynamic frames, datum changes, external
#'   sidecars, rotated grids and nonidentity scale/offset are refused. Aligned
#'   grids use nearest sampling; others use bilinear interpolation, which changes
#'   samples but performs no elevation-unit or vertical-datum conversion. Output
#'   defaults to Float32 storage with Float64 working precision. Aligned samples
#'   are checked exactly after conversion to the selected storage precision.
#'   Float64 storage is optional. Original bytes are rehashed.
#'   GDAL warp/cache budgets are each 64 MiB, not a total process-memory limit.
#'   Pixel checks use bounded row blocks.
#'   Failed/interrupted attempts have no verified manifest and are never overwritten.
#'   This is not a Stream mosaic, scientific acceptance, or source-reference
#'   reconciliation. Assemble compatible tiles with interpolation halos before
#'   using it and apply masks afterwards. Applications must run it in a worker
#'   and check current revisions before publishing the attempt.
#' @export
warp_terrain_horizontal <- function(source,template,directory,max_cells=5e7,
                                    output_type=c("Float32","Float64")) {
  output_type <- match.arg(output_type)
  if(length(directory)!=1L || is.na(directory) || file.exists(directory) || !dir.exists(dirname(directory)))
    stop("Supply a new horizontal processing attempt directory.")
  plan <- .fg_horizontal_plan(source,template,max_cells,output_type)
  required <- 2*ifelse(output_type=="Float32",4,8)*prod(plan$template$size)+256*1024^2
  available <- ps::ps_disk_usage(dirname(directory))$available[1]
  if(!is.finite(available) || available<required) stop("Insufficient available disk space for horizontal processing.")
  source_pixels <- .fg_horizontal_scan(source)
  if(output_type=="Float32" && source_pixels$valid_cells>0 &&
      max(abs(c(source_pixels$minimum,source_pixels$maximum)))>(2-2^-23)*2^127)
    stop("Source samples exceed Float32 range; select Float64 storage.")
  if(!dir.create(directory)) stop("Cannot create horizontal processing attempt.")
  output <- file.path(directory,"terrain.tif")
  config <- c(GDAL_PAM_ENABLED="NO",GTIFF_REPORT_COMPD_CS="TRUE",PROJ_NETWORK="OFF",GDAL_CACHEMAX="64")
  success <- sf::gdal_utils("warp",source,output,options=plan$options,config_options=config,quiet=TRUE)
  if(!isTRUE(success)) stop("GDAL horizontal processing did not complete successfully.")
  observed <- .fg_horizontal_info(output)
  if(!identical(observed$size,plan$template$size) ||
      any(abs(observed$transform-plan$template$transform)>1e-8) ||
      !isTRUE(sf::st_crs(observed$observation$internal_compound$wkt)==sf::st_crs(plan$target_horizontal)) ||
      !identical(observed$info$bands[[1L]]$type,output_type) ||
      !identical(observed$observation$internal_compound$band_unit,plan$source$observation$internal_compound$band_unit))
    stop("Reopened horizontal output metadata differs from the plan.")
  pixels <- .fg_horizontal_scan(output)
  if(plan$aligned) .fg_horizontal_compare_aligned(source,output,plan)
  if(!identical(.fg_dem_hash(source),plan$source$observation$sha256) ||
      !identical(.fg_dem_hash(template),plan$template$observation$sha256)) stop("Horizontal processing inputs changed.")
  if(!identical(.fg_dem_hash(output),observed$observation$sha256)) stop("Horizontal output changed during verification.")
  observed$observation$path <- "terrain.tif"
  manifest <- list(schema="HORIZONTAL_TERRAIN_WARP_1",created_at=.fg_dem_time(),
    source=plan$source$observation,template=plan$template$observation,
    source_band=plan$source$info$bands[[1L]],
    source_horizontal=plan$source_horizontal,target_horizontal=plan$target_horizontal,
    operation=plan$operation,resampling=plan$resampling,aligned_samples_verified=plan$aligned,
    output_type=output_type,working_type="Float64",
    options=plan$options,config=as.list(config),
    vertical_operation="none; source declarations retained in evidence",elevation_unit_conversion="none",
    source_pixels=source_pixels,output=list(file="terrain.tif",sha256=.fg_dem_hash(output),pixels=pixels,
      observation=observed$observation),required_bytes=required,available_bytes=available,
    software=list(terra=as.character(utils::packageVersion("terra")),
      sf=as.character(utils::packageVersion("sf")),fluvgeo=as.character(utils::packageVersion("fluvgeo")),
      geospatial=as.list(sf::sf_extSoftVersion())),scientific_acceptance=FALSE)
  .fg_dem_json(manifest,file.path(directory,"verified.json"))
  manifest
}
