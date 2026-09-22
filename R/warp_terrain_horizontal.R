.fg_horizontal_info <- function(path,template=FALSE) {
  observation <- inspect_terrain_vertical_reference(path)
  stem <- tools::file_path_sans_ext(path)
  sidecars <- c(paste0(path,c(".aux.xml",".msk")),paste0(stem,c(".tfw",".tifw",".wld")))
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
  if(length(t)!=6L || any(!is.finite(t)) || t[2]*t[6]-t[3]*t[5]==0 ||
      length(size)!=2L || any(!is.finite(size)) || any(size<=0) ||
      !is.null(info$gcps))
    stop("A valid affine raster grid is required; GCP-only georeferencing is not supported here.")
  if(template && (t[2]<=0 || t[6]>=0 || t[3]!=0 || t[5]!=0))
    stop("The output template must be a north-up raster grid.")
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
  a <- .fg_horizontal_info(source); b <- .fg_horizontal_info(template,template=TRUE)
  s <- .fg_horizontal_crs(a,compound=TRUE); t <- .fg_horizontal_crs(b)
  base_crs <- function(j) sf::st_crs(as.character(jsonlite::toJSON(j,auto_unbox=TRUE,digits=NA,null="null")))
  if(!identical(s$base$id,t$base$id) || !isTRUE(base_crs(s$base)==base_crs(t$base)))
    stop("Geodetic reference changes require separate operation qualification.")
  candidates <- as.data.frame(sf::sf_proj_pipelines(s$wkt,t$wkt,desired_accuracy=0,
    grid_availability="DISCARD",axis_order_authority_compliant=TRUE))
  acceptable <- which(candidates$instantiable & candidates$accuracy==0 & candidates$grid_count==0 &
    !grepl("ballpark|helmert|gridshift|deformation|z_in|z_out",paste(candidates$description,candidates$definition),ignore.case=TRUE))
  if(length(acceptable)!=1L) stop("A unique exact, grid-free horizontal operation is required.")
  operation <- candidates[acceptable,,drop=FALSE]
  at <- a$transform; bt <- b$transform
  same_crs <- isTRUE(sf::st_crs(s$wkt)==sf::st_crs(t$wkt))
  offsets <- (bt[c(1,4)]-at[c(1,4)])/at[c(2,6)]
  aligned <- same_crs && at[3]==0 && at[5]==0 &&
    all(abs(at[c(2,6)]-bt[c(2,6)])<1e-10*abs(at[c(2,6)])) &&
    all(abs(offsets-round(offsets))<1e-7)
  extent <- c(bt[1],bt[4]+b$size[2]*bt[6],bt[1]+b$size[1]*bt[2],bt[4])
  number <- function(x) format(x,digits=17,scientific=FALSE,trim=TRUE)
  options <- c("-of","GTiff","-s_srs",s$wkt,"-t_srs",t$wkt,"-ct",operation$definition,
    "-novshift","-r",if(aligned) "near" else "bilinear","-ot",output_type,
    "-te",number(extent),"-ts",as.character(b$size),"-et","0","-ovr","NONE",
    "-dstnodata","nan",
    "-co","COMPRESS=DEFLATE","-co","TILED=YES","-co","BIGTIFF=IF_SAFER",
    "-oo","GEOREF_SOURCES=INTERNAL")
  list(source=a,template=b,source_horizontal=s$wkt,target_horizontal=t$wkt,
    operation=operation,aligned=aligned,output_type=output_type,
    resampling=if(aligned) "near" else "bilinear",options=options)
}

.fg_horizontal_scan <- function(path) {
  # Native summaries stream file-backed data; no R cell traversal or matrices.
  summary <- terra::global(terra::rast(path),c("min","max","notNA"),na.rm=TRUE)
  count <- as.numeric(summary$notNA[1])
  if(count > 0 && any(is.infinite(c(summary$min[1],summary$max[1]))))
    stop("Infinite terrain samples require review.")
  list(valid_cells=count,minimum=if(count) summary$min[1] else NA_real_,
    maximum=if(count) summary$max[1] else NA_real_)
}
#' Warp terrain on a qualified static horizontal grid without elevation conversion
#' @param source Local single-band native GeoTIFF, with identity scale/offset.
#' @param template Local projected 2D GeoTIFF defining the exact output grid.
#'   Its pixel values are ignored; this function does not apply a mask.
#' @param directory New nonexistent output directory under an existing parent.
#' @param max_cells Deprecated compatibility argument; ignored. No cell-count limit.
#' @param output_type Storage type: Float32 by default, or explicit Float64.
#'   Float32 rounds samples to single precision, including higher-precision inputs.
#' @return Verification manifest, also written last as verified.json beside terrain.tif.
#' @details This backend primitive accepts static east/north projected
#'   CRSs with identical identified geodetic references and a unique exact,
#'   grid-free PROJ operation. Compound source vertical declarations are retained
#'   in evidence; an extracted horizontal definition and explicit GDAL -novshift
#'   prevent implicit height operations. Dynamic frames, datum changes, external
#'   georeferencing/mask sidecars and nonidentity scale/offset are refused. Rotated
#'   and unequal-spacing affine source grids are handled by GDAL. Aligned
#'   grids use nearest sampling; others use bilinear interpolation, which changes
#'   samples but performs no elevation-unit or vertical-datum conversion. Output
#'   defaults to Float32 storage; GDAL selects working precision and memory settings.
#'   Native summaries check the value range and count. Exact pixel comparisons
#'   belong to small reference tests, not an additional production cell audit.
#'   Float64 storage is optional. Original bytes are rehashed. External overviews
#'   are allowed, but analytical warping uses original pixels (-ovr NONE) to avoid
#'   inheriting an unknown overview resampling method. No application cell-count,
#'   row-width or estimated-disk admission limit is imposed.
#'   Failed/interrupted attempts have no verified manifest and are never overwritten.
#'   This is not a Stream mosaic, scientific acceptance, or source-reference
#'   reconciliation. Assemble compatible tiles with interpolation halos before
#'   using it and apply masks afterwards. Applications must run it in a worker
#'   and check current revisions before publishing the attempt.
#' @export
warp_terrain_horizontal <- function(source,template,directory,max_cells=NULL,
                                    output_type=c("Float32","Float64")) {
  output_type <- match.arg(output_type)
  if(length(directory)!=1L || is.na(directory) || file.exists(directory) || !dir.exists(dirname(directory)))
    stop("Supply a new horizontal processing attempt directory.")
  plan <- .fg_horizontal_plan(source,template,max_cells,output_type)
  source_pixels <- .fg_horizontal_scan(source)
  if(output_type=="Float32" && source_pixels$valid_cells>0 &&
      max(abs(c(source_pixels$minimum,source_pixels$maximum)))>(2-2^-23)*2^127)
    stop("Source samples exceed Float32 range; select Float64 storage.")
  if(!dir.create(directory)) stop("Cannot create horizontal processing attempt.")
  output <- file.path(directory,"terrain.tif")
  config <- c(GDAL_PAM_ENABLED="NO",GTIFF_REPORT_COMPD_CS="TRUE",PROJ_NETWORK="OFF")
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
  if(!identical(.fg_dem_hash(source),plan$source$observation$sha256) ||
      !identical(.fg_dem_hash(template),plan$template$observation$sha256)) stop("Horizontal processing inputs changed.")
  if(!identical(.fg_dem_hash(output),observed$observation$sha256)) stop("Horizontal output changed during verification.")
  observed$observation$path <- "terrain.tif"
  manifest <- list(schema="HORIZONTAL_TERRAIN_WARP_1",created_at=.fg_dem_time(),
    source=plan$source$observation,template=plan$template$observation,
    source_band=plan$source$info$bands[[1L]],
    source_horizontal=plan$source_horizontal,target_horizontal=plan$target_horizontal,
    operation=plan$operation,resampling=plan$resampling,aligned_samples_verified=FALSE,
    output_type=output_type,working_type="GDAL default",
    options=plan$options,config=as.list(config),
    vertical_operation="none; source declarations retained in evidence",elevation_unit_conversion="none",
    source_pixels=source_pixels,output=list(file="terrain.tif",sha256=observed$observation$sha256,pixels=pixels,
      observation=observed$observation),
    software=list(terra=as.character(utils::packageVersion("terra")),
      sf=as.character(utils::packageVersion("sf")),fluvgeo=as.character(utils::packageVersion("fluvgeo")),
      geospatial=as.list(sf::sf_extSoftVersion())),scientific_acceptance=FALSE)
  .fg_dem_json(manifest,file.path(directory,"verified.json"))
  manifest
}
