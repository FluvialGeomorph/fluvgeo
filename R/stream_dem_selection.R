#' Save explicit Stream DEM file choices
#' @param inventory Result of discover_stream_dem_files().
#' @param selected Character file IDs from that result; empty selection is allowed.
#' @param dsn New GeoPackage path; existing files are never replaced.
#' @param context_revision Opaque caller context revision label.
#' @return Path invisibly. Records intent, not downloads or accepted terrain.
#' @export
write_stream_dem_selection <- function(inventory,selected,dsn,context_revision) {
  f <- inventory$files
  if(!inventory$outcome %in% c("COMPLETE","PARTIAL") || !inherits(f,"sf") ||
      !all(c("file_id","download_url","raw_metadata") %in% names(f)) || anyNA(f$file_id) || anyDuplicated(f$file_id) ||
      !is.character(selected) || anyNA(selected) || anyDuplicated(selected) || !all(selected %in% f$file_id))
    stop("Select only file IDs from a successful reviewed inventory.",call.=FALSE)
  if(!inherits(inventory$stream,"sf") || nrow(inventory$stream)!=1L ||
      !inherits(inventory$collection,"sf") || nrow(inventory$collection)!=1L ||
      !"stream_id" %in% names(inventory$stream) || !"candidate_key" %in% names(inventory$collection))
    stop("Stream and collection evidence are required.",call.=FALSE)
  if(!is.character(context_revision) || length(context_revision)!=1L || is.na(context_revision) || !nzchar(context_revision))
    stop("Context revision is required.",call.=FALSE)
  dsn <- .fg_network_dsn(dsn)
  if(file.exists(dsn)) stop("File selection destination already exists.",call.=FALSE)
  stage <- tempfile("dem-selection-",tmpdir=dirname(dsn),fileext=".gpkg");on.exit(unlink(stage),add=TRUE)
  f$selected <- as.integer(f$file_id %in% selected)
  sf::st_write(inventory$stream,stage,layer="stream",quiet=TRUE)
  sf::st_write(inventory$collection,stage,layer="collection",quiet=TRUE)
  sf::st_write(f,stage,layer="files",quiet=TRUE)
  sf::st_write(data.frame(schema="STREAM_DEM_SELECTION_1",context_revision=context_revision,
    outcome=inventory$outcome,message=inventory$message,retrieved_at=inventory$retrieved_at,
    endpoint=inventory$endpoint,saved_at=format(Sys.time(),tz="UTC",usetz=TRUE)),stage,layer="metadata",quiet=TRUE)
  check <- read_stream_dem_selection(stage)
  if(!setequal(check$selected,selected) || nrow(check$files)!=nrow(f)) stop("File selection round trip failed.")
  if(!isTRUE(suppressWarnings(file.link(stage,dsn)))) stop("Could not publish file choices without replacement.")
  invisible(dsn)
}

#' Read saved Stream DEM file choices without network access
#' @param dsn Existing selection GeoPackage.
#' @return Inventory plus selected IDs and context_revision. Does not validate current study state.
#' @export
read_stream_dem_selection <- function(dsn) {
  m <- sf::st_read(dsn,layer="metadata",quiet=TRUE)
  if(nrow(m)!=1L || !identical(m$schema,"STREAM_DEM_SELECTION_1")) stop("Unknown DEM selection schema.")
  f <- sf::st_read(dsn,layer="files",quiet=TRUE)
  list(stream=sf::st_read(dsn,layer="stream",quiet=TRUE),collection=sf::st_read(dsn,layer="collection",quiet=TRUE),
    files=f[,setdiff(names(f),"selected"),drop=FALSE],selected=f$file_id[f$selected==1L],
    context_revision=m$context_revision,outcome=m$outcome,message=m$message,retrieved_at=m$retrieved_at,endpoint=m$endpoint)
}
