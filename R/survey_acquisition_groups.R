#' Propose acquisition months from retained provider evidence
#' @param discovery Saved Survey Collection discovery with selected keys.
#' @return One row per selected collection, preserving date evidence and a
#'   proposed month only for complete, ordered dates in one calendar month.
#' @export
propose_survey_acquisition_groups <- function(discovery) {
  x <- sf::st_drop_geometry(discovery$records)
  x <- x[x$candidate_key %in% discovery$selected,,drop=FALSE]
  rows <- lapply(seq_len(nrow(x)), function(i) {
    raw <- tryCatch(jsonlite::fromJSON(x$raw_metadata[i]), error=function(e) NULL)
    field <- function(n) {
      v <- tryCatch(raw[[n]],error=function(e) NULL)
      if(length(v)!=1L || is.na(v)) NA_character_ else as.character(v)
    }
    start <- end <- NA_character_
    evidence <- if(x$catalog[i]=="USGS 3DEP") {
      a <- field("collect_start"); b <- field("collect_end")
      epoch <- function(v) {
        n <- suppressWarnings(as.numeric(v))
        if(!is.finite(n)) return(NA_character_)
        as.character(as.Date(as.POSIXct(n/1000,origin="1970-01-01",tz="UTC")))
      }
      start <- epoch(a); end <- epoch(b)
      paste("collect_start:",a,"collect_end:",b)
    } else if(x$catalog[i]=="USIEI") {
      v <- field("collectiondate")
      # Only explicit ISO day endpoints are interpreted; free text needs review.
      if(!is.na(v) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",v)) start <- end <- v
      if(!is.na(v) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} to [0-9]{4}-[0-9]{2}-[0-9]{2}$",v)) {
        parts <- strsplit(v," to ",fixed=TRUE)[[1]]; start <- parts[1]; end <- parts[2]
      }
      paste("collectiondate:",v)
    } else "Unsupported acquisition evidence; review required"
    valid_date <- function(v) !is.na(v) && isTRUE(tryCatch(
      identical(format(as.Date(v),"%Y-%m-%d"),v),error=function(e) FALSE))
    month <- NA_character_
    if(valid_date(start) && valid_date(end) && start<=end && substr(start,1,7)==substr(end,1,7))
      month <- substr(start,1,7)
    expected_label <- if(x$catalog[i]=="USGS 3DEP") paste(start,end,sep=" to ") else field("collectiondate")
    if(!is.na(expected_label) && !identical(x$date_label[i],expected_label)) month <- NA_character_
    data.frame(candidate_key=x$candidate_key[i],title=x$title[i],catalog=x$catalog[i],
      record_id=x$record_id[i],snapshot_id=x$snapshot_id[i],date_label=x$date_label[i],
      acquisition_evidence=evidence,start=start,end=end,proposed_month=month,
      review=if(is.na(month)) "Unresolved: review acquisition evidence" else "Proposed: confirm membership",
      raw_metadata=x$raw_metadata[i])
  })
  if(length(rows)) do.call(rbind,rows) else data.frame(candidate_key=character(),
    title=character(),catalog=character(),record_id=character(),snapshot_id=character(),
    date_label=character(),acquisition_evidence=character(),start=character(),end=character(),
    proposed_month=character(),review=character(),raw_metadata=character())
}

#' Save reviewed local acquisition group and Event spacing
#' @param context Existing study context GeoPackage path.
#' @param selection Saved Survey Collection selection GeoPackage path.
#' @param members Selected candidate keys belonging to this group.
#' @param stream_ids Applicable saved Stream IDs.
#' @param year Known acquisition year (required).
#' @param month Acquisition month or NA for year precision.
#' @param cell_size Positive finite output spacing in the saved planar CRS unit.
#' @param rationale Analyst evidence for reviewed membership and date precision.
#' @param dsn New GeoPackage destination; never replaced.
#' @param previous Previous group snapshot, or NULL for a new stable identity.
#' @param event_ids Existing Reach-owned Survey Event IDs, or character().
#' @return New snapshot path invisibly. Does not create FGDB Events or rasters.
#' @export
write_survey_acquisition_group <- function(context, selection, members, stream_ids,
    year, month=NA_integer_, cell_size, rationale, dsn, previous=NULL,
    event_ids=character()) {
  ctx <- read_study_context(context); discovery <- read_survey_collection_selection(selection)
  id <- ctx$study_area$study_area_id
  if(!identical(id,discovery$study_area$study_area_id)) stop("Selection belongs to another study.")
  .fg_survey_boundary(ctx$study_area)
  if(!isTRUE(lengths(sf::st_equals(ctx$study_area,sf::st_transform(discovery$study_area,
      sf::st_crs(ctx$study_area))))[1]==1L)) stop("Selection boundary changed; review again.")
  keys <- function(v, allowed, label, required=TRUE) {
    if(!is.character(v) || anyNA(v) || anyDuplicated(v) ||
        (required && !length(v)) || !all(v %in% allowed)) stop(paste("Choose valid",label))
  }
  keys(members,discovery$selected,"selected collections.")
  keys(stream_ids,ctx$streams$stream_id,"Streams.")
  keys(event_ids,ctx$survey_events$survey_event_id,"Reach Events.",FALSE)
  events <- ctx$survey_events[match(event_ids,ctx$survey_events$survey_event_id),,drop=FALSE]
  if(length(event_ids) && !all(events$reach_id %in% ctx$reaches$reach_id[ctx$reaches$stream_id %in% stream_ids]))
    stop("Reach Event must belong to a selected Stream.")
  integer_in <- function(v,a,b) is.numeric(v) && length(v)==1L && !is.na(v) && is.finite(v) && v==floor(v) && v>=a && v<=b
  if(!integer_in(year,1,9999)) stop("A known acquisition year is required.")
  if(length(month)!=1L || (!is.na(month) && !integer_in(month,1,12))) stop("Month must be 1 to 12 or unknown.")
  if(length(event_ids) && (any(events$survey_year!=year) ||
      any(!is.na(events$survey_month) & (is.na(month) | events$survey_month!=month))))
    stop("Reach Event dates conflict with the reviewed group.")
  if(!is.numeric(cell_size) || length(cell_size)!=1L || !is.finite(cell_size) || cell_size<=0)
    stop("Choose one positive finite output cell size.")
  if(!is.character(rationale) || length(rationale)!=1L || is.na(rationale) || !nzchar(trimws(rationale)))
    stop("Document the acquisition membership and date evidence.")
  ref <- ctx$analysis_reference
  at <- which(ref$component=="horizontal" & ref$basis=="PROJECT_RECORD")
  if(length(at)!=1L) stop("Save the Study Area analysis CRS first.")
  crs <- validate_study_analysis_crs(ref$value[at],ctx$study_area)
  old <- if(!is.null(previous)) read_survey_acquisition_group(previous) else NULL
  if(!is.null(old) && !identical(old$settings$study_area_id,id)) stop("Previous group belongs to another study.")
  settings <- data.frame(schema="SURVEY_ACQUISITION_GROUP_1",study_area_id=id,
    group_id=if(is.null(old)) .fg_generate_uuid(1L) else old$settings$group_id,
    year=as.integer(year),month=as.integer(month),date_precision=if(is.na(month)) "year" else "month",
    cell_size=as.numeric(cell_size),unit=crs$unit,wkt=crs$wkt,anchor_x=0,anchor_y=0,
    rationale=trimws(rationale),context_revision=basename(context),selection_revision=basename(selection),
    saved_at=format(Sys.time(),tz="UTC",usetz=TRUE))
  evidence <- propose_survey_acquisition_groups(discovery)
  evidence <- evidence[match(members,evidence$candidate_key),,drop=FALSE]
  dsn <- .fg_network_dsn(dsn)
  if(file.exists(dsn)) stop("Group destination already exists.")
  stage <- tempfile("acquisition-group-",tmpdir=dirname(dsn),fileext=".gpkg")
  on.exit(unlink(stage),add=TRUE)
  sf::st_write(settings,stage,layer="settings",quiet=TRUE)
  sf::st_write(evidence,stage,layer="members",quiet=TRUE)
  sf::st_write(data.frame(stream_id=stream_ids),stage,layer="streams",quiet=TRUE)
  sf::st_write(data.frame(survey_event_id=event_ids),stage,layer="event_links",quiet=TRUE)
  check <- read_survey_acquisition_group(stage)
  if(!identical(check$members$candidate_key,members) || !isTRUE(all.equal(check$settings$cell_size,as.numeric(cell_size))))
    stop("Group round trip failed.")
  if(!isTRUE(suppressWarnings(file.link(stage,dsn)))) stop("Could not publish group without replacement.")
  invisible(dsn)
}

#' Read a local acquisition group without changing its evidence
#' @param dsn Existing acquisition group GeoPackage.
#' @return List of settings, members, streams and event_links tables.
#' @export
read_survey_acquisition_group <- function(dsn) {
  result <- lapply(c("settings","members","streams","event_links"),function(layer)
    sf::st_read(dsn,layer=layer,quiet=TRUE))
  names(result) <- c("settings","members","streams","event_links")
  s <- result$settings
  if(nrow(s)!=1L || !identical(s$schema,"SURVEY_ACQUISITION_GROUP_1") ||
      !is.finite(s$cell_size) || s$cell_size<=0 || s$anchor_x!=0 || s$anchor_y!=0)
    stop("Invalid acquisition group schema or grid settings.")
  Encoding(result$settings$wkt) <- "UTF-8"
  result
}
