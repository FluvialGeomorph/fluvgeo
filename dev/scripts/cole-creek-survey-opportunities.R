# Workspace-root offline demo; public snapshots are prepared separately.
# Usage: Rscript .../cole-creek-survey-opportunities.R <snapshot-directory> <new.html> [terrain-review.rds]
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args) %in% c(2L,3L),!file.exists(args[2]))
pkgload::load_all('fluvgeo',quiet=TRUE)
huc <- sf::st_read('fluvgeodata/inst/extdata/NWO_Papillion_ColeCreek_Stream.gdb',
  layer='Papillion_HUC12',quiet=TRUE)
study <- sf::st_sf(study_area_id='papillion-demo',study_area_name='NWO_Papillion',
  geometry=sf::st_union(sf::st_geometry(sf::st_transform(huc,26914))))
focus <- sf::st_read('fluvgeodata/inst/extdata/y2010_R1.gdb',layer='flowline',quiet=TRUE)
wesm_file <- file.path(args[1],'wesm-study.geojson')
usiei_file <- file.path(args[1],'usiei-study.geojson')
sha256 <- function(path) {
  con <- file(path,'rb'); on.exit(close(con),add=TRUE)
  toupper(as.character(openssl::sha256(con)))
}
stopifnot(sha256(wesm_file)=='D1B23596548D7E839FEF352FDF1810DE6ADBC0C824D6669BCA5B9783FD54E848',
  sha256(usiei_file)=='B46D9096F1B7545FE0154F09021F36E3025A7260D420050AA93024E38B4BBDC4')
w <- sf::st_read(wesm_file,quiet=TRUE)
u <- sf::st_read(usiei_file,quiet=TRUE)
epoch_date <- function(x) as.Date(as.POSIXct(x/1000,origin='1970-01-01',tz='UTC'))
w_rows <- sf::st_sf(catalog='3DEP index',record_id=as.character(w$workunit_id),
  snapshot_id='papillion-2026-09-12',title=w$workunit,
  collection_start=epoch_date(w$collect_start),collection_end=epoch_date(w$collect_end),
  date_label=paste(epoch_date(w$collect_start),epoch_date(w$collect_end),sep=' to '),
  status='COMPLETE',metadata_url=w$metadata_link,geometry=sf::st_geometry(w))
# Reviewed date bounds for THIS frozen fixture, not a generic free-text parser.
# Month/year-only dates use enclosing bounds and retain the literal source label.
ids <- c(23873,23976,24256,24325,24913,24914,25140,25404,33061,34504,
         39542,41287,47472,47473,47476,47477,54494)
starts <- c('2016-12-08','2004-04-01','2010-03-01','2007-01-01','2011-12-01',
  '2012-01-25','2012-04-01','2013-12-01','2019-04-13','2019-05-25',
  '2013-06-01','2020-11-27',rep('2022-03-29',4),NA)
ends <- c('2017-02-03','2004-04-30','2010-04-30','2009-12-31','2011-12-31',
  '2012-03-18','2012-06-30','2014-03-31','2020-02-21','2019-07-13',
  '2013-06-30','2021-04-01',rep('2022-04-14',4),NA)
stopifnot(setequal(u$ID,ids))
idx <- match(u$ID,ids)
metadata <- vapply(seq_len(nrow(u)),function(i) {
  links <- if (!is.na(u$Links[i]) && nzchar(u$Links[i])) jsonlite::fromJSON(u$Links[i])$links else NULL
  if (!is.null(links)) {
    candidates <- links$link[links$linktype=='Metadata']
    candidates <- candidates[grepl('^https?://[^[:space:]]+$',candidates)]
    if (length(candidates)) return(candidates[1])
  }
  paste0('https://maps.coast.noaa.gov/arcgis/rest/services/USInteragencyElevationInventory/USIEIv2/MapServer/2/query?where=ID%3D',u$ID[i],'&outFields=*&f=pjson')
},character(1))
u_rows <- sf::st_sf(catalog='USIEI',record_id=as.character(u$ID),
  snapshot_id='papillion-2026-09-12',title=u$Title,
  collection_start=as.Date(starts[idx]),collection_end=as.Date(ends[idx]),
  date_label=trimws(u$collectiondate),status=ifelse(u$Status=='Complete','COMPLETE','PLANNED'),
  metadata_url=metadata,geometry=sf::st_geometry(u))
# Preserve raw service links/labels in the saved snapshot; no inferred source links.
events <- data.frame(event_id=paste0('cole-',c(2006,2010,2016)),
  label=paste('Cole Creek R1',c(2006,2010,2016)),
  collection_start=as.Date(paste0(c(2006,2010,2016),'-01-01')),
  collection_end=as.Date(paste0(c(2006,2010,2016),'-12-31')),
  date_label=paste(c(2006,2010,2016),'(year only; source lineage unresolved)'))
searches <- data.frame(catalog=c('3DEP index','USIEI'),snapshot_id='papillion-2026-09-12',
  retrieved_at=c('2026-09-12T16:11:24Z','2026-09-12T16:11:25Z'),
  outcome='COMPLETE',scope_note=c(
  'Published lidar layer 24; Study Area bounding-envelope query; 9 records count-checked. Not all USGS holdings.',
  'Topographic lidar layer 2; same envelope; 17 records count-checked. Other USIEI layers/providers not searched.'))
summary <- survey_opportunity_summary(study,events,rbind(w_rows,u_rows),searches,
  focus=focus,focus_label='Cole Creek R1 flowline within NWO_Papillion; only retained 2006/2010/2016 events are compared. The 2006 history references a 2004 source: unresolved, not relabeled.',
  terrain_review=if (length(args)==3L) readRDS(args[3]) else NULL)
survey_opportunity_report(summary,args[2])
print(sf::st_drop_geometry(summary$records)[c('review_id','catalog','title','classification','spatial_relation')])
