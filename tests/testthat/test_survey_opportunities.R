opportunity_fixture <- function() {
  box <- function(x=0,y=0,w=10) sf::st_polygon(list(matrix(
    c(x,y,x+w,y,x+w,y+w,x,y+w,x,y),ncol=2,byrow=TRUE)))
  study <- sf::st_sf(study_area_id='s1',study_area_name='Synthetic study',
    geometry=sf::st_sfc(box(),crs=26914))
  events <- data.frame(event_id=c('e1','e2'),label=c('2006','2016'),
    collection_start=as.Date(c('2006-01-01','2016-01-01')),
    collection_end=as.Date(c('2006-12-31','2016-12-31')),date_label=c('2006 (year only)','2016 (year only)'))
  dates <- c('2004-04-01','2013-06-01','2022-03-29','2016-12-08',NA,'2026-01-01')
  records <- sf::st_sf(catalog=rep('A',6),record_id=as.character(seq_len(6)),
    snapshot_id=rep('snap',6),title=paste('Synthetic',seq_len(6)),
    collection_start=as.Date(dates),collection_end=as.Date(dates),
    date_label=ifelse(is.na(dates),'Unknown',dates),
    status=c(rep('COMPLETE',5),'PLANNED'),metadata_url=rep('https://example.org',6),
    geometry=sf::st_sfc(rep(list(box()),6),crs=26914))
  searches <- data.frame(catalog='A',snapshot_id='snap',retrieved_at='2026-09-12T00:00:00Z',
    outcome='COMPLETE',scope_note='Synthetic full test query, not a real collection')
  list(study_area=study,survey_events=events,catalog_records=records,searches=searches)
}

test_that('acquisition intervals distinguish candidate roles without asserting lineage', {
  args <- opportunity_fixture(); before <- args
  out <- do.call(survey_opportunity_summary,args)
  expect_identical(out$records$classification,c('EARLIER_CANDIDATE','GAP_CANDIDATE',
    'LATER_CANDIDATE','OVERLAPS_RECORDED_PERIOD','REVIEW_DATES','PLANNED_NOT_ACQUIRED'))
  expect_true(all(out$records$spatial_relation=='COVERS_FOCUS'))
  expect_identical(args,before)
  expect_identical(out$survey_events,args$survey_events)
  expect_identical(out$records$disposition,rep('UNREVIEWED',6))
})

test_that('reviewed links and duplicate listings do not create acquisitions', {
  a <- opportunity_fixture()
  a$catalog_records$disposition <- c('DISMISSED','REISSUE','REPRESENTED',rep('UNREVIEWED',3))
  a$catalog_records$review_note <- c('Analyst decision','Known revised product','Documented source link',rep(NA_character_,3))
  a$catalog_records$event_id <- c(NA,'e1','e2',rep(NA_character_,3))
  a$catalog_records$source_group <- c('external1','external1',rep(NA_character_,4))
  a$catalog_records$identity_evidence <- c('Explicit test relationship','Explicit test relationship',rep(NA_character_,4))
  a$catalog_records$catalog[2] <- 'B'
  a$searches <- rbind(a$searches,transform(a$searches,catalog='B'))
  o <- do.call(survey_opportunity_summary,a)
  expect_identical(o$records$classification[1:3],c('DISMISSED','REISSUE','REPRESENTED'))
  expect_equal(nrow(o$records),6)
  expect_equal(nrow(o$survey_events),2)
  expect_identical(o$records$source_group,a$catalog_records$source_group)
  a$catalog_records$identity_evidence[1] <- NA
  expect_error(do.call(survey_opportunity_summary,a),'identity_evidence')
  a$catalog_records$identity_evidence[1] <- 'test'
  a$catalog_records$event_id[2] <- 'absent'
  expect_error(do.call(survey_opportunity_summary,a),'existing event_id')
  a$catalog_records$event_id[2] <- 'e1'
  a$catalog_records$review_note[1] <- NA
  expect_error(do.call(survey_opportunity_summary,a),'review_note')
})

test_that('spatial triage retains invalid and partial footprints without repair', {
  a <- opportunity_fixture()
  sf::st_geometry(a$catalog_records)[1] <- sf::st_geometry(a$catalog_records)[1] + c(20,20)
  sf::st_geometry(a$catalog_records)[2] <- sf::st_geometry(a$catalog_records)[2] + c(5,0)
  sf::st_geometry(a$catalog_records)[3] <- sf::st_sfc(sf::st_polygon(list(matrix(
    c(0,0,10,10,0,10,10,0,0,0),ncol=2,byrow=TRUE))))
  sf::st_geometry(a$catalog_records)[4] <- sf::st_sfc(sf::st_polygon())
  original <- sf::st_as_binary(sf::st_geometry(a$catalog_records))
  o <- do.call(survey_opportunity_summary,a)
  expect_identical(o$records$spatial_relation[1:4],c('OUTSIDE_FOCUS','INTERSECTS_FOCUS','INVALID_GEOMETRY','EMPTY_GEOMETRY'))
  expect_identical(o$records$classification[3:4],rep('REVIEW_LOCATION',2))
  expect_identical(sf::st_as_binary(sf::st_geometry(o$records)),original)
})

test_that('incomplete searches and unknown event baselines stay explicit', {
  a <- opportunity_fixture()
  a$searches$outcome <- 'PARTIAL'
  a$searches <- rbind(a$searches,transform(a$searches,catalog='B',outcome='FAILED'))
  a$survey_events$collection_start[1] <- as.Date(NA)
  o <- do.call(survey_opportunity_summary,a)
  expect_identical(o$records$classification[1:4],rep('REVIEW_BASELINE',4))
  expect_identical(o$searches$outcome,c('PARTIAL','FAILED'))
  expect_true(all(o$records$search_outcome=='PARTIAL'))
  a$survey_events <- a$survey_events[FALSE,]
  expect_equal(nrow(do.call(survey_opportunity_summary,a)$survey_events),0)
  a$catalog_records <- a$catalog_records[FALSE,]
  expect_equal(nrow(do.call(survey_opportunity_summary,a)$records),0)
})

test_that('input contracts refuse ambiguity and unsupported keys', {
  a <- opportunity_fixture(); a$catalog_records$collection_end[1] <- as.Date('2000-01-01')
  expect_error(do.call(survey_opportunity_summary,a),'reversed')
  a <- opportunity_fixture(); a$catalog_records$collection_start <- as.character(a$catalog_records$collection_start)
  expect_error(do.call(survey_opportunity_summary,a),'Date columns')
  a <- opportunity_fixture(); a$catalog_records$record_id[2] <- '1'
  expect_error(do.call(survey_opportunity_summary,a),'Duplicate catalog')
  a <- opportunity_fixture(); a$catalog_records$snapshot_id[1] <- 'missing'
  expect_error(do.call(survey_opportunity_summary,a),'search evidence')
  a <- opportunity_fixture(); a$searches$outcome <- 'FAILED'
  expect_error(do.call(survey_opportunity_summary,a),'search evidence')
  a <- opportunity_fixture(); a$catalog_records$status[1] <- 'invented'
  expect_error(do.call(survey_opportunity_summary,a),'status')
  a <- opportunity_fixture(); a$focus <- a$study_area
  sf::st_geometry(a$focus) <- sf::st_geometry(a$focus)+c(20,20)
  sf::st_crs(a$focus) <- 26914
  expect_error(do.call(survey_opportunity_summary,a),'inside')
})

test_that('durable report is escaped, offline and refuses overwrite', {
  skip_if_not_installed('knitr')
  skip_if_not_installed('gt')
  skip_if_not(rmarkdown::pandoc_available())
  a <- opportunity_fixture()
  a$catalog_records$title[1] <- '<script>alert(1)</script>'
  o <- do.call(survey_opportunity_summary,a)
  dest <- tempfile(fileext='.html')
  on.exit(unlink(dest),add=TRUE)
  expect_identical(survey_opportunity_report(o,dest),normalizePath(dest,winslash='/'))
  html <- paste(readLines(dest,warn=FALSE),collapse='\n')
  expect_match(html,'What to review next',fixed=TRUE)
  expect_match(html,'&lt;script&gt;',fixed=TRUE)
  expect_false(grepl('<script>alert(1)</script>',html,fixed=TRUE))
  expect_match(html,'data:image/png;base64',fixed=TRUE)
  before <- tools::md5sum(dest)
  expect_error(survey_opportunity_report(o,dest),'already exists')
  expect_identical(tools::md5sum(dest),before)
  a$catalog_records <- a$catalog_records[FALSE,]
  empty <- tempfile(fileext='.html'); on.exit(unlink(empty),add=TRUE)
  expect_silent(survey_opportunity_report(do.call(survey_opportunity_summary,a),empty))
})

test_that('optional terrain evidence renders a frozen snapshot without changing catalog decisions', {
  skip_if_not_installed('knitr')
  skip_if_not_installed('gt')
  skip_if_not(rmarkdown::pandoc_available())
  f <- tempfile(fileext='.tif')
  d <- terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs='EPSG:26914')
  terra::values(d) <- c(0,-1,.125,NA); terra::writeRaster(d,f)
  artifacts <- data.frame(artifact_id='test',label='<script>terrain-label</script>',
    role='ANALYSIS_DEM',path=f,evidence='Synthetic selection',
    review_note='<b>Review source permission</b>',review_evidence='Synthetic evidence')
  ref <- data.frame(component='elevation_unit',value='feet',basis='OWNER_RECOLLECTION',
    evidence='<b>Attributed owner account; not exact foot definition</b>')
  t <- terrain_reference_review(artifacts,ref)
  args <- opportunity_fixture()
  before <- do.call(survey_opportunity_summary,args)
  args$terrain_review <- t
  o <- do.call(survey_opportunity_summary,args)
  expect_identical(o$records,before$records)
  expect_identical(o$survey_events,before$survey_events)
  expect_identical(o$terrain_review,t)
  # Report rendering must not re-open files or silently refresh observed evidence.
  unlink(f)
  dest <- tempfile(fileext='.html'); on.exit(unlink(dest),add=TRUE)
  survey_opportunity_report(o,dest)
  html <- paste(readLines(dest,warn=FALSE),collapse='\n')
  for (text in c('Chosen project analysis reference','Owner recollection',
    'How did the source become the analysis DEM?', 'Not exposed by this inspection',
    'Ordinary reader definition','Internal compound-CRS definition',
    '&lt;script&gt;terrain-label&lt;/script&gt;','&lt;b&gt;Review source permission&lt;/b&gt;'))
    expect_true(grepl(text,html,fixed=TRUE),info=paste('Missing report text:',text))
  expect_false(grepl('<script>terrain-label</script>',html,fixed=TRUE))
  expect_false(grepl(normalizePath(dirname(f),winslash='/'),html,fixed=TRUE))
  args$terrain_review <- list(schema='FUTURE')
  expect_error(do.call(survey_opportunity_summary,args),'terrain_reference_review')
})
