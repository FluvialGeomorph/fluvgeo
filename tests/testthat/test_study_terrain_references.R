reference_context_fixture <- function(root) {
  ids <- .fg_generate_uuid(5)
  dem <- terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs='EPSG:26914')
  terra::values(dem) <- 1:4
  terra::writeRaster(dem,file.path(root,'dem.tif'))
  links <- data.frame(artifact_id='dem',survey_event_id=ids[4:5],
    purpose='Review retained terrain',evidence='Synthetic explicit selection',
    analyst='Test analyst',use_for_report=TRUE)
  write_terrain_manifest(root,data.frame(artifact_id='dem',path='dem.tif',role='terrain',
    vertical_reference='NAVD88',vertical_unit='ft',metadata_evidence='Synthetic supplied claim'),
    'synthetic',event_links=links)
  write_study_context(file.path(root,'study.gpkg'),
    study_area=data.frame(study_area_id=ids[1],study_area_name='Test <study>'),
    streams=data.frame(stream_id=ids[2],study_area_id=ids[1],stream_name='Stream'),
    reaches=data.frame(reach_id=ids[3],stream_id=ids[2],reach_name='R1'),
    survey_events=data.frame(survey_event_id=ids[4:5],reach_id=ids[3],survey_year=c(2006L,2010L)),
    folder_manifest='terrain-manifest.json')
}

test_that('saved reference review is opt-in, keyed and does not promote assertions', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- reference_context_fixture(root)
  paths <- file.path(root,c('study.gpkg','terrain-manifest.json','dem.tif'))
  before <- tools::md5sum(paths)
  old <- read_study_context_summary(dsn)
  expect_null(old$terrain_review)
  choices <- data.frame(component='elevation_unit',value='feet <unconfirmed>',
    basis='OWNER_RECOLLECTION',evidence='Synthetic recollection')
  new <- read_study_context_summary(dsn,TRUE,choices)
  tr <- new$terrain_review
  expect_equal(nrow(tr$files),1L)
  expect_equal(nrow(tr$context_links),2L)
  expect_setequal(tr$context_links$survey_event_id,old$surveys$survey_event_id)
  expect_equal(tr$files$inspection_status,'VERTICAL_CRS_NOT_EXPOSED')
  expect_equal(tr$recorded_metadata$vertical_reference,'NAVD88')
  expect_equal(tr$analysis_reference$basis,c('UNRESOLVED','UNRESOLVED','OWNER_RECOLLECTION'))
  expect_identical(new$assessment,old$assessment)
  expect_identical(new$event_artifacts,old$event_artifacts)
  expect_identical(new$surveys,old$surveys)
  expect_identical(tools::md5sum(paths),before)
  expect_error(read_study_context_summary(dsn,NA),'TRUE or FALSE')
  expect_error(read_study_context_summary(dsn,analysis_reference=choices),'requires')
  expect_error(read_study_context_summary(dsn,TRUE,data.frame()),'analysis_reference')
  expect_false(any(tr$files$role=='SOURCE_PRODUCT'))
})

test_that('changed files remain blocked and pinned manifest changes fail', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- reference_context_fixture(root)
  file <- file.path(root,'dem.tif')
  # Modify only this synthetic test file, never a retained fixture.
  con <- file(file,'ab'); writeBin(as.raw(0),con); close(con)
  tr <- read_study_context_summary(dsn,TRUE)$terrain_review
  expect_equal(tr$files$inspection_status,'CONTEXT_SELECTION_BLOCKED')
  expect_null(tr$observations[[1]])
  expect_true(is.na(tr$files$path))
  expect_true(all(tr$context_links$grid_status=='NOT_LOADED'))
  expect_true(any(grepl('Resolve',tr$files$next_action)))
  unlink(file)
  expect_equal(read_study_context_summary(dsn,TRUE)$terrain_review$files$inspection_status,
    'CONTEXT_SELECTION_BLOCKED')
  con <- file(file.path(root,'terrain-manifest.json'),'ab'); writeBin(charToRaw(' '),con); close(con)
  expect_error(read_study_context_summary(dsn,TRUE),'changed')
})

test_that('empty drafts remain usable without inventing terrain', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- start_study_context(file.path(root,'draft.gpkg'),'Future study','Draft')$context
  tr <- read_study_context_summary(dsn,TRUE)$terrain_review
  expect_equal(nrow(tr$files),0L)
  expect_equal(nrow(tr$context_links),0L)
  expect_true(all(tr$analysis_reference$basis=='UNRESOLVED'))
})

test_that('unselected inventory is not discovered and read races fail explicitly', {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- reference_context_fixture(root)
  s <- read_study_context_summary(dsn)
  manifest <- file.path(root,'terrain-manifest.json')
  s$event_artifacts$use_for_report <- FALSE
  empty <- .fg_study_terrain_references(s,manifest,NULL)
  expect_equal(nrow(empty$files),0L)
  expect_equal(nrow(empty$recorded_metadata),0L)
  s$event_artifacts$use_for_report <- TRUE
  local_mocked_bindings(inspect_terrain_vertical_reference=function(path)
    list(sha256=paste(rep('0',64),collapse=''),internal_compound=list(
      status='VERTICAL_CRS_NOT_EXPOSED',band_unit=''),crs_text_differs=FALSE))
  expect_error(.fg_study_terrain_references(s,manifest,NULL),'changed between')
})

test_that('all saved views render reference snapshots without mutation or AI services', {
  skip_if_not_installed('gt')
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- reference_context_fixture(root)
  before <- tools::md5sum(dsn)
  for (purpose in c('definition','terrain','staging')) {
    output <- study_context_report(dsn,file.path(root,paste0(purpose,'.html')),purpose,TRUE)
    html <- paste(readLines(output,warn=FALSE,encoding='UTF-8'),collapse='\n')
    for (phrase in c('Terrain references: what is known','Previously recorded artifact metadata',
        'Explicit saved event selections','Test &lt;study&gt;'))
      expect_true(grepl(phrase,html,fixed=TRUE),info=paste(purpose,phrase))
    expect_error(study_context_report(dsn,output,purpose,TRUE),'already exists')
  }
  expect_identical(tools::md5sum(dsn),before)
})
