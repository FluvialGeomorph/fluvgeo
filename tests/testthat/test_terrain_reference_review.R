reference_review_fixture <- function() {
  folder <- tempfile('reference-review-'); dir.create(folder)
  p <- file.path(folder,'analysis.tif')
  r <- terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs='EPSG:26914')
  terra::values(r) <- c(0,-1,.125,NA)
  terra::writeRaster(r,p,datatype='FLT4S')
  data.frame(artifact_id=c('source','analysis'),label=c('Candidate source','Analysis DEM'),
    role=c('SOURCE_PRODUCT','ANALYSIS_DEM'),path=c(NA_character_,p),
    evidence='Synthetic selection; not a real project')
}

test_that('observations do not populate missing analysis choices or source lineage', {
  a <- reference_review_fixture(); before <- a
  hash <- tools::md5sum(a$path[2])
  o <- terrain_reference_review(a)
  expect_identical(o$files$inspection_status,c('NOT_SELECTED','VERTICAL_CRS_NOT_EXPOSED'))
  expect_identical(o$analysis_reference$basis,rep('UNRESOLVED',3))
  expect_true(all(is.na(o$analysis_reference$value)))
  expect_true(all(is.na(o$files$preparation_note)))
  expect_identical(a,before)
  expect_identical(tools::md5sum(a$path[2]),hash)
  expect_identical(names(o$observations),a$artifact_id)
  expect_null(o$observations$source)
  expect_match(o$files$horizontal[2],'NAD83')
})

test_that('project choices and evidence remain assertions, including differing CRSs', {
  a <- reference_review_fixture()
  a$preparation_note <- c(NA,'Owner recalls transforming once before FG derivation')
  a$preparation_evidence <- c(NA,'Attributed synthetic recollection, not a retained recipe')
  ref <- data.frame(component=c('horizontal','elevation_unit'),value=c('A different project CRS','feet'),
    basis=c('PROPOSED','OWNER_RECOLLECTION'),evidence='Synthetic evidence')
  o <- terrain_reference_review(a,ref)
  expect_identical(o$analysis_reference$basis,c('PROPOSED','UNRESOLVED','OWNER_RECOLLECTION'))
  expect_identical(o$analysis_reference$value,c('A different project CRS',NA_character_,'feet'))
  expect_identical(o$files$preparation_note,a$preparation_note)
  expect_false(any(grepl('CONFLICT',o$files$inspection_status)))
  expect_identical(o$observations$analysis$internal_compound$status,'VERTICAL_CRS_NOT_EXPOSED')
  a$preparation_evidence[2] <- NA
  expect_error(terrain_reference_review(a,ref),'attributed evidence')
  ref$evidence[1] <- ''
  expect_error(terrain_reference_review(reference_review_fixture(),ref),'evidence')
})

test_that('missing and unsupported files remain file findings, not missing CRS diagnoses', {
  a <- reference_review_fixture()
  a$path[1] <- paste0(a$path[2],'.missing.tif')
  a$path[2] <- tempfile(fileext='.tif'); writeLines('not a TIFF',a$path[2])
  o <- terrain_reference_review(a)
  expect_identical(o$files$inspection_status,rep('FILE_INSPECTION_FAILED',2))
  expect_true(all(vapply(o$observations,function(x) nzchar(x$error),logical(1))))
  expect_true(all(o$files$vertical=='Not inspected'))
  a$path <- NA_character_
  expect_true(all(terrain_reference_review(a)$files$inspection_status=='NOT_SELECTED'))
})

test_that('malformed roles, keys, evidence and analysis components are rejected', {
  a <- reference_review_fixture(); a$artifact_id[2] <- a$artifact_id[1]
  expect_error(terrain_reference_review(a),'unique')
  a <- reference_review_fixture(); a$role[1] <- 'AUTOMATIC_SOURCE'
  expect_error(terrain_reference_review(a),'roles')
  a <- reference_review_fixture(); a$path[1] <- ''
  expect_error(terrain_reference_review(a),'use NA')
  a <- reference_review_fixture(); a$review_note <- c('Needs source permission',NA)
  expect_error(terrain_reference_review(a),'attributed evidence')
  a <- reference_review_fixture(); a$preparation_note <- c('Source recipe',NA)
  a$preparation_evidence <- c('test',NA)
  expect_error(terrain_reference_review(a),'ANALYSIS_DEM')
  a <- reference_review_fixture()
  bad <- data.frame(component='horizontal',value='CRS',basis='VERIFIED_BY_FILENAME',evidence='test')
  expect_error(terrain_reference_review(a,bad),'bases')
  bad$basis <- 'PROJECT_RECORD'; bad$component <- 'invented'
  expect_error(terrain_reference_review(a,bad),'components')
  expect_equal(nrow(terrain_reference_review(a[FALSE,])$files),0)
})

test_that('CRS unit formatting retains exact factors and handles absent structured metadata', {
  expect_identical(.fg_reference_unit(NULL),'Not exposed as a vertical CRS unit')
  expect_identical(.fg_reference_unit('metre'),'metre')
  expect_match(.fg_reference_unit(list(type='LinearUnit',name='foot',conversion_factor=.3048)),
    '0.3048 m/unit',fixed=TRUE)
  expect_false(identical(.fg_reference_unit(list(type='LinearUnit',name='foot',conversion_factor=.3048)),
    .fg_reference_unit(list(type='LinearUnit',name='US survey foot',conversion_factor=1200/3937))))
  expect_match(.fg_reference_horizontal(NULL),'Not exposed')
})
