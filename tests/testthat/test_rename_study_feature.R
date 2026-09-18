test_that("renaming a hierarchy record preserves all other context values", {
  folder <- withr::local_tempdir()
  base <- start_study_context(file.path(folder,"base.gpkg"),"Names")
  args <- read_study_context(base$context)
  sid <- .fg_generate_uuid(1); rid <- .fg_generate_uuid(1)
  args$streams <- data.frame(stream_id=sid,study_area_id=args$study_area$study_area_id,stream_name="Creek")
  args$reaches <- data.frame(reach_id=rid,stream_id=sid,reach_name="R1")
  args$survey_events <- data.frame(survey_event_id=.fg_generate_uuid(1),reach_id=rid,survey_year=2010L)
  source <- file.path(folder,"source.gpkg")
  do.call(write_study_context,c(list(dsn=source),args))
  original <- read_study_context(source)
  result <- rename_study_feature(source,file.path(folder,"reach.gpkg"),"reach",rid,"Lower")
  expect <- original; expect$reaches$reach_name <- "Lower"
  expect_equal(read_study_context(result$context),expect)
  result <- rename_study_feature(result$context,file.path(folder,"stream.gpkg"),"stream",sid,"Cole")
  expect$streams$stream_name <- "Cole"
  expect_equal(read_study_context(result$context),expect)
  expect_equal(read_study_context(source),original)
})
