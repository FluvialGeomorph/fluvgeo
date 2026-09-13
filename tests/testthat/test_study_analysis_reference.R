choice_fixture <- function(root) {
  ids <- .fg_generate_uuid(4)
  dem <- terra::rast(nrows=2, ncols=2, xmin=0, xmax=2, ymin=0, ymax=2, crs="EPSG:26914")
  terra::values(dem) <- 1:4
  terra::writeRaster(dem, file.path(root,"dem.tif"))
  links <- data.frame(artifact_id="dem", survey_event_id=ids[4], purpose="Test",
    evidence="Synthetic selection", analyst="Tester", use_for_report=TRUE)
  write_terrain_manifest(root, data.frame(artifact_id="dem",path="dem.tif",role="terrain",
    vertical_reference="Synthetic artifact assertion",metadata_evidence="Test only"),
    "Test", event_links=links)
  write_study_context(file.path(root,"old.gpkg"),
    study_area=data.frame(study_area_id=ids[1],study_area_name="Synthetic study"),
    streams=data.frame(stream_id=ids[2],study_area_id=ids[1],stream_name="Stream"),
    reaches=data.frame(reach_id=ids[3],stream_id=ids[2],reach_name="R1"),
    survey_events=data.frame(survey_event_id=ids[4],reach_id=ids[3],survey_year=2006L),
    folder_manifest="terrain-manifest.json")
}

test_that("choices round-trip without promoting file assertions or changing assessment", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root, recursive=TRUE))
  dsn <- choice_fixture(root)
  sources <- list.files(root, full.names=TRUE); hashes <- tools::md5sum(sources)
  original <- read_study_context(dsn); baseline <- read_study_context_summary(dsn)
  expect_identical(sf::st_read(dsn,layer="fluvgeo_study_context",quiet=TRUE)$schema,"FLUVGEO_STUDY_CONTEXT_1")
  out <- record_study_analysis_reference(dsn,file.path(root,"chosen.gpkg"),
    "elevation_unit","International foot (0.3048 m)","PROPOSED","Synthetic proposal <not adopted>","Test analyst")
  x <- read_study_context(out$context)
  expect_equal(x[setdiff(names(x),"analysis_reference")], original)
  expect_identical(x$analysis_reference$basis,"PROPOSED")
  expect_identical(x$analysis_reference$analyst,"Test analyst")
  expect_match(x$analysis_reference$recorded_at,"Z$")
  expect_identical(sf::st_read(out$context,layer="fluvgeo_study_context",quiet=TRUE)$schema,"FLUVGEO_STUDY_CONTEXT_2")
  ordinary <- read_study_context_summary(out$context)
  expect_null(ordinary$terrain_review)
  expect_identical(ordinary$analysis_reference,x$analysis_reference)
  expect_identical(ordinary$assessment,baseline$assessment)
  expect_identical(ordinary$event_artifacts,baseline$event_artifacts)
  reviewed <- read_study_context_summary(out$context,TRUE)
  expect_identical(reviewed$terrain_review$analysis_reference$basis,c("UNRESOLVED","UNRESOLVED","PROPOSED"))
  expect_identical(reviewed$terrain_review$files$inspection_status,"VERTICAL_CRS_NOT_EXPOSED")
  expect_identical(reviewed$terrain_review$recorded_metadata$vertical_reference,"Synthetic artifact assertion")
  expect_error(read_study_context_summary(out$context,TRUE,x$analysis_reference),"Saved analysis-reference choices exist")
  expect_identical(tools::md5sum(sources),hashes)
  moved <- file.path(root,"moved"); dir.create(moved)
  expect_true(all(file.copy(list.files(root,full.names=TRUE,pattern="\\.(gpkg|tif|json)$"),moved)))
  expect_identical(read_study_context(file.path(moved,"chosen.gpkg"))$analysis_reference,x$analysis_reference)
  # Another ordinary context editor must retain the new table and schema.
  renamed <- revise_study_context(out$context,file.path(root,"renamed.gpkg"),study_area_name="Revised name")
  expect_identical(read_study_context(renamed$context)$analysis_reference,x$analysis_reference)
  # Broken terrain remains a finding; stored choices cannot make it inspectable.
  unlink(file.path(moved,"dem.tif"))
  blocked <- read_study_context_summary(file.path(moved,"chosen.gpkg"),TRUE)
  expect_identical(blocked$terrain_review$files$inspection_status,"CONTEXT_SELECTION_BLOCKED")
  expect_identical(blocked$analysis_reference,x$analysis_reference)
})

test_that("component revisions preserve earlier snapshots and other choices", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- start_study_context(file.path(root,"draft.gpkg"),"Test draft")$context
  add <- function(src, name, component="horizontal", value="EPSG:26914", basis="PROPOSED",
      evidence="Synthetic design proposal", analyst="Tester", ...)
    record_study_analysis_reference(src,file.path(root,paste0(name,".gpkg")),component,value,basis,evidence,analyst,...)
  one <- add(dsn,"one"); old <- read_study_context(one$context)$analysis_reference
  hash <- tools::md5sum(one$context)
  two <- add(one$context,"two","vertical","NAVD88")
  three <- add(two$context,"three",value="Different explicit description",basis="PROJECT_RECORD",
    evidence="Synthetic document reference")
  x <- read_study_context(three$context)$analysis_reference
  expect_identical(x$value,c("Different explicit description","NAVD88"))
  expect_identical(x$basis,c("PROJECT_RECORD","PROPOSED"))
  expect_identical(read_study_context(one$context)$analysis_reference,old)
  expect_identical(tools::md5sum(one$context),hash)
  expect_identical(x[2,],read_study_context(two$context)$analysis_reference[2,])
  expect_error(add(one$context,"noop"),"No change supplied")
  expect_false(file.exists(file.path(root,"noop.gpkg")))
  expect_error(add(dsn,"one"),"already exists")
  for (case in list(list(component="source_crs"),list(value=""),list(basis="CONFIRMED"),
      list(evidence=""),list(analyst=NA_character_))) {
    expect_error(do.call(add,c(list(src=dsn,name="bad"),case)))
    expect_false(file.exists(file.path(root,"bad.gpkg")))
  }
  bare <- write_study_context(file.path(root,"bare.gpkg"))
  expect_error(add(bare,"no-study"),"Study Area")
  dir.create(file.path(root,"other"))
  expect_error(add(dsn,"other/moved"),"beside")
  expect_error(add(dsn,"bad-view",report_purpose="other"),"report_purpose")
})

test_that("saved choice schema refuses silent truncation and version mismatches", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  area <- data.frame(study_area_id=.fg_generate_uuid(1),study_area_name="Test")
  row <- data.frame(component="vertical",value="NAVD88",basis="OWNER_RECOLLECTION",
    evidence="Synthetic recollection",analyst="Tester",recorded_at="2026-09-12T12:00:00Z")
  write <- function(x) write_study_context(file.path(root,"bad.gpkg"),study_area=area,analysis_reference=x)
  bad <- row; bad$extra <- "No dropping"
  expect_error(write(bad),"supported fields")
  bad <- row; bad$recorded_at <- "2026-02-30T12:00:00Z"
  expect_error(write(bad),"UTC timestamp")
  bad <- row; bad$value <- factor("NAVD88")
  expect_error(write(bad),"plain character")
  expect_error(write(rbind(row,row)),"unique supported")
  expect_error(write(row[FALSE,]),"nonempty")
  expect_false(file.exists(file.path(root,"bad.gpkg")))
  path <- write(row)
  meta <- sf::st_read(path,layer="fluvgeo_study_context",quiet=TRUE)
  meta$schema <- "FLUVGEO_STUDY_CONTEXT_1"
  sf::st_write(meta,path,layer="fluvgeo_study_context",delete_layer=TRUE,quiet=TRUE)
  expect_error(read_study_context(path),"schema and catalog disagree")
})

test_that("all report views show saved evidence without requiring inspection", {
  skip_if_not_installed("gt"); skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- choice_fixture(root)
  out <- record_study_analysis_reference(dsn,file.path(root,"choices.gpkg"),"horizontal",
    "Synthetic <choice>","PROPOSED","Evidence <script>alert(1)</script>","Test recorder")
  paths <- list.files(root,full.names=TRUE); hashes <- tools::md5sum(paths)
  for (purpose in c("definition","terrain","staging")) for (inspect in c(FALSE,TRUE)) {
    path <- file.path(root,paste0(purpose,inspect,".html"))
    study_context_report(out$context,path,purpose,terrain_references=inspect)
    html <- paste(readLines(path,warn=FALSE),collapse="\n")
    expect_match(html,"Synthetic &lt;choice&gt;",fixed=TRUE)
    expect_match(html,"Test recorder",fixed=TRUE)
    expect_match(html,"Proposed; not established",fixed=TRUE)
    expect_false(grepl("<script>alert(1)</script>",html,fixed=TRUE))
  }
  expect_identical(tools::md5sum(paths),hashes)
  expect_error(record_study_analysis_reference(out$context,file.path(root,"collision.gpkg"),
    "vertical","NAVD88","PROPOSED","Test","Tester",report_file=file.path(root,"terrainTRUE.html")),"already exists")
  expect_false(file.exists(file.path(root,"collision.gpkg")))
  testthat::local_mocked_bindings(study_context_report=function(...) stop("synthetic render failure"))
  expect_error(record_study_analysis_reference(out$context,file.path(root,"retained.gpkg"),
    "vertical","NAVD88","PROPOSED","Test","Tester",report_file=file.path(root,"failed.html")),"Revised context saved")
  expect_true(file.exists(file.path(root,"retained.gpkg")))
})
