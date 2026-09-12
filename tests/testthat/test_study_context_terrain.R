terrain_context_fixture <- function(root) {
  aid <- .fg_generate_uuid(1); sid <- .fg_generate_uuid(1); rid <- .fg_generate_uuid(1)
  ids <- .fg_generate_uuid(3)
  ctx <- write_study_context(file.path(root,"study.gpkg"),
    study_area=data.frame(study_area_id=aid,study_area_name="Study"),
    streams=data.frame(stream_id=sid,study_area_id=aid,stream_name="Stream"),
    reaches=data.frame(reach_id=rid,stream_id=sid,reach_name="R1"),
    survey_events=data.frame(survey_event_id=ids,reach_id=rid,survey_year=c(2006L,2010L,2016L)))
  r <- terra::rast(nrows=3,ncols=3,xmin=0,xmax=3,ymin=0,ymax=3,crs="EPSG:26914")
  terra::values(r) <- c(NA,1:8)
  tif <- file.path(root,"terrain.tif"); terra::writeRaster(r,tif)
  list(context=ctx,ids=ids,tif=tif)
}

test_that("terrain association preserves snapshots and permits deliberate shared files", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- terrain_context_fixture(root); hashes <- tools::md5sum(c(f$context,f$tif))
  a <- associate_study_terrain(f$context,file.path(root,"a.gpkg"),f$ids[1],f$tif,
    "Known source; vertical reference unknown","Fixture",file.path(root,"a.json"))
  old <- jsonlite::read_json(a$manifest)
  b <- associate_study_terrain(a$context,file.path(root,"b.gpkg"),f$ids[2],f$tif,
    "Explicit shared terrain for test only","Fixture",file.path(root,"b.json"))
  new <- jsonlite::read_json(b$manifest)
  expect_identical(new$artifacts,old$artifacts)
  expect_identical(new$software$fluvgeo,as.character(utils::packageVersion("fluvgeo")))
  expect_identical(new$event_links[[1]],old$event_links[[1]])
  expect_length(new$artifacts,1L); expect_length(new$event_links,2L)
  x <- read_study_context(b$context); x$folder_manifest <- NULL
  expect_identical(x,read_study_context(f$context))
  expect_identical(tools::md5sum(names(hashes)),hashes)
  review <- read_study_context_summary(b$context)
  expect_true(all(review$event_artifacts$grid_status=="GRID_LOADED"))
  expect_true("VERTICAL_REFERENCE_UNKNOWN" %in% review$assessment$code)
  expect_true(all(is.na(review$folder_inventory$artifacts$vertical_unit)))
  report <- file.path(root,"report.html"); study_context_report(b$context,report,"definition")
  html <- xml2::xml_text(xml2::read_html(report))
  expect_match(html,"Associated terrain files",fixed=TRUE)
  expect_match(html,"Unknown / Unknown",fixed=TRUE)
  expect_match(html,"confirm elevation units",fixed=TRUE)
  moved <- tempfile(); dir.create(moved); withr::defer(unlink(moved,recursive=TRUE))
  expect_true(all(file.copy(list.files(root,full.names=TRUE),moved)))
  expect_identical(read_study_context_summary(file.path(moved,"b.gpkg"))$event_artifacts,review$event_artifacts)
})

test_that("adding terrain never refreshes old missing or changed file evidence", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- terrain_context_fixture(root)
  a <- associate_study_terrain(f$context,file.path(root,"a.gpkg"),f$ids[1],f$tif,"Evidence","Fixture",file.path(root,"a.json"))
  old <- jsonlite::read_json(a$manifest)
  newfile <- file.path(root,"new.tif"); file.copy(f$tif,newfile)
  r <- terra::rast(f$tif); terra::values(r) <- 99
  terra::writeRaster(r,f$tif,overwrite=TRUE)
  expect_error(associate_study_terrain(a$context,file.path(root,"never.gpkg"),f$ids[2],f$tif,"Evidence","Fixture",file.path(root,"never.json")),"fingerprint was not refreshed")
  expect_false(file.exists(file.path(root,"never.json")))
  b <- associate_study_terrain(a$context,file.path(root,"b.gpkg"),f$ids[2],newfile,"Evidence","Fixture",file.path(root,"b.json"))
  expect_identical(jsonlite::read_json(b$manifest)$artifacts[[1]],old$artifacts[[1]])
  review <- read_study_context_summary(b$context)
  expect_true("FILE_CHANGED" %in% review$assessment$code)
  expect_identical(review$event_artifacts$grid_status,c("NOT_LOADED","GRID_LOADED"))
  unlink(f$tif)
  c <- associate_study_terrain(b$context,file.path(root,"c.gpkg"),f$ids[3],newfile,"Evidence","Fixture",file.path(root,"c.json"))
  expect_true("FILE_MISSING" %in% read_study_context_summary(c$context)$assessment$code)
  expect_identical(jsonlite::read_json(c$manifest)$artifacts[[1]],old$artifacts[[1]])
})

test_that("invalid parents, paths and collisions publish nothing", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- terrain_context_fixture(root)
  p <- list(dsn=f$context,output_file=file.path(root,"a.gpkg"),survey_event_id=f$ids[1],
    terrain_file=f$tif,evidence="Evidence",analyst="Fixture",manifest_file=file.path(root,"a.json"))
  invalid <- list(list(survey_event_id="unknown"),list(evidence=" "),list(analyst=" "),
    list(terrain_file=f$context),list(terrain_file=file.path(root,"missing.tif")),
    list(output_file=f$context),list(manifest_file=file.path(root,"bad.txt")))
  for (override in invalid) {
    expect_error(do.call(associate_study_terrain,utils::modifyList(p,override)))
    expect_false(file.exists(p$manifest_file)); expect_false(file.exists(p$output_file))
  }
  other <- tempfile(); dir.create(other); withr::defer(unlink(other,recursive=TRUE))
  file.copy(f$tif,file.path(other,"outside.tif"))
  expect_error(do.call(associate_study_terrain,utils::modifyList(p,list(terrain_file=file.path(other,"outside.tif")))),"inside the manifest")
  expect_error(do.call(associate_study_terrain,utils::modifyList(p,list(manifest_file=file.path(other,"new.json")))),"context folder")
  a <- do.call(associate_study_terrain,p)
  expect_error(do.call(associate_study_terrain,utils::modifyList(p,list(output_file=file.path(root,"new.gpkg")))),"Manifest destination already exists")
  expect_error(do.call(associate_study_terrain,utils::modifyList(p,list(dsn=a$context,output_file=file.path(root,"new.gpkg"),manifest_file=file.path(root,"new.json")))),"already has a selected")
  report <- file.path(root,"old.html"); writeLines("keep",report)
  expect_error(do.call(associate_study_terrain,utils::modifyList(p,list(output_file=file.path(root,"new.gpkg"),manifest_file=file.path(root,"new.json"),report_file=report))),"Report destination already exists")
  expect_identical(readLines(report),"keep")
  expect_false(file.exists(file.path(root,"new.json")))
})

test_that("terrain association retains a missing descriptive Study Area record", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- terrain_context_fixture(root)
  x <- read_study_context(f$context); x$study_area <- NULL
  ctx <- do.call(write_study_context,c(list(dsn=file.path(root,"partial.gpkg")),x))
  a <- associate_study_terrain(ctx,file.path(root,"a.gpkg"),f$ids[1],f$tif,
    "Evidence","Fixture",file.path(root,"a.json"))
  expect_null(read_study_context(a$context)$study_area)
  expect_identical(read_study_context(a$context)$survey_events,x$survey_events)
  expect_identical(inspect_terrain_folder(a$manifest)$intake_id,x$streams$study_area_id[1L])
})

test_that("report failure retains the new linked context with recovery instructions", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  f <- terrain_context_fixture(root)
  local_mocked_bindings(study_context_report=function(...) stop("Render probe"))
  expect_error(associate_study_terrain(f$context,file.path(root,"a.gpkg"),f$ids[1],f$tif,
    "Evidence","Fixture",file.path(root,"a.json"),file.path(root,"a.html")),"Manifest saved.*Revised context saved.*Render probe")
  expect_true(file.exists(file.path(root,"a.json")))
  expect_true(file.exists(file.path(root,"a.gpkg")))
  expect_false(file.exists(file.path(root,"a.html")))
  expect_true(all(read_study_context_summary(file.path(root,"a.gpkg"))$folder_inventory$artifacts$hash_verified))
})
