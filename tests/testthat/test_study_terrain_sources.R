source_claim_fixture <- function(root) {
  dem <- terra::rast(nrows=2, ncols=2, xmin=0, xmax=2, ymin=0, ymax=2, crs="EPSG:26914")
  terra::values(dem) <- c(1, 2, NA, 4)
  for (name in c("first", "second")) terra::writeRaster(dem, file.path(root,paste0(name,".tif")))
  ids <- .fg_generate_uuid(5)
  links <- data.frame(artifact_id=c("first","first"),survey_event_id=ids[4:5],
    purpose="Synthetic shared terrain",evidence="Tests preservation only",analyst="Tester",use_for_report=TRUE)
  write_terrain_manifest(root, data.frame(artifact_id=c("first","second"),
    path=c("first.tif","second.tif"), role="test terrain"), "Synthetic sources",event_links=links)
  write_study_context(file.path(root,"old.gpkg"),
    study_area=data.frame(study_area_id=ids[1],study_area_name="Synthetic source-use study"),
    streams=data.frame(stream_id=ids[2],study_area_id=ids[1],stream_name="Synthetic Stream"),
    reaches=data.frame(reach_id=ids[3],stream_id=ids[2],reach_name="Synthetic Reach"),
    survey_events=data.frame(survey_event_id=ids[4:5],reach_id=ids[3],survey_year=c(2006L,2016L)),
    folder_manifest="terrain-manifest.json",analyst_notes="Test only; no recovered archive claims.")
}

source_claim_add <- function(dsn, output, association_id="claim-1", artifact_id="first",
    source_catalog="TEST_ARCHIVE", source_record_id="record-1", source_snapshot="Synthetic archive inventory p.1",
    source_description="Synthetic collection <not real>", status="CANDIDATE", basis="PROJECT_RECORD",
    evidence="Synthetic spatial overlap only; actual use unknown", analyst="Test recorder", ...) {
  record_study_terrain_source(dsn, output, association_id, artifact_id, source_catalog,
    source_record_id, source_snapshot, source_description, status, basis, evidence, analyst, ...)
}

test_that("source-use records pin targets and preserve multiple sources and existing records", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- source_claim_fixture(root)
  choice <- record_study_analysis_reference(dsn,file.path(root,"choice.gpkg"),"vertical",
    "Unresolved design proposal","PROPOSED","Synthetic proposal","Tester")$context
  initial <- read_study_context(choice); base <- read_study_context_summary(choice)
  files <- list.files(root,full.names=TRUE); hashes <- tools::md5sum(files)
  one <- source_claim_add(choice,file.path(root,"one.gpkg"))$context
  x <- read_study_context(one)
  expect_equal(x[setdiff(names(x),"terrain_sources")],initial)
  expect_identical(x$terrain_sources$status,"CANDIDATE")
  expect_true(is.na(x$terrain_sources$source_version))
  expect_identical(x$terrain_sources$artifact_sha256,.fg_file_sha256(file.path(root,"first.tif")))
  expect_identical(sf::st_read(one,layer="fluvgeo_study_context",quiet=TRUE)$schema,"FLUVGEO_STUDY_CONTEXT_3")
  expect_identical(read_study_context_summary(one)$assessment,base$assessment)
  expect_identical(read_study_context_summary(one)$event_artifacts,base$event_artifacts)
  expect_equal(nrow(base$event_artifacts),2L)
  two <- source_claim_add(one,file.path(root,"two.gpkg"),"claim-2",source_record_id="record-2")$context
  three <- source_claim_add(two,file.path(root,"three.gpkg"),"claim-3",artifact_id="second")$context
  # Same record ID in another catalog remains distinct; no deduplication by name.
  four <- source_claim_add(three,file.path(root,"four.gpkg"),"claim-4",source_catalog="TEST_OTHER")$context
  rows <- read_study_context(four)$terrain_sources
  expect_equal(nrow(rows),4L)
  expect_identical(rows$artifact_id,c("first","first","second","first"))
  revised <- source_claim_add(four,file.path(root,"revised.gpkg"),status="RECORDED_USE",
    basis="OWNER_RECOLLECTION",evidence="Synthetic owner account; recipe and exact source edition unknown")$context
  changed <- read_study_context(revised)$terrain_sources
  expect_identical(changed$status,c("RECORDED_USE",rep("CANDIDATE",3)))
  expect_identical(changed[-1,],rows[-1,])
  expect_identical(read_study_context(four)$terrain_sources,rows)
  note <- revise_study_context(revised,file.path(root,"note.gpkg"),study_area_name="Renamed")$context
  expect_identical(read_study_context(note)$terrain_sources,changed)
  next_choice <- record_study_analysis_reference(note,file.path(root,"next-choice.gpkg"),"horizontal",
    "Test proposal","PROPOSED","Synthetic design only","Tester")$context
  expect_identical(read_study_context(next_choice)$terrain_sources,changed)
  event <- read_study_context(next_choice)$survey_events$survey_event_id[1]
  metadata <- record_study_terrain_metadata(next_choice,file.path(root,"metadata.gpkg"),event,
    vertical_unit="International foot",evidence="Synthetic assertion only",analyst="Tester",
    manifest_file=file.path(root,"metadata.json"))$context
  expect_identical(read_study_context(metadata)$terrain_sources,changed)
  expect_identical(read_study_context_summary(metadata)$event_artifacts,base$event_artifacts)
  expect_identical(tools::md5sum(files),hashes)
  # Move the full folder; no absolute target path was stored.
  moved <- file.path(root,"moved"); dir.create(moved)
  expect_true(all(file.copy(list.files(root,full.names=TRUE,pattern="\\.(gpkg|tif|json)$"),moved)))
  expect_identical(read_study_context(file.path(moved,"revised.gpkg"))$terrain_sources,changed)
  unlink(file.path(moved,"first.tif"))
  missing <- read_study_context_summary(file.path(moved,"revised.gpkg"))
  expect_identical(missing$terrain_sources,changed)
  expect_true("FILE_MISSING" %in% missing$assessment$code)
})

test_that("claim revisions refuse retargeting, silent promotion and unsafe publication", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- source_claim_fixture(root); out <- file.path(root,"one.gpkg")
  source_claim_add(dsn,out)
  bad <- file.path(root,"bad.gpkg")
  expect_error(source_claim_add(out,bad),"No change supplied")
  expect_error(source_claim_add(out,bad,association_id="duplicate"),"Duplicate source association")
  for (args in list(list(artifact_id="second"),list(source_catalog="other"),
      list(source_record_id="other"),list(source_snapshot="new snapshot"),list(source_version="new edition")))
    expect_error(do.call(source_claim_add,c(list(dsn=out,output=bad),args)),"cannot be retargeted")
  for (args in list(list(status="VERIFIED"),list(basis="PROPOSED"),list(evidence=""),
      list(analyst=NA_character_),list(source_snapshot=""),list(artifact_id="unknown"),
      list(source_version=""),list(report_purpose="unknown"))) {
    expect_error(do.call(source_claim_add,c(list(dsn=dsn,output=bad),args)))
    expect_false(file.exists(bad))
  }
  expect_error(source_claim_add(dsn,out),"already exists")
  dir.create(file.path(root,"other"))
  expect_error(source_claim_add(dsn,file.path(root,"other/bad.gpkg")),"beside")
  bare <- start_study_context(file.path(root,"bare.gpkg"),"No terrain")$context
  expect_error(source_claim_add(bare,bad),"linked terrain manifest")
  rejected <- source_claim_add(out,file.path(root,"rejected.gpkg"),status="REJECTED",
    evidence="Synthetic project log rules out this source")$context
  expect_identical(read_study_context(rejected)$terrain_sources$status,"REJECTED")
  # Schema 3 also works without analysis choices, and schema 1 remains untouched.
  expect_null(read_study_context(rejected)$analysis_reference)
  expect_identical(sf::st_read(dsn,layer="fluvgeo_study_context",quiet=TRUE)$schema,"FLUVGEO_STUDY_CONTEXT_1")
})

test_that("schema 3 rejects malformed claims and changed manifest targets", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- source_claim_fixture(root)
  one <- source_claim_add(dsn,file.path(root,"one.gpkg"))$context
  args <- read_study_context(one); row <- args$terrain_sources
  write <- function(x) write_study_context(file.path(root,"bad.gpkg"),study_area=args$study_area,
    folder_manifest="terrain-manifest.json",terrain_sources=x)
  for (change in list(list(artifact_id="absent"), list(artifact_sha256=paste(rep("0",64),collapse="")),
      list(recorded_at="2026-02-30T12:00:00Z"),list(source_catalog=NA_character_),
      list(source_version=factor("v1")),list(status="ACCEPTED"))) {
    bad <- row; bad[names(change)] <- change
    expect_error(write(bad)); expect_false(file.exists(file.path(root,"bad.gpkg")))
  }
  bad <- row; bad$extra <- "Not silently dropped"
  expect_error(write(bad),"supported fields")
  expect_error(write(row[FALSE,]),"nonempty")
  expect_error(write(rbind(row,row)),"unique")
  # A valid, freshly inventoried replacement cannot inherit an old source claim.
  terra::writeRaster(terra::rast(matrix(10:13,2,2)),file.path(root,"replacement.tif"))
  write_terrain_manifest(root,data.frame(artifact_id="first",path="replacement.tif",role="test"),
    "Replacement",filename="replacement.json")
  expect_error(write_study_context(file.path(root,"bad.gpkg"),study_area=args$study_area,
    folder_manifest="replacement.json",terrain_sources=row),"fingerprint")
  meta <- sf::st_read(one,layer="fluvgeo_study_context",quiet=TRUE)
  meta$schema <- "FLUVGEO_STUDY_CONTEXT_2"
  sf::st_write(meta,one,layer="fluvgeo_study_context",delete_layer=TRUE,quiet=TRUE)
  expect_error(read_study_context(one),"schema and catalog disagree")
})

test_that("all report views separate source-use claims and preserve render failure evidence", {
  skip_if_not_installed("gt"); skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- source_claim_fixture(root)
  literal <- "Untrusted <script>alert(1)</script> & literal C:\\new\\terrain\nCréek"
  one <- source_claim_add(dsn,file.path(root,"one.gpkg"),evidence=literal)$context
  expect_identical(read_study_context(one)$terrain_sources$evidence,literal)
  two <- source_claim_add(one,file.path(root,"two.gpkg"),"claim-2",source_record_id="record-2",
    status="RECORDED_USE",basis="OWNER_RECOLLECTION")$context
  hashes <- tools::md5sum(list.files(root,full.names=TRUE))
  for (view in c("definition","terrain","staging")) {
    path <- file.path(root,paste0(view,".html")); study_context_report(two,path,view)
    html <- paste(readLines(path,warn=FALSE),collapse="\n")
    expect_match(html,"Candidate; use not established",fixed=TRUE)
    expect_match(html,"Recorded use; attributed account",fixed=TRUE)
    expect_match(html,"Next action",fixed=TRUE)
    expect_match(html,"Unresolved; exact source edition not established",fixed=TRUE)
    expect_match(html,"Synthetic collection &lt;not real&gt;",fixed=TRUE)
    expect_false(grepl("<script>alert(1)</script>",html,fixed=TRUE))
  }
  expect_identical(tools::md5sum(names(hashes)),hashes)
  expect_error(source_claim_add(two,file.path(root,"collision.gpkg"),status="REJECTED",
    report_file=file.path(root,"definition.html")),"already exists")
  expect_false(file.exists(file.path(root,"collision.gpkg")))
  testthat::local_mocked_bindings(study_context_report=function(...) stop("synthetic render failure"))
  expect_error(source_claim_add(two,file.path(root,"retained.gpkg"),status="REJECTED",
    report_file=file.path(root,"failed.html")),"Revised context saved")
  expect_true(file.exists(file.path(root,"retained.gpkg")))
})
