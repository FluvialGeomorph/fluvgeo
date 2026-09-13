retained_evidence_fixture <- function(root) {
  dem <- terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:26914")
  terra::values(dem) <- 1:4
  dir.create(file.path(root,"intake"))
  terra::writeRaster(dem,file.path(root,"intake/terrain.tif"))
  write_terrain_manifest(file.path(root,"intake"),data.frame(artifact_id="dem",
    path="terrain.tif",role="Synthetic DEM"),"Synthetic evidence test")
  start <- start_study_context(file.path(root,"start.gpkg"),"Synthetic study")$context
  args <- read_study_context(start); args$folder_manifest <- "intake/terrain-manifest.json"
  dsn <- do.call(write_study_context,c(list(dsn=file.path(root,"inventory.gpkg")),args))
  record_study_terrain_source(dsn,file.path(root,"claim.gpkg"),"claim","dem","TEST","record",
    "Synthetic consulted metadata","Synthetic collection","CANDIDATE","PROJECT_RECORD",
    "A lead only","Tester")$context
}

retain_test_evidence <- function(dsn, out, file, evidence_id="metadata", kind="METADATA_SNAPSHOT", ...) {
  retain_study_terrain_evidence(dsn,out,evidence_id,"claim",file,kind,
    "Synthetic <record>","Archive reference, edition unknown",
    "Literal <script>bad()</script>\nNo verified execution; Créek", "Tester", ...)
}

test_that("retained evidence copies exact bytes and keeps source use and prior snapshots unchanged", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- retained_evidence_fixture(root)
  file <- file.path(root,"Original Créek.txt"); writeBin(as.raw(c(0,1,10,255,4)),file)
  before <- read_study_context(dsn); baseline <- read_study_context_summary(dsn)$assessment
  files <- list.files(root,recursive=TRUE,full.names=TRUE); hashes <- tools::md5sum(files)
  one <- retain_test_evidence(dsn,file.path(root,"one.gpkg"),file)$context
  x <- read_study_context(one)
  expect_identical(x[setdiff(names(x),"terrain_evidence")],before)
  expect_identical(sf::st_read(one,layer="fluvgeo_study_context",quiet=TRUE)$schema,"FLUVGEO_STUDY_CONTEXT_4")
  target <- file.path(root,"intake",x$terrain_evidence$path)
  expect_identical(readBin(target,"raw",n=10),readBin(file,"raw",n=10))
  expect_identical(x$terrain_evidence$sha256,.fg_file_sha256(file))
  expect_identical(read_study_context_summary(one)$retained_evidence$integrity,"MATCH")
  expect_identical(read_study_context_summary(one)$assessment,baseline)
  # Same bytes can document a different role, without another byte copy.
  two <- retain_test_evidence(one,file.path(root,"two.gpkg"),file,"processing","PROCESSING_RECORD")$context
  y <- read_study_context(two)$terrain_evidence
  expect_identical(y$path,rep(x$terrain_evidence$path,2))
  expect_identical(nrow(read_study_context(one)$terrain_evidence),1L)
  note <- revise_study_context(two,file.path(root,"note.gpkg"),study_area_name="New label")$context
  expect_identical(read_study_context(note)$terrain_evidence,y)
  choice <- record_study_analysis_reference(note,file.path(root,"choice.gpkg"),"horizontal",
    "Synthetic proposal","PROPOSED","Test only","Tester")$context
  expect_identical(read_study_context(choice)$terrain_evidence,y)
  rejected <- record_study_terrain_source(choice,file.path(root,"rejected.gpkg"),"claim","dem","TEST","record",
    "Synthetic consulted metadata","Synthetic collection","REJECTED","PROJECT_RECORD",
    "Record rules out candidate","Tester")$context
  expect_identical(read_study_context(rejected)$terrain_evidence,y)
  expect_identical(tools::md5sum(files),hashes)
  # Relative anchor is the manifest folder, not the context directory.
  moved <- file.path(root,"moved"); dir.create(moved)
  expect_true(all(file.copy(c(file.path(root,"intake"),one),moved,recursive=TRUE)))
  relocated <- file.path(moved,"one.gpkg")
  expect_identical(read_study_context_summary(relocated)$retained_evidence$integrity,"MATCH")
  expect_identical(read_study_context(relocated)$terrain_evidence,x$terrain_evidence)
  changed <- file.path(moved,"intake",x$terrain_evidence$path)
  writeLines("changed",changed)
  expect_identical(read_study_context_summary(relocated)$retained_evidence$integrity,"CHANGED")
  expect_identical(read_study_context(relocated)$terrain_sources$status,"CANDIDATE")
  unlink(changed)
  expect_identical(read_study_context_summary(relocated)$retained_evidence$integrity,"MISSING")
  expect_identical(read_study_context(relocated)$terrain_evidence,x$terrain_evidence)
})

test_that("unreadable evidence remains a finding and late failure retains a reusable copy", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- retained_evidence_fixture(root); file <- file.path(root,"record.txt"); writeLines("test",file)
  out <- file.path(root,"saved.gpkg")
  testthat::with_mocked_bindings({
    expect_error(retain_test_evidence(dsn,out,file),"synthetic context failure")
  }, .fg_save_study_revision=function(...) stop("synthetic context failure"))
  expect_false(file.exists(out))
  copies <- list.files(file.path(root,"intake/terrain-evidence"),full.names=TRUE)
  expect_length(copies,1L)
  hash <- tools::md5sum(copies)
  retain_test_evidence(dsn,out,file)
  expect_identical(tools::md5sum(copies),hash)
  args <- read_study_context(out)
  testthat::with_mocked_bindings({
    x <- .fg_terrain_evidence_inspect(args$terrain_evidence,args$terrain_sources,args$folder_manifest)
    expect_identical(x$integrity,"UNREADABLE")
  }, .fg_file_sha256=function(...) stop("synthetic access failure"))
})

test_that("retention refuses malformed evidence, duplicate IDs and replacing saved files", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- retained_evidence_fixture(root); file <- file.path(root,"record.json"); writeLines("{}",file)
  out <- file.path(root,"one.gpkg"); retain_test_evidence(dsn,out,file)
  bad <- file.path(root,"bad.gpkg")
  expect_error(retain_test_evidence(dsn,out,file),"already exists")
  expect_error(retain_test_evidence(out,bad,file),"append-only")
  expect_error(retain_test_evidence(out,bad,file,"another"),"already retained")
  expect_error(retain_test_evidence(dsn,bad,file,kind="EXECUTED"),"Unsupported")
  expect_error(retain_test_evidence(dsn,bad,root),"existing local")
  expect_error(retain_test_evidence(dsn,bad,paste0(file,"missing")),"existing local")
  expect_false(file.exists(bad))
  x <- read_study_context(out)
  for (change in list(list(path="../escape.txt"),list(association_id="absent"),
      list(sha256=paste(rep("0",64),collapse="")),list(retained_at="2026-02-30T00:00:00Z"),
      list(qualifications=""),list(analyst=NA_character_),list(kind=factor("METADATA_SNAPSHOT")))) {
    y <- x$terrain_evidence; y[names(change)] <- change
    expect_error(.fg_terrain_evidence_check(y,x$terrain_sources))
  }
  target <- file.path(root,"intake",x$terrain_evidence$path)
  writeLines("replaced",target)
  expect_error(retain_test_evidence(dsn,bad,file),"differs")
  expect_identical(readLines(target),"replaced")
  expect_false(file.exists(bad))
  # Older schema cannot advertise evidence and discard it.
  meta <- sf::st_read(out,layer="fluvgeo_study_context",quiet=TRUE)
  meta$schema <- "FLUVGEO_STUDY_CONTEXT_3"
  sf::st_write(meta,out,layer="fluvgeo_study_context",delete_layer=TRUE,quiet=TRUE)
  expect_error(read_study_context(out),"schema and catalog disagree")
})

test_that("all report views expose retained integrity and escaped qualifications", {
  skip_if_not_installed("gt"); skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- retained_evidence_fixture(root); file <- file.path(root,"notes.txt"); writeLines("test",file)
  out <- retain_test_evidence(dsn,file.path(root,"one.gpkg"),file)$context
  for (view in c("definition","staging","terrain")) {
    path <- file.path(root,paste0(view,".html")); study_context_report(out,path,view)
    html <- paste(readLines(path,warn=FALSE),collapse="\n")
    expect_match(html,"Retained bytes match",fixed=TRUE)
    expect_match(html,"Candidate; use not established",fixed=TRUE)
    expect_match(html,"Synthetic &lt;record&gt;",fixed=TRUE)
    expect_false(grepl("<script>bad()</script>",html,fixed=TRUE))
  }
  expect_error(retain_test_evidence(dsn,file.path(root,"collision.gpkg"),file,
    report_file=file.path(root,"definition.html")),"already exists")
  expect_false(file.exists(file.path(root,"collision.gpkg")))
  testthat::local_mocked_bindings(study_context_report=function(...) stop("render failure"))
  expect_error(retain_test_evidence(dsn,file.path(root,"saved.gpkg"),file,
    report_file=file.path(root,"failed.html")),"Revised context saved")
  expect_identical(read_study_context_summary(file.path(root,"saved.gpkg"))$retained_evidence$integrity,"MATCH")
})
