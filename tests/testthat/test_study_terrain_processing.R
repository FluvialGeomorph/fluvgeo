processing_fixture <- function(root) {
  dem <- terra::rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2,crs="EPSG:26914")
  terra::values(dem) <- 1:4
  terra::writeRaster(dem,file.path(root,"terrain.tif"))
  write_terrain_manifest(root,data.frame(artifact_id="dem",path="terrain.tif",
    role="Synthetic DEM"),"Processing account test")
  start <- start_study_context(file.path(root,"start.gpkg"),"Synthetic study")$context
  args <- read_study_context(start); args$folder_manifest <- "terrain-manifest.json"
  dsn <- do.call(write_study_context,c(list(dsn=file.path(root,"inventory.gpkg")),args))
  record_study_terrain_source(dsn,file.path(root,"claim.gpkg"),"claim","dem","TEST","record",
    "Synthetic consulted metadata","Synthetic collection","CANDIDATE","PROJECT_RECORD",
    "A lead only","Tester")$context
}

processing_steps <- function() data.frame(operation=c("Derive DEM", "Raster calculation"),
  input_description=c("Named collection; exact files unknown", "Derived DEM"),
  output_description=c("Derived DEM", "Analysis DEM; relationship is an account"),
  parameters=c(NA_character_,"DEM / 0.3048; literal supplied expression"),
  software=c(NA_character_,"ArcMap"),software_version=rep(NA_character_,2),
  execution_time=c("2006; supplied year only",NA_character_))

add_processing <- function(dsn, out, ...) record_study_terrain_processing(dsn,out,
  "account","claim",processing_steps(),"OWNER_RECOLLECTION",
  "Synthetic Créek <script>bad()</script>; no execution is verified.","Tester",...)

test_that("preparation accounts round-trip without changing other evidence or terrain", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- processing_fixture(root); before <- read_study_context(dsn)
  baseline <- read_study_context_summary(dsn)$assessment
  files <- list.files(root,recursive=TRUE,full.names=TRUE); hashes <- tools::md5sum(files)
  out <- add_processing(dsn,file.path(root,"processing.gpkg"))$context
  saved <- read_study_context(out); x <- saved$terrain_processing
  expect_identical(saved[setdiff(names(saved),"terrain_processing")],before)
  expect_identical(x[.fg_terrain_processing_step_fields()],processing_steps())
  expect_identical(x$step_number,1:2)
  expect_true(all(is.na(x$evidence_id)))
  expect_identical(read_study_context_summary(out)$assessment,baseline)
  expect_identical(read_study_context_summary(out)$terrain_processing,x)
  expect_identical(sf::st_read(out,layer="fluvgeo_study_context",quiet=TRUE)$schema,"FLUVGEO_STUDY_CONTEXT_5")
  expect_identical(tools::md5sum(files),hashes)
  note <- revise_study_context(out,file.path(root,"note.gpkg"),study_area_name="New label")$context
  expect_identical(read_study_context(note)$terrain_processing,x)
  choice <- record_study_analysis_reference(note,file.path(root,"choice.gpkg"),"horizontal",
    "Synthetic proposal","PROPOSED","Test only","Tester")$context
  expect_identical(read_study_context(choice)$terrain_processing,x)
  rejected <- record_study_terrain_source(choice,file.path(root,"rejected.gpkg"),"claim","dem","TEST","record",
    "Synthetic consulted metadata","Synthetic collection","REJECTED","PROJECT_RECORD",
    "Record rules out candidate","Tester")$context
  expect_identical(read_study_context(rejected)$terrain_processing,x)
  expect_identical(read_study_context(rejected)$terrain_sources$status,"REJECTED")
  corrected <- record_study_terrain_processing(out,file.path(root,"correction.gpkg"),"correction",
    "claim",processing_steps()[1,,drop=FALSE],"PROJECT_RECORD","Corrects earlier account; synthetic only","Tester")$context
  y <- read_study_context(corrected)$terrain_processing
  expect_identical(y[1:2,],x)
  expect_identical(y$step_number,c(1L,2L,1L))
  moved <- file.path(root,"moved"); dir.create(moved)
  expect_true(all(file.copy(c(out,file.path(root,c("terrain-manifest.json","terrain.tif"))),moved)))
  expect_identical(read_study_context(file.path(moved,"processing.gpkg"))$terrain_processing,x)
})

test_that("processing contracts refuse malformed accounts and unsupported evidence links", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- processing_fixture(root); out <- add_processing(dsn,file.path(root,"saved.gpkg"))$context
  bad <- file.path(root,"bad.gpkg")
  expect_error(add_processing(dsn,out),"already exists")
  expect_error(add_processing(out,bad),"append-only")
  other <- file.path(root,"other"); dir.create(other)
  expect_error(add_processing(dsn,file.path(other,"bad.gpkg")),"beside")
  expect_error(add_processing(dsn,bad,evidence_id="absent"),"PROCESSING_RECORD")
  args <- read_study_context(out); original <- args$terrain_processing
  for (change in list(list(association_id="absent"), list(step_number=c(1L,1L)),
      list(step_number=c(1L,3L)), list(step_number=c(1,2)),list(step_number=c(NA_integer_,2L)),
      list(basis="VERIFIED_EXECUTION"),list(basis=c("PROJECT_RECORD","OWNER_RECOLLECTION")),
      list(evidence_id="absent"),list(recorded_at="2026-02-30T00:00:00Z"),
      list(operation=NA_character_),list(parameters=""),list(software=factor(c("A","B"))),
      list(analyst=NA_character_),list(qualifications=c("a","b")))) {
    x <- original; x[names(change)] <- change
    expect_error(.fg_terrain_processing_check(x,args$terrain_sources,args$terrain_evidence))
  }
  for (steps in list(processing_steps()[FALSE,],processing_steps()[,"operation",drop=FALSE],
      transform(processing_steps(),software=factor(software)),
      transform(processing_steps(),operation=NA_character_))) {
    expect_error(record_study_terrain_processing(dsn,bad,"bad","claim",steps,
      "PROJECT_RECORD","Test only","Tester"))
  }
  x <- original; x$extra <- "no"
  expect_error(.fg_terrain_processing_check(x,args$terrain_sources,args$terrain_evidence),"exactly")
  expect_error(.fg_terrain_processing_check(original,NULL,NULL),"existing source-use")
  expect_false(file.exists(bad))
  meta <- sf::st_read(out,layer="fluvgeo_study_context",quiet=TRUE)
  meta$schema <- "FLUVGEO_STUDY_CONTEXT_4"
  sf::st_write(meta,out,layer="fluvgeo_study_context",delete_layer=TRUE,quiet=TRUE)
  expect_error(read_study_context(out),"schema and catalog disagree")
})

test_that("retained processing documentation can support accounts without proving execution", {
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- processing_fixture(root); file <- file.path(root,"notes.txt"); writeLines("synthetic",file)
  one <- retain_study_terrain_evidence(dsn,file.path(root,"evidence.gpkg"),"notes","claim",file,
    "PROCESSING_RECORD","Supplied note","Synthetic source","Not an execution log","Tester")$context
  out <- add_processing(one,file.path(root,"account.gpkg"),evidence_id="notes")$context
  args <- read_study_context(out); x <- args$terrain_processing
  expect_identical(x$evidence_id,rep("notes",2))
  expect_identical(args$terrain_sources$status,"CANDIDATE")
  bad_evidence <- args$terrain_evidence; bad_evidence$kind <- "METADATA_SNAPSHOT"
  expect_error(.fg_terrain_processing_check(x,args$terrain_sources,bad_evidence),"PROCESSING_RECORD")
  bad_evidence$kind <- "PROCESSING_RECORD"; bad_evidence$association_id <- "different"
  expect_error(.fg_terrain_processing_check(x,args$terrain_sources,bad_evidence),"same source-use")
  changed <- file.path(root,args$terrain_evidence$path); writeLines("changed",changed)
  expect_identical(read_study_context_summary(out)$retained_evidence$integrity,"CHANGED")
  expect_identical(read_study_context(out)$terrain_processing,x)
  unlink(changed)
  expect_identical(read_study_context_summary(out)$retained_evidence$integrity,"MISSING")
  expect_identical(read_study_context(out)$terrain_processing,x)
  # No retained copy is required for an attributed recollection account.
  nofile <- add_processing(dsn,file.path(root,"recollection.gpkg"))$context
  later <- retain_study_terrain_evidence(nofile,file.path(root,"later.gpkg"),"later","claim",file,
    "METADATA_SNAPSHOT","Later evidence","Synthetic source","Test only","Tester")$context
  expect_identical(read_study_context(later)$terrain_processing,read_study_context(nofile)$terrain_processing)
})

test_that("reports show unknowns and next actions with literal escaped preparation text", {
  skip_if_not_installed("gt"); skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile(); dir.create(root); withr::defer(unlink(root,recursive=TRUE))
  dsn <- processing_fixture(root); out <- add_processing(dsn,file.path(root,"account.gpkg"))$context
  for (view in c("definition","staging","terrain")) {
    path <- file.path(root,paste0(view,".html")); study_context_report(out,path,view)
    html <- paste(readLines(path,warn=FALSE),collapse="\n")
    expect_match(html,"Recorded terrain preparation",fixed=TRUE)
    expect_match(html,"Unknown; not recovered",fixed=TRUE)
    expect_match(html,"Recover where feasible:",fixed=TRUE)
    expect_match(html,"DEM / 0.3048; literal supplied expression",fixed=TRUE)
    expect_match(html,"2006; supplied year only",fixed=TRUE)
    expect_match(html,"Candidate; use not established",fixed=TRUE)
    expect_false(grepl("<script>bad()</script>",html,fixed=TRUE))
  }
  expect_error(add_processing(dsn,file.path(root,"collision.gpkg"),report_file=file.path(root,"definition.html")),"already exists")
  expect_false(file.exists(file.path(root,"collision.gpkg")))
  testthat::local_mocked_bindings(study_context_report=function(...) stop("synthetic render failure"))
  expect_error(add_processing(dsn,file.path(root,"saved.gpkg"),report_file=file.path(root,"failed.html")),"Revised context saved")
  expect_identical(read_study_context(file.path(root,"saved.gpkg"))$terrain_processing$step_number,1:2)
})

test_that("preparation reporting respects rejected sources and fresh document integrity", {
  skip_if_not_installed("gt")
  x <- data.frame(processing_id="account",association_id="claim",step_number=1L,
    processing_steps()[1,,drop=FALSE],basis="PROJECT_RECORD",evidence_id="notes",
    qualifications="Test only",analyst="Tester",recorded_at="2026-09-13T12:00:00Z")
  sources <- data.frame(association_id="claim",artifact_id="dem",status="CANDIDATE")
  for (state in c("MATCH","MISSING","CHANGED","UNREADABLE")) {
    retained <- data.frame(evidence_id="notes",integrity=state)
    text <- paste(capture.output(.fg_report_terrain_processing(x,sources,retained)),collapse="\n")
    expect_match(text,paste("Retained document:",state),fixed=TRUE)
    expect_identical(grepl("Resolve the retained document",text,fixed=TRUE),state!="MATCH")
  }
  sources$status <- "REJECTED"
  text <- paste(capture.output(.fg_report_terrain_processing(x,sources,NULL)),collapse="\n")
  expect_match(text,"Source association rejected",fixed=TRUE)
  expect_match(text,"not inspected",fixed=TRUE)
  sources$status <- "RECORDED_USE"
  x[.fg_terrain_processing_step_fields()] <- lapply(x[.fg_terrain_processing_step_fields()],function(v) {v[is.na(v)] <- "Supplied detail"; v})
  x$evidence_id <- NA_character_
  text <- paste(capture.output(.fg_report_terrain_processing(x,sources,NULL)),collapse="\n")
  expect_match(text,"populated fields do not verify execution",fixed=TRUE)
})
