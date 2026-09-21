test_that("month proposals use acquisition endpoints conservatively", {
  dates <- c("2020-02-01","2020-02-01 to 2020-02-29","2020-02-28 to 2020-03-01",
    "2020","Unknown","2020-02-30","2020-02-20 to 2020-02-01")
  d <- list(records=data.frame(candidate_key=as.character(seq_along(dates)),title="Test",catalog="USIEI",
    record_id=as.character(seq_along(dates)),snapshot_id="s",date_label=dates,
    raw_metadata=vapply(dates,function(d) as.character(jsonlite::toJSON(list(collectiondate=d,
      publicationdate="2020-02-01"),auto_unbox=TRUE)),character(1))),selected=as.character(seq_along(dates)))
  p <- propose_survey_acquisition_groups(d)
  expect_identical(p$proposed_month[1:2],rep("2020-02",2))
  expect_true(all(is.na(p$proposed_month[3:7])))
  expect_identical(p$start[2],"2020-02-01")
  expect_identical(p$end[2],"2020-02-29")
  d$records$date_label[1] <- "2021-02-01"
  expect_true(is.na(propose_survey_acquisition_groups(d)$proposed_month[1]))
  d$records$raw_metadata[1] <- "not json"
  expect_true(is.na(propose_survey_acquisition_groups(d)$proposed_month[1]))
  d$selected <- character()
  expect_equal(nrow(propose_survey_acquisition_groups(d)),0L)
})

test_that("USGS milliseconds preserve UTC dates and reject missing endpoints", {
  d <- list(records=data.frame(candidate_key="a",title="Test",catalog="USGS 3DEP",record_id="1",snapshot_id="s",
    date_label="2020-02-01 to 2020-02-29",raw_metadata='[{"collect_start":1580515200000,"collect_end":1582934400000}]'),selected="a")
  expect_identical(propose_survey_acquisition_groups(d)$proposed_month,"2020-02")
  d$records$raw_metadata <- '{"collect_start":1580515200000,"publicationdate":1582934400000}'
  expect_true(is.na(propose_survey_acquisition_groups(d)$proposed_month))
})

test_that("group persistence validates Reach ownership and preserves immutable evidence", {
  root <- withr::local_tempdir(); ids <- .fg_generate_uuid(6L)
  area <- sf::st_sf(study_area_id=ids[1],study_area_name="Test",
    geometry=sf::st_as_sfc(sf::st_bbox(c(xmin=-94,ymin=41,xmax=-93.9,ymax=41.1),crs=4326)))
  streams <- data.frame(stream_id=ids[2:3],study_area_id=ids[1],stream_name=c("A","B"))
  reaches <- data.frame(reach_id=ids[4:5],stream_id=ids[2:3],reach_name=c("R1","R2"))
  events <- data.frame(survey_event_id=ids[6],reach_id=ids[4],survey_year=2020L,survey_month=2L)
  crs <- validate_study_analysis_crs(26915)
  context <- file.path(root,"study.gpkg")
  write_study_context(context,area,streams,reaches,events,analysis_reference=data.frame(
    component="horizontal",value=crs$wkt,basis="PROJECT_RECORD",evidence="Local test",analyst="Test",recorded_at="2026-09-20T00:00:00Z"))
  raw <- sf::st_sf(ID=1L,Title="Test",collectiondate="2020-02-01",geometry=sf::st_geometry(area))
  d <- list(study_area=area,records=.fg_survey_normalize(raw,"USIEI","s","now","https://example.org"),
    searches=data.frame(catalog="USIEI",snapshot_id="s",outcome="COMPLETE"))
  selection <- file.path(root,"selection.gpkg")
  write_survey_collection_selection(d,"USIEI:1",selection)
  before <- tools::md5sum(c(context,selection))
  out <- file.path(root,"group.gpkg")
  save <- function(stream=ids[2],year=2020,month=2,previous=NULL,dsn=out,cell_size=1L,rationale="Reviewed source")
    write_survey_acquisition_group(context,selection,"USIEI:1",stream,year,month,cell_size,
      rationale,dsn,previous,ids[6])
  expect_error(save(stream=ids[3]),"selected Stream")
  expect_error(save(year=2019),"dates conflict")
  expect_error(save(month=NA_integer_),"dates conflict")
  expect_error(save(rationale=""),"evidence")
  save()
  a <- read_survey_acquisition_group(out)
  expect_identical(a$event_links$survey_event_id,ids[6])
  expect_equal(a$settings$cell_size,1)
  expect_equal(a$settings$anchor_x,0)
  expect_identical(a$settings$wkt,crs$wkt)
  expect_error(save(),"already exists")
  next_path <- file.path(root,"next.gpkg")
  save(previous=out,dsn=next_path,cell_size=2.5)
  b <- read_survey_acquisition_group(next_path)
  expect_identical(a$settings$group_id,b$settings$group_id)
  expect_equal(b$settings$cell_size,2.5)
  expect_equal(read_survey_acquisition_group(out)$settings$cell_size,1)
  expect_identical(tools::md5sum(c(context,selection)),before)
})
