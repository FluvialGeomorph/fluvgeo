survey_collection_fixture <- function() {
  b <- sf::st_sf(study_area_id="study-one",geometry=sf::st_sfc(sf::st_polygon(list(rbind(
    c(-90,40),c(-89.99,40),c(-89.99,40.01),c(-90,40.01),c(-90,40)))),crs=4326))
  x <- sf::st_sf(workunit_id=1L,workunit="Test lidar",collect_start=1262304000000,
    collect_end=1293753600000,lpc_link="https://example.org/data",metadata_link="https://example.org/meta",
    ID=2L,Title="Planned lidar",Status="Planned/Funded",collectiondate="2027 (planned)",
    productsavailable="Unknown",geometry=sf::st_geometry(b))
  list(boundary=b,raw=x)
}

test_that("product evidence does not promote general access to DEM availability", {
  f <- survey_collection_fixture()
  f$raw$sourcedem_link <- "https://example.org/dem"
  r <- list(records=.fg_survey_normalize(f$raw,"USGS 3DEP","s","now","https://example.org"))
  p <- survey_collection_products(r)
  expect_identical(p$product,c("DEM","POINT_CLOUD"))
  expect_identical(p$access_url,c("https://example.org/dem","https://example.org/data"))
  r$records <- .fg_survey_normalize(f$raw,"USIEI","s","now","https://example.org")
  expect_true(all(survey_collection_products(r)$access_url=="Unknown"))
  r$records$raw_metadata <- "invalid json"
  expect_true(all(survey_collection_products(r)$access_url=="Unknown"))
})

test_that("DEM pixel size screens resolution without asserting suitability", {
  f <- survey_collection_fixture()
  for(pixel in c(.5,1,2,0,-1,NA_real_,Inf)) {
    f$raw$dem_gsd_meters <- pixel
    r <- list(records=.fg_survey_normalize(f$raw,"USGS 3DEP","s","now","https://example.org"))
    p <- survey_collection_products(r)
    expect_true(is.na(p$dem_pixel_size_m[2]))
    if(is.finite(pixel) && pixel>0) {
      expect_equal(p$dem_pixel_size_m[1],pixel)
      expect_match(p$resolution_screen[1],if(pixel<=1) "suitability unreviewed" else "Too coarse")
    } else expect_true(is.na(p$dem_pixel_size_m[1]))
  }
  f$raw$pointspacing <- "0.5 m"
  r <- list(records=.fg_survey_normalize(f$raw,"USIEI","s","now","https://example.org"))
  expect_true(is.na(survey_collection_products(r)$dem_pixel_size_m[1]))
})

test_that("product plans round trip and reject unsupported or unselected intent", {
  f <- survey_collection_fixture()
  r <- list(study_area=f$boundary,records=.fg_survey_normalize(f$raw,"USIEI","s","now","https://example.org"),
    searches=data.frame(catalog="USIEI",snapshot_id="s",outcome="COMPLETE"),
    acquisition_plan=data.frame(candidate_key=c("USIEI:2","USIEI:2"),product=c("DEM","POINT_CLOUD")))
  path <- tempfile(fileext=".gpkg"); on.exit(unlink(path))
  expect_error(write_survey_collection_selection(r,character(),path),"Acquisition plan")
  write_survey_collection_selection(r,"USIEI:2",path)
  expect_identical(as.data.frame(read_survey_collection_selection(path)$acquisition_plan),r$acquisition_plan)
  r$acquisition_plan$product[1] <- "guessed"
  expect_error(write_survey_collection_selection(r,"USIEI:2",tempfile(fileext=".gpkg")),"Acquisition plan")
  # Prior version remains readable without implying product choices.
  sf::st_write(data.frame(schema="SURVEY_COLLECTION_SELECTION_1"),path,layer="selection_metadata",delete_layer=TRUE,quiet=TRUE)
  expect_equal(nrow(read_survey_collection_selection(path)$acquisition_plan),0L)
})

test_that("catalog normalization retains source status, literal dates and unverified access", {
  f <- survey_collection_fixture()
  a <- .fg_survey_normalize(f$raw,"USGS 3DEP","one","now","https://example.org")
  b <- .fg_survey_normalize(f$raw,"USIEI","two","now","https://example.org")
  expect_identical(a$candidate_key,"USGS 3DEP:1")
  expect_match(a$availability,"not verified")
  expect_identical(b$status,"Planned/Funded")
  expect_identical(b$date_label,"2027 (planned)")
  expect_match(b$raw_metadata,"Planned/Funded")
  f$raw$ID <- NA_integer_
  expect_error(.fg_survey_normalize(f$raw,"USIEI","two","now","https://example.org"),"identity")
})

test_that("discovery distinguishes capped, empty and failed queries", {
  f <- survey_collection_fixture()
  path <- tempfile(fileext=".geojson"); on.exit(unlink(path))
  sf::st_write(f$raw,path,quiet=TRUE)
  payload <- jsonlite::fromJSON(paste(readLines(path),collapse="\n"),simplifyVector=FALSE)
  with_mocked_bindings({
    r <- discover_survey_collections(f$boundary,max_records=1L)
    expect_identical(r$searches$outcome,c("PARTIAL","FAILED"))
    expect_equal(nrow(r$records),1L)
    expect_identical(r$records$coverage,"Covers Study Area")
  },.fg_survey_get=function(endpoint,query) {
    if(grepl("noaa",endpoint)) stop("Synthetic timeout")
    if(identical(query$returnIdsOnly,"true")) list(objectIds=list(1,2)) else payload
  })
  with_mocked_bindings({
    r <- discover_survey_collections(f$boundary)
    expect_true(all(r$searches$outcome=="COMPLETE")); expect_equal(nrow(r$records),0L)
    path <- tempfile(fileext=".gpkg"); on.exit(unlink(path),add=TRUE)
    write_survey_collection_selection(r,character(),path)
    expect_equal(nrow(read_survey_collection_selection(path)$records),0L)
  },.fg_survey_get=function(...) list(objectIds=NULL))
})

test_that("immutable selection snapshots preserve identity, intent and query evidence", {
  f <- survey_collection_fixture()
  r <- list(study_area=f$boundary,records=.fg_survey_normalize(f$raw,"USIEI","snapshot","now","https://example.org"),
    searches=data.frame(catalog="USIEI",snapshot_id="snapshot",outcome="COMPLETE"))
  path <- tempfile(fileext=".gpkg"); on.exit(unlink(path))
  expect_error(write_survey_collection_selection(r,"invented",path),"reviewed discovery")
  expect_false(file.exists(path))
  write_survey_collection_selection(r,r$records$candidate_key,path)
  value <- read_survey_collection_selection(path)
  expect_identical(value$selected,r$records$candidate_key)
  expect_identical(value$records$status,"Planned/Funded")
  expect_identical(value$records$raw_metadata,r$records$raw_metadata)
  expect_true(all(lengths(sf::st_equals(value$records,r$records))==1L))
  next_path <- tempfile(fileext=".gpkg"); on.exit(unlink(next_path),add=TRUE)
  write_survey_collection_selection(value,character(),next_path)
  expect_length(read_survey_collection_selection(next_path)$selected,0L)
  expect_error(write_survey_collection_selection(r,character(),path),"already exists")
})
