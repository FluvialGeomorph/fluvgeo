test_that("local catalog identifies whole-area systems and supplies trustworthy links", {
  b <- sf::st_as_sfc(sf::st_bbox(c(xmin=-94,ymin=41,xmax=-93.9,ymax=41.1),crs=4326))
  x <- study_crs_candidates(b)
  expect_true("26915" %in% x$candidates$code)
  expect_true("32615" %in% x$candidates$code) # WGS84 datum ensemble
  expect_false("3857" %in% x$candidates$code)
  row <- x$candidates[x$candidates$code=="26915",]
  expect_equal(nrow(row),1)
  expect_equal(row$coverage,"Full bounds")
  expect_equal(row$unit,"metre")
  expect_equal(row$metres_per_unit,1)
  expect_equal(row$datum,"North American Datum 1983")
  expect_match(x$explorer_url,"latlng=41.100000,-93.900000,41.000000,-94.000000",fixed=TRUE)
  expect_true("EPSG.VERSION" %in% x$metadata$key)
  expect_true(all(startsWith(x$candidates$url,"https://spatialreference.org/ref/epsg/")))
  expect_false(anyDuplicated(x$candidates$code)>0)
  expect_error(study_crs_candidates(sf::st_sfc(sf::st_point(c(0,0)),crs=4326)),"polygon")
  global <- sf::st_as_sfc(sf::st_bbox(c(xmin=-179,ymin=-50,xmax=179,ymax=50),crs=4326))
  expect_error(study_crs_candidates(global),"dateline")
})

test_that("area filter distinguishes overlap, containment and antimeridian extents", {
  x <- data.frame(code=c("1","2","3","4","3857","1"),name=letters[1:6],
    west=c(-100,-94,-80,170,-180,-94),east=c(-90,-93,-70,-170,180,-93),
    south=0,north=80)
  bounds <- c(xmin=-95,ymin=40,xmax=-93,ymax=42)
  out <- .fg_crs_area_filter(x,bounds)
  expect_equal(out$code,c("1","2"))
  expect_equal(out$coverage,c("Full bounds","Partial overlap"))
  expect_equal(.fg_crs_area_filter(x,c(xmin=175,ymin=40,xmax=178,ymax=42))$code,"4")
})

test_that("epoch-dependent projected definitions cannot bypass the unsupported-workflow gate", {
  wkt <- sf::st_crs(26915)$wkt
  modern <- sub("NAD83","NATRF2022",wkt,fixed=TRUE)
  expect_error(validate_study_analysis_crs(modern),"coordinate-epoch")
})
