test_that("three descending-bank regions partition a known rectangle and preserve centerline evidence", {
  f <- layout_fixture()
  x <- cross_section_regions(f$line,f$channel,f$floodplain)
  expect_identical(x$region_polygons$region,c("channel","left_floodplain","right_floodplain"))
  expect_equal(as.numeric(sf::st_area(x$region_polygons)),c(2000,5000,5000))
  expect_equal(sum(as.numeric(sf::st_area(x$regions))),12000)
  expect_equal(sum(as.numeric(sf::st_area(sf::st_sym_difference(
    sf::st_union(x$region_polygons),sf::st_union(f$floodplain))))),0,tolerance=1e-6)
  expect_true(all(x$diagnostics$centerline_available))
  expect_equal(nrow(x$centerlines),3)
  expect_setequal(x$raw_centerlines$region_id,x$centerlines$region_id)
  for(i in seq_len(nrow(x$centerlines))) {
    line <- x$centerlines[i,]
    polygon <- x$regions[x$regions$region_id==line$region_id,]
    expect_equal(sum(as.numeric(sf::st_length(sf::st_difference(
      sf::st_geometry(line),sf::st_geometry(polygon))))),0,tolerance=1e-5)
    xy <- sf::st_coordinates(line)
    expect_lt(xy[1,1],xy[nrow(xy),1])
    if(line$region=="left_floodplain")expect_true(all(xy[,2]<=-5))
    if(line$region=="right_floodplain")expect_true(all(xy[,2]>=5))
  }
  expect_error(cross_section_layout(f$line,f$channel,f$floodplain,
    region_artifacts=x,smoothing_refinements=0),"do not match")
  coarser <- cross_section_layout(f$line,f$channel,f$floodplain,spacing_m=40,region_artifacts=x)
  expect_identical(sf::st_as_binary(sf::st_geometry(coarser$paths)),
    sf::st_as_binary(sf::st_geometry(x$centerlines)))
})

test_that("reversing the directed Flowline swaps descending-bank labels", {
  f <- layout_fixture()
  sf::st_geometry(f$line)[[1]] <- sf::st_linestring(rbind(c(200,0),c(0,0)))
  x <- cross_section_regions(f$line,f$channel,f$floodplain,smoothing_refinements=0)
  left <- x$regions[x$regions$region=="left_floodplain",]
  right <- x$regions[x$regions$region=="right_floodplain",]
  expect_true(sf::st_bbox(left)[["ymin"]]>=5)
  expect_true(sf::st_bbox(right)[["ymax"]]<=-5)
  expect_error(cross_section_regions(f$line,f$channel,f$floodplain,smoothing_refinements=11),"0 to 10")
})

test_that("a genuine overbank gap remains separate polygon and centerline components", {
  f <- layout_fixture()
  notch <- sf::st_sfc(sf::st_polygon(list(rbind(c(80,5),c(120,5),c(120,40),c(80,40),c(80,5)))),crs=26915)
  fp <- suppressWarnings(sf::st_difference(f$floodplain,notch))
  x <- cross_section_regions(f$line,f$channel,fp)
  right <- x$regions[x$regions$region=="right_floodplain",]
  expect_equal(nrow(right),2)
  paths <- x$centerlines[x$centerlines$region=="right_floodplain",]
  expect_equal(nrow(paths),2)
  for(i in seq_len(nrow(paths))) {
    coordinates <- sf::st_coordinates(paths[i,])
    expect_false(any(coordinates[,1]>80 & coordinates[,1]<120))
  }
  expect_equal(nrow(x$region_polygons),3)
})
