test_that("Reach Flowlines follow ordered retained Stream transitions", {
  raw <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(matrix(c(
    0,0, 3,0, 7,0, 10,0), ncol=2, byrow=TRUE)), crs=26915))
  smoothed <- raw
  reference <- sf::st_sf(selection_id=c("a","b","c"), geometry=sf::st_sfc(
    sf::st_linestring(matrix(c(0,0,3,0),ncol=2,byrow=TRUE)),
    sf::st_linestring(matrix(c(3,0,7,0),ncol=2,byrow=TRUE)),
    sf::st_linestring(matrix(c(7,0,10,0),ncol=2,byrow=TRUE)),crs=26915))
  mappings <- data.frame(selection_id=c("a","b","c"),
    reach_id=c("lower","lower","upper"))
  reaches <- data.frame(reach_id=c("lower","upper"),
    reach_name=c("Lower Reach","Upper Reach"))
  dem <- terra::rast(ncols=10,nrows=2,xmin=0,xmax=10,ymin=-1,ymax=1,
    crs="EPSG:26915");terra::values(dem) <- 1

  value <- derive_reach_flowlines(raw,smoothed,reference,mappings,reaches,dem)
  expect_identical(value$reach_order,c("lower","upper"))
  expect_identical(value$flowlines$ReachName,c("Lower Reach","Upper Reach"))
  expect_equal(value$flowlines$length_m,c(7,3))
  expect_equal(value$flowlines$from_measure,c(0,.007))
  expect_equal(value$flowlines$to_measure,c(.007,.010))
  expect_true(all(vapply(seq_len(nrow(value$flowlines)), function(i) {
    check_flowline(value$flowlines[i, ], step = "profile_points")
  }, logical(1))))
  expect_equal(value$boundaries$raw_fraction,.7)
  expect_equal(tail(sf::st_coordinates(value$flowlines[1,]),1)[1,c("X","Y")],
    head(sf::st_coordinates(value$flowlines[2,]),1)[1,c("X","Y")])
})

test_that("Reach Flowline division fails closed on invalid assignments", {
  raw <- sf::st_sf(geometry=sf::st_sfc(sf::st_linestring(matrix(c(
    0,0, 1,0, 2,0, 3,0),ncol=2,byrow=TRUE)),crs=26915))
  reference <- sf::st_sf(selection_id=c("a","b","c"),geometry=sf::st_sfc(
    sf::st_linestring(matrix(c(0,0,1,0),ncol=2,byrow=TRUE)),
    sf::st_linestring(matrix(c(1,0,2,0),ncol=2,byrow=TRUE)),
    sf::st_linestring(matrix(c(2,0,3,0),ncol=2,byrow=TRUE)),crs=26915))
  reaches <- data.frame(reach_id=c("one","two"),reach_name=c("One","Two"))
  dem <- terra::rast(ncols=3,nrows=1,xmin=0,xmax=3,ymin=-1,ymax=1,
    crs="EPSG:26915");terra::values(dem) <- 1
  expect_error(derive_reach_flowlines(raw,raw,reference,
    data.frame(selection_id=c("a","b","c"),reach_id=c("one","two","one")),
    reaches,dem),"noncontiguous")
  expect_error(derive_reach_flowlines(raw,raw,reference[-3,],
    data.frame(selection_id="a",reach_id="one"),reaches,dem),
    "exactly one Reach")
})

test_that("flowline preserves topology-established direction when requested", {
  line <- sf::st_sf(value="discard",geometry=sf::st_sfc(sf::st_linestring(
    matrix(c(0,0,10,0),ncol=2,byrow=TRUE)),crs=26915))
  dem <- terra::rast(ncols=10,nrows=1,xmin=0,xmax=10,ymin=-1,ymax=1,
    crs="EPSG:26915");terra::values(dem) <- 10:1
  preserved <- flowline(line,"Known topology",dem,direction="preserve")
  expect_setequal(names(preserved),c("ReachName","geometry"))
  expect_equal(sf::st_coordinates(preserved)[1,c("X","Y")],c(X=0,Y=0))
})
