test_that("synthetic stream extraction writes a reviewable, reproducible candidate", {
  source <- tempfile(fileext=".tif")
  output <- tempfile("synthetic-stream-");dir.create(output)
  dem <- terra::rast(matrix(c(9,8,7,6,5,4,3,2,1),nrow=3,byrow=TRUE),
    extent=terra::ext(0,3,0,3),crs="EPSG:26915")
  terra::writeRaster(dem,source,datatype="FLT4S")

  result <- extract_synthetic_stream_network(source,9,output,
    threshold_ha=.0001,memory_budget_mb=2048)

  expect_identical(result$schema,"SYNTHETIC_STREAM_NETWORK_1")
  expect_equal(result$threshold_cells,1)
  expect_true(result$stream_lines>0)
  expect_false("fill_display" %in% names(result$files))
  expect_true("fill_depth" %in% names(result$files))
  expect_true(all(file.exists(file.path(output,unlist(result$files)))))
  expect_true(file.exists(file.path(output,"result.rds")))
  expect_true(file.exists(file.path(output,"provenance.json")))
  network <- sf::st_read(file.path(output,result$files$stream_network),quiet=TRUE)
  expect_s3_class(network,"sf")
  expect_true(all(c("stream_line_id","upstream_cell","downstream_cell",
    "length_m","threshold_ha") %in% names(network)))

  second <- file.path(output,"stream-network-2ha.gpkg")
  direction_path <- file.path(output,result$files$direction)
  direction_before <- unname(tools::md5sum(direction_path))
  revised <- threshold_synthetic_stream_network(
    direction_path,
    file.path(output,result$files$accumulation),second,threshold_ha=.0002)
  expect_equal(revised$threshold_cells,2)
  expect_true(file.exists(second))
  expect_identical(unname(tools::md5sum(direction_path)),direction_before)
})

test_that("outlet location falls back to the lowest rounded-cap cell without NLDI continuation", {
  source <- tempfile(fileext=".tif")
  dem <- terra::rast(matrix(seq(100,1),nrow=10,byrow=TRUE),
    extent=terra::ext(0,10,0,10),crs="EPSG:26915")
  terra::writeRaster(dem,source)
  reference <- sf::st_sf(source_id="123",geometry=sf::st_sfc(
    sf::st_linestring(matrix(c(5,9,5,1),ncol=2,byrow=TRUE)),crs=26915))
  unavailable <- sf::st_sf(nhdplus_comid="123",geometry=sf::st_sfc(
    sf::st_linestring(matrix(c(5,9,5,1),ncol=2,byrow=TRUE)),crs=26915))

  outlet <- locate_stream_outlet(source,reference,search_radius_m=4,
    crossing_radius_m=2,downstream_lines=unavailable)

  expect_match(outlet$method,"NLDI continuation unavailable")
  expect_true(is.na(outlet$next_source_id))
  expect_true(outlet$cell %in% c(96:100))
})
