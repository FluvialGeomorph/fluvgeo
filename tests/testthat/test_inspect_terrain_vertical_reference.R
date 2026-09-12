vertical_fixture <- function(compound = TRUE, version = "1.0", crs = "EPSG:26914+5703",
                             flavor = "STANDARD") {
  root <- tempfile("vertical reference "); dir.create(root)
  base <- file.path(root, "base.tif")
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 500000, xmax = 500002,
    ymin = 4500000, ymax = 4500002, crs = "EPSG:26914")
  terra::values(r) <- c(0, -1, .125, NA)
  terra::writeRaster(r, base, datatype = "FLT4S")
  if (!compound) return(base)
  output <- file.path(root, "compound.tif")
  # A deliberately assigned synthetic CRS tests serialization, not site truth.
  sf::gdal_utils("translate", base, output, options = c("-a_srs", crs,
    "-co", paste0("GEOTIFF_VERSION=", version),
    "-co", paste0("GEOTIFF_KEYS_FLAVOR=", flavor)), quiet = TRUE,
    config_options = c(GTIFF_REPORT_COMPD_CS = "TRUE"))
  output
}

test_that("GeoTIFF 1.0 vertical declarations are recovered without rewriting", {
  p <- vertical_fixture()
  before <- tools::md5sum(list.files(dirname(p), full.names = TRUE))
  x <- inspect_terrain_vertical_reference(p)
  expect_identical(x$schema, "FLUVGEO_VERTICAL_REFERENCE_OBSERVATION_1")
  expect_identical(x$internal_compound$status, "VERTICAL_CRS_EXPOSED")
  expect_match(x$internal_compound$wkt, "NAVD88")
  expect_identical(x$internal_compound$vertical_crs$type, "VerticalCRS")
  expect_identical(x$internal_compound$vertical_crs$coordinate_system$axis[[1]]$unit, "metre")
  expect_identical(x$sha256, .fg_file_sha256(p))
  expect_identical(tools::md5sum(list.files(dirname(p), full.names = TRUE)), before)
  expect_equal(as.vector(terra::values(terra::rast(p))), c(0, -1, .125, NA))
  expect_identical(x$internal_compound$config_options$GTIFF_REPORT_COMPD_CS, "TRUE")
  expect_identical(x$internal_compound$open_options, "GEOREF_SOURCES=INTERNAL")
})

test_that("a misleading CRS name never supplies elevation units", {
  crs <- paste0('COMPOUNDCRS["Synthetic international feet",', sf::st_crs(26914)$wkt,
    ',VERTCRS["NAVD88 international feet synthetic",',
    'VDATUM["North American Vertical Datum 1988",ID["EPSG",5103]],CS[vertical,1],',
    'AXIS["height",up,LENGTHUNIT["metre",1]]]]')
  x <- inspect_terrain_vertical_reference(vertical_fixture(crs = crs, flavor = "ESRI_PE"))
  unit <- x$internal_compound$vertical_crs$coordinate_system$axis[[1]]$unit
  expect_identical(x$internal_compound$status, "VERTICAL_CRS_EXPOSED")
  expect_identical(unit, "metre")
  expect_match(x$internal_compound$vertical_crs$name, "feet")
})

test_that("both versions and exact foot units survive internal observation", {
  for (version in c("1.0", "1.1")) {
    p <- vertical_fixture(version = version, crs = "EPSG:26914+6360")
    x <- inspect_terrain_vertical_reference(p)
    expect_identical(x$internal_compound$status, "VERTICAL_CRS_EXPOSED")
    unit <- x$internal_compound$vertical_crs$coordinate_system$axis[[1]]$unit
    expect_match(unit$name, "US survey foot")
    expect_equal(unit$conversion_factor, 1200 / 3937, tolerance = 1e-14)
    expect_false(isTRUE(all.equal(unit$conversion_factor, .3048, tolerance = 1e-10)))
  }
})

test_that("unexposed vertical CRS stays unknown rather than using horizontal units", {
  x <- inspect_terrain_vertical_reference(vertical_fixture(compound = FALSE))
  expect_identical(x$internal_compound$status, "VERTICAL_CRS_NOT_EXPOSED")
  expect_null(x$internal_compound$vertical_crs)
  expect_match(x$internal_compound$wkt, "PROJCRS")
  expect_identical(x$internal_compound$band_unit, "")
})

test_that("internal observations exclude conflicting PAM definitions", {
  p <- vertical_fixture()
  baseline <- inspect_terrain_vertical_reference(p)$internal_compound
  aux <- paste0(p, ".aux.xml")
  writeLines(c("<PAMDataset><SRS>", sf::st_crs(4326)$wkt,
    "</SRS></PAMDataset>"), aux)
  before <- tools::md5sum(c(p, aux))
  x <- inspect_terrain_vertical_reference(p)
  expect_identical(x$internal_compound, baseline)
  expect_true(x$crs_text_differs)
  expect_match(x$default_reader$wkt, "WGS 84")
  expect_identical(x$default_reader$status, "VERTICAL_CRS_NOT_EXPOSED")
  expect_identical(tools::md5sum(c(p, aux)), before)
})

test_that("scoped reads do not change later ordinary GDAL observations", {
  p <- vertical_fixture()
  read <- function() sf::gdal_utils("info", p, options = c("-json", "-norat"), quiet = TRUE)
  before <- read()
  inspect_terrain_vertical_reference(p)
  expect_identical(read(), before)
  # Caller-provided environment options remain unchanged too.
  withr::local_envvar(c(GTIFF_REPORT_COMPD_CS = "TRUE", GDAL_PAM_ENABLED = "YES"))
  before <- read()
  inspect_terrain_vertical_reference(p)
  expect_identical(read(), before)
  expect_identical(Sys.getenv("GTIFF_REPORT_COMPD_CS"), "TRUE")
})

test_that("bad inputs fail rather than being classified as missing metadata", {
  expect_error(inspect_terrain_vertical_reference(NA_character_), "path")
  expect_error(inspect_terrain_vertical_reference(tempfile(fileext = ".tif")), "existing")
  p <- vertical_fixture(compound = FALSE)
  expect_error(inspect_terrain_vertical_reference(dirname(p)), "existing")
  fake <- file.path(dirname(p), "fake.tif")
  writeLines("<VRTDataset/>", fake)
  expect_error(inspect_terrain_vertical_reference(fake), "native TIFF")
  r <- terra::rast(p); multi <- c(r, r)
  many <- file.path(dirname(p), "many.tif"); terra::writeRaster(multi, many)
  expect_error(inspect_terrain_vertical_reference(many), "single-band")
  truncated <- file.path(dirname(p), "truncated.tif")
  writeBin(as.raw(c(0x49, 0x49, 0x2a, 0x00)), truncated)
  expect_error(suppressWarnings(inspect_terrain_vertical_reference(truncated)))
})

test_that("vertical component extraction never treats a geodetic 3D CRS as vertical", {
  v <- list(type = "VerticalCRS", name = "test only")
  expect_identical(.fg_vertical_component(list(type = "BoundCRS", source_crs = v)), v)
  expect_null(.fg_vertical_component(list(type = "GeographicCRS")))
  expect_null(.fg_vertical_component(NULL))
})
