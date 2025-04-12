test_that("Does testing OAuth token auth work?", {
  skip("remember to reset temporary token before running")
  arcgisutils::unset_arc_token()
  # ESRI Living Atlas
  url_image_server <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  # Get updated key before test
  # https://usace-mvr.maps.arcgis.com/home/item.html?id=b5e3ddc1fbb444cda8d5837693e45739
  test_key <- "3NKHt6i2urmWtqOuugvr9TrazKNUuZVJjHbGSqYJWs5wG_O80ALtit7IoCJPyyiM-ZpuPb5PL6WjJAp8eXiNkuDuoDYvuwCcmZwxyoTGi2oMYRDF2LB1xfrxLhO44ndc"
  test_token <- httr2::oauth_token(
                     test_key,
                     arcgis_host = "https://usace-mvr.maps.arcgis.com/")
  arcgisutils::set_arc_token(test_token)
  token <- arcgisutils::arc_token()
  expect_true("httr2_token" %in% class(token))
  imgsrv  <- arcgislayers::arc_open(url_image_server)
  expect_true("ImageServer" %in% class(imgsrv))
})

test_that("Does 'auth_code' function for auth work?", {
  skip("this only work interactively")
  arcgisutils::unset_arc_token()
  # ESRI Living Atlas
  url_image_server <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  # enter credentials in browser, copy code, close browser, paste in console
  token <- arcgisutils::auth_code()
  expect_true("httr2_token" %in% class(token))
  arcgisutils::set_arc_token(token)
  imgsrv  <- arcgislayers::arc_open(url_image_server)
  expect_true("ImageServer" %in% class(imgsrv))
})

test_that("Does 'auth_user' function for auth work?", {
  skip("not recommended")
  arcgisutils::unset_arc_token()
  # ESRI Living Atlas
  url_image_server <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  # ensure .Renviron has variables set, restart R session
  user_token <- arcgisutils::auth_user(
    username = Sys.getenv("ARCGIS_USER"),
    password = Sys.getenv("ARCGIS_PASSWORD"),
    host = Sys.getenv("ARCGIS_HOST"),
    expiration = 60)
  arcgisutils::set_arc_token(token = user_token)
  token <- arcgisutils::arc_token()
  expect_true("httr2_token" %in% class(token))
  imgsrv  <- arcgislayers::arc_open(url_image_server)
  expect_true("ImageServer" %in% class(imgsrv))
})

test_that("Does 'auth_binding' function for auth work?", {
  skip("need to confirm with esri how to configure this")
  arcgisutils::unset_arc_token()
  token <- arcgisutils::auth_binding()
  arcgisutils::set_arc_token(token)
})

test_that("Does 'auth_client' function for auth work?", {
  arcgisutils::unset_arc_token()
  url_image_server <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  perm_token <- arcgisutils::auth_client(
  client = Sys.getenv("ARCGIS_CLIENTID"),
  secret = Sys.getenv("ARCGIS_CLIENTSECRET"),
  host = Sys.getenv("ARCGIS_HOST"),
  expiration = 120)

  arcgisutils::set_arc_token(token = perm_token)
  token <- arcgisutils::arc_token()
  expect_true("httr2_token" %in% class(token))
  imgsrv  <- arcgislayers::arc_open(url_image_server)
  expect_true("ImageServer" %in% class(imgsrv))
})

