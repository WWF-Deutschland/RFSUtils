test_that("Can cut on dateline as expected...", {

  ## Load fiji file
  fiji <- sf::read_sf(system.file("extdata/fiji.gpkg", package = "RFSUtils"))

  ## Split across dateline
  fiji_split <- trans_poly_cutDateline(fiji)

  ## Expect 2 polygons
  expect_true(nrow(fiji_split) == 2)
  ## New polygons should be valid
  expect_true(all(sf::st_is_valid(fiji_split)))

})

test_that("Currently doesn't work with other CRS...", {

  ## Load fiji file
  fiji <- sf::read_sf(system.file("extdata/fiji.gpkg", package = "RFSUtils"))

  expect_error(trans_poly_cutDateline(fiji, crs = "EPSG:8857"))

})
