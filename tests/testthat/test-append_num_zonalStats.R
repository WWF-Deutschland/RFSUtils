test_that("Extracting zonal statistics works for both sf and sfc inputs...", {

  raster <- terra::rast(system.file("ex/elev.tif", package="terra"))
  polygon_sf <- read_sf(system.file("ex/lux.shp", package="terra")) |>
    ## Simplify so easier and quicker to compare
    dplyr::select(ID_2)

  ## Works with sf object
  sf_output <- append_num_zonalStats(polygon_sf, raster, funs = "max")

  ## Works with sfc object
  sfc_output <- append_num_zonalStats(polygon_sf$geometry, raster, funs = "max")

  ## Should return the same max values
  expect_identical(sf_output$max, sfc_output$max)

})

test_that("Can use existing custom functions for zonal statistics...", {

  raster <- terra::rast(system.file("ex/elev.tif", package="terra"))
  polygon_sf <- read_sf(system.file("ex/lux.shp", package="terra")) |>
    ## Simplify so easier and quicker to compare
    dplyr::select(ID_2)

  ## Works with sf object
  sf_output <- append_num_zonalStats(polygon_sf, raster, funs = c("nodata", "propNA"))

  ## Should match expected values
  expect_true(all(!sf_output$nodata)) ## All polygons should have data
  expect_equal(sf_output$propNA,
               c(0.0732824427480916, 0.0190677966101695, 0.0348623853211009,
                 0.125714285714286, 0.0165441176470588, 0.0951219512195122, 0.125448028673835,
                 0.0486725663716814, 0.0252525252525253, 0.111111111111111, 0,
                 0)) ## These values shouldn't change because polygons are from terra pkg

})

test_that("Can provide new custom functions for zonal statistics...", {

  raster <- terra::rast(system.file("ex/elev.tif", package="terra"))
  polygon_sf <- read_sf(system.file("ex/lux.shp", package="terra")) |>
    ## Simplify so easier and quicker to compare
    dplyr::select(ID_2)

  ## Works with sf object
  sf_output <- append_num_zonalStats(polygon_sf, raster, funs = c("nodata", "nodata2" = function(df, ...){
    sum(!is.na(df$value)) == 0 ## Different way to calculate nodata
  }))

  ## Should match expected values
  expect_identical(sf_output$nodata, sf_output$nodata2) ## All polygons should have data

})

test_that("Can use ... to pass arguments to exact_extract", {

  raster <- terra::rast(system.file("ex/elev.tif", package="terra"))
  polygon_sf <- read_sf(system.file("ex/lux.shp", package="terra")) |>
    ## Simplify so easier and quicker to compare
    dplyr::select(ID_2)

  ## Extract mean
  mean_stat <- append_num_zonalStats(polygon_sf, raster, funs = "max")
  ## Extract area weighted mean
  weighted_mean_stat <- append_num_zonalStats(polygon_sf, raster, funs = "weighted_mean", weights = "area")
  ## Combine
  combo_stat <- append_num_zonalStats(polygon_sf, raster, funs = c("max", "weighted_mean"), weights = "area")

  ## Should get the same output
  expect_identical(mean_stat$max, combo_stat$max)
  expect_identical(weighted_mean_stat$weighted_mean, combo_stat$weighted_mean)

})
