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
               c(0.0202484210234714, 0.0040455394946925, 0.00637546070814402,
                 0.0606959992694244, 0.00397761815462046, 0.0319155720122228,
                 0.0546177347644113, 0.0115773805408231, 0.00687701782841438,
                 0.0375266577816092, 0, 0)) ## These values shouldn't change because polygons are from terra pkg

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

test_that("propNA method matches the output of inbuilt 'frac' method", {

  raster <- terra::rast(system.file("ex/elev.tif", package="terra"))
  polygon_sf <- read_sf(system.file("ex/lux.shp", package="terra")) |>
    ## Simplify so easier and quicker to compare
    dplyr::select(ID_2)

  ## Use custom method to return a single column
  ## This is useful because a) it returns a single column b) ignore other values (unlike frac)
  propNA_stat <- append_num_zonalStats(polygon_sf, raster, funs = "propNA")

  ## Use 'frac' to return columns for an is.na raster
  isnaraster <- is.na(raster)
  frac_stat <- append_num_zonalStats(polygon_sf, isnaraster, funs = "frac")

  ## Should get the same output
  expect_equal(propNA_stat$propNA, frac_stat$frac_1)

})
