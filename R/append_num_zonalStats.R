#' Append zonal statistics column(s) to vector data/
#'
#' Wrapper around `exactextractr` function that overcomes some minor issues in the original function.
#' 1. Takes vector data as first argument to allow function to be more easily used in piped spatial workflow.
#' 2. A non-destructive method that returns a spatial object with all columns by default.
#'
#' @param input Vector data of type `sf` or `sfc` (from [sf::sf-package] package) such as polygon or point data.
#' @param raster A [terra::SpatRaster] object created by the [terra::terra-package] package or `raster` object created by the `raster` package.
#' NOTE: `raster` is expected to be [deprecated in the future](https://r-spatial.org/r/2022/04/12/evolution.html#packages-depending-on-sp-and-raster), so using [terra::terra-package] is recommended.
#' @param funs A vector of zonal statistic functions passed to the `fun` argument in [exactextractr::exact_extract()]. May be either names of known functions described in [exactextractr::exact_extract()] or (named) custom functions that expect an argument 'df'. See examples for more detail.
#' @param quantiles Numeric vector between 0 and 1. When `funs` includes the "quantile" zonal statistic function, which quantiles should be extracted.
#' @param ... Additional arguments passed to [exactextractr::exact_extract()] function.
#'
#' @return Object of type `sf`
#' @export
#' @import sf
#' @import exactextractr
#' @import dplyr
#'
#' @examples
#' ### EXAMPLE 1: USE SINGLE KNOWN ZONAL STAT FUNCTION
#'
#' ## Load a raster file from the terra package
#' ## This is a digital elevation map of Luxembourg
#' raster <- terra::rast(system.file("ex/elev.tif", package="terra"))
#'
#' ## Polygon objects
#' ## Use data from terra package
#' polygon_sf <- sf::read_sf(system.file("ex/lux.shp", package="terra"))
#' multipolygon_sf <- polygon_sf |>
#'     dplyr::group_by(NAME_1) |>
#'     dplyr::summarise()
#'
#' ## Find the max value for each of these objects
#' ## NOTE: The zonal statistic is added as a new column to the original dataset
#' append_num_zonalStats(polygon_sf, raster, funs = "max")
#' append_num_zonalStats(multipolygon_sf, raster, funs = "max")
#'
#' ### EXAMPLE 2: USE MULTIPLE ZONAL STAT FUNCTIONS
#'
#' ## Find max, min, q50, and q75 elevation within each polygon
#' ## NOTE: Again, all columns are added to the existing dataset
#' append_num_zonalStats(polygon_sf, raster, funs = c("max", "min", "quantile"),
#'                 quantile = c(0.5, 0.75))
#' append_num_zonalStats(multipolygon_sf, raster, funs = c("max", "min", "quantile"),
#'                 quantile = c(0.5, 0.75))
#'
#' ### EXAMPLE 3: USE CUSTOM ZONAL STATS FUNCTION
#'
#' ## Find the range of elevation within each polygon
#' ## NOTE: The function must take df and ...
#' append_num_zonalStats(polygon_sf, raster,
#'                 funs = c("range" = function(df, ...){
#' max(df$value, na.rm = TRUE) - min(df$value, na.rm = TRUE)}))

append_num_zonalStats <- function(input, raster,
                                  funs = c("mean", "min", "max", "quantile", "nodata",
                                           "propNA"),
                                  quantiles = c(0.5, 0.90, 0.95),
                                  ...){

  ## Check that raster and polygon are of the correct class

  ## Only accept terra or raster object
  check_arg_type(raster, c("RasterLayer", "SpatRaster"), "raster",
                 msg = "'%s' argument should be a raster from either the terra or raster package (%s object)")

  ## Give depracation warning for raster package
  if(inherits(raster, "RasterLayer")){
    warning("Package `raster` is expected to be depracated. Would recommend using package `terra` instead to read in raster objects.")
  }

  ## Give a specific error for sp objects
  if(inherits(input, c("SpatialPolygonsDataFrame", "SpatialPolygons"))){
    stop("The `sp` package is depracated. Use the `sf` package to work with spatial objects in R instead.")
  }

  ## Give a generic error for all other objects
  check_arg_type(input, c("sf", "sfc"), "input")

  ## If funs is a list (i.e. it contains custom funs)
  ## Make sure the list items have names
  ## We need these names for indexing
  if (inherits(funs, "list")) {
    index <- names(funs) == ""
    names(funs)[index] <- sapply(funs[index],
                                 FUN = function(value){
                                   value
                                 }, simplify = TRUE)
  } else {
    names(funs) <- funs
  }

  ## Replace fn names with explicit bespoke functions where needed
  ## Identify any functions that need to be replaced
  fns_replace_index <- which(funs %in% names(RFSTools::fns_vct))
  ## If there are any, replace these with stored functions
  if (length(fns_replace_index) > 0){
    funs <- c(funs[-fns_replace_index], RFSTools::fns_vct[names(funs[fns_replace_index])])
  }

  ## Loop through all functions provided
  output_list <- sapply(X = 1:length(funs), FUN = function(fn_index){

    fn <- funs[[fn_index]]
    fn_name <- names(funs[fn_index])

    ## If we're provided with a custom fn
    ## Use summarize_df and give new column with the same name
    if (inherits(fn, "function")) {
      tibble({{fn_name}} := exact_extract(x = raster, y = input, fun = fn, summarize_df = TRUE, ...))
    } else {
      ## Otherwise, just run the function as normal and force a df output
      exact_extract(x = raster, y = input, fun = fn, quantiles = quantiles, force_df = TRUE, ...)
    }
  }, simplify = FALSE)

  ## If spatial data is an sfc (i.e. not a data frame) then we join differently
  if(inherits(input, "sfc")){
    ## Bind only the zonal stats cols
    output <- dplyr::bind_cols(output_list, .name_repair = "unique")
    ## Specify the geometry of these zonal stats comes from the polygon input
    st_geometry(output) <- input
  } else if(inherits(input, "sf")){
    output <- dplyr::bind_cols(input, bind_cols(output_list),
                        .name_repair = "unique")
  }

  return(output)

}
