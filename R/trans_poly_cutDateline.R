#' Cut polygons that cross dateline.
#'
#' Unproject polygon (EPSG:4326) that cross dateline can be broken when
#' transforming. This function takes identifies these polygons and splits them
#' into two to allow transformation.
#'
#' @param sf Polygon data of class sf
#' @param crs Character string. The CRS of the sf data (currently only EPSG:4326)
#' is possible.
#'
#' @returns Polygon data of class sf
#' @export
#'
#' @examples
#' fiji <- sf::st_read(system.file("extdata/fiji.gpkg", package = "RFSUtils"))
#'
#' ## Fiji is one polygon but crosses dateline
#' nrow(fiji)
#' plot(fiji)
#'
#' ## Split across dateline
#' fiji_split <- fiji |>
#'   trans_poly_cutDateline()
#'
#' ## Now we have two polygons either side
#' nrow(fiji_split)
#' plot(fiji_split[1, ])
#' plot(fiji_split[2, ])
trans_poly_cutDateline <- function(sf, crs = "EPSG:4326"){

  ## Filter those polygons that may overlap dateline
  atDateline <- c()
  for(x in 1:nrow(sf)){

    focal_bbox <- sf::st_bbox(sf[x, ])
    atDateline <- append(atDateline, focal_bbox[["xmax"]] > 179 & focal_bbox[["xmin"]] < -179)

  }

  if (crs != "EPSG:4326"){
    stop("Currently only unprojected data EPSG:4326 is possible")
  }

  poly_dateline <- sf[atDateline, ]
  poly_east <- sf::st_intersection(poly_dateline, sf::st_as_sfc(sf::st_bbox(c(xmin = 0, xmax = 180, ymin = -90, ymax = 90))) |>
                                     sf::st_set_crs("EPSG:4326"))
  poly_west <- sf::st_intersection(poly_dateline, sf::st_as_sfc(sf::st_bbox(c(xmin = -180, xmax = 0, ymin = -90, ymax = 90))) |>
                                     sf::st_set_crs("EPSG:4326"))
  poly_split <- bind_rows(poly_east, poly_west)
  fixed_sf <- sf |>
    filter(!atDateline) |>
    bind_rows(poly_split)

  return(fixed_sf)

}
