#' AMP Bathymetric interpolation
#'
#' @param dataframe sf dataframe of data with coordinates variables included
#' @param x longitude
#' @param y latitude
#' @param z parameter column name to be interpolated
#' @param cellsize grid cell size in meters
#'
#' @import dplyr
#' @import sf
#' @import ggplot2
#' @import stars
#' @name %>%
#'
#' @return dataframe of kriged valuess
#' @export
#'
#' @examples ampInterpolation()
ampInterpolation <- function(dataframe, z, cellsize, contour_interval = 10, crs = 32620) {
  plot_grid <- df %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_transform(crs) %>%
    sf::st_make_grid(cellsize, what = 'centers') %>%
    sf::st_as_sf() %>%
    cbind(., sf::st_coordinates(.))

  sp_grid <- as(object = plot_grid, Class = "Spatial")
  sp::gridded(sp_grid) <- TRUE
  sp_grid <- as(sp_grid, "SpatialPixels")

  try(UK <-
        automap::autoKrige(formula = eval(parse(text = z)) ~ coords.x1 + coords.x2,
                           as(df, "Spatial"),
                           sp_grid))

  plot(UK)

  krige_data <-
    cbind(UK$krige_output@data, UK$krige_output@coords) %>%
    `colnames<-`(c('Z', 'var', 'stdev', 'X', 'Y')) %>%
    sf::st_as_sf(coords = c("X", "Y"), crs = crs, remove = FALSE)

  krige_raster <-
    stars::st_rasterize(krige_data)

  ggplot2::ggplot() +
    stars::geom_stars(data = krige_raster) +
    ggplot2::geom_contour(data = krige_data, aes(x = X, y = Y, z = Z), bins = contour_interval, inherit.aes = FALSE) +
    ggplot2::coord_equal()

}
