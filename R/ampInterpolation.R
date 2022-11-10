#' AMP Bathymetric interpolation
#'
#' @param data sf dataframe of data with coordinates variables included
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
ampInterpolation <- function(data, z, cellsize, crs = 32620) {

  plot_grid <- data %>%
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
        automap::autoKrige(formula = z ~ coords.x1 + coords.x2,
                           as(data, "Spatial"),
                           sp_grid))

  plot(UK)

  krige_data <-
    cbind(UK$krige_output@data, UK$krige_output@coords)
  colnames(krige_data) <- c('Z', 'var', 'stdev', 'X', 'Y')

  krige_data <-
    sf::st_as_sf(krige_data, coords = c("X", "Y"), crs = 32620) %>%
    stars::st_rasterize()

  # krige_df <-
  #   akima::interp(
  #     x = sf::st_coordinates(krige_data)[, 1],
  #     y = sf::st_coordinates(krige_data)[, 2],
  #     z = as.vector(krige_data$Z)
  #   )
  #
  # krige_contour <- data.frame(
  #   x = rep(krige_df$x, ncol(krige_df$z)),
  #   y = rep(krige_df$y, each = nrow(krige_df$z)),
  #   z = as.numeric(krige_df$z)
  # )

ggplot2::ggplot() +
  stars::geom_stars(data = krige_data) +
  ggplot2::geom_contour(data = krige_contour, aes(x, y, z = z), bins = 20, inherit.aes = FALSE) +
  ggplot2::coord_equal()
    }
