#' Title
#'
#' @param krigedData a dataframe of Universal Kriged data from the `ampKrige`
#'   function
#' @param contour boolean option, if `TRUE`, will plot contour lines
#' @param contour_bins integer value of the number of contour lines bins to use
#' @param crs the EPSG code for the coordinate reference system you would like
#'   to use, default is 32620
#' @param colourTheme character string of the name of the ggplot2 viridis colour
#'   themes. options are "magma", "plasma", and "volcano"
#' @param raster boolean option to use `stars` raster if TRUE (faster), or to
#'   convert the raster to an sf polygon if FALSE (slower)
#'
#' @return a gpplot2 plot of the kriged values supplied
#' @export
#'
#' @examples ampKrigePlot()
ampKrigePlot <-
  function(krigedData,
           contour = TRUE,
           contour_bins = 10,
           crs = 32620,
           colourTheme = "plasma",
           raster = TRUE) {
    UK <- krigedData

    krige_data <-
      cbind(UK$krige_output@data, UK$krige_output@coords) %>%
      `colnames<-`(c('Z', 'var', 'stdev', 'X', 'Y')) %>%
      sf::st_as_sf(coords = c("X", "Y"),
                   crs = crs,
                   remove = FALSE)

    krige_raster <-
      stars::st_rasterize(krige_data)

    if (raster == FALSE) {
      krige_raster <- krige_raster %>%
        sf::st_as_sf(crs)

      ggplot2::ggplot() +
        ggplot2::geom_sf(data = krige_raster, aes(fill = Z, colour = Z)) +
        ggplot2::geom_contour(
          data = krige_data,
          aes(x = X, y = Y, z = Z),
          bins = contour_bins,
          inherit.aes = TRUE
        ) +
        ggplot2::coord_sf(expand = FALSE) +
        ggplot2::scale_fill_viridis_c(option = colourTheme) +
        ggplot2::scale_colour_viridis_c(option = colourTheme) +
        ggplot2::labs(x = "", y = "", title = "TEST")
    } else {
      ggplot2::ggplot() +
        stars::geom_stars(data = krige_raster) +
        ggplot2::geom_contour(
          data = krige_data,
          aes(x = X, y = Y, z = Z),
          bins = contour_bins,
          inherit.aes = TRUE
        ) +
        ggplot2::coord_sf(expand = FALSE) +
        ggplot2::scale_fill_viridis_c(option = colourTheme) +
        ggplot2::scale_colour_viridis_c(option = colourTheme) +
        ggplot2::labs(x = "", y = "", title = "TEST")
    }
  }
