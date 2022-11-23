#' Title
#'
#' @param krigedData
#' @param contour
#'
#' @return
#' @export
#'
#' @examples
ampKrigePlot <-
  function(krigedData,
           contour = TRUE,
           contour_interval = 10,
           crs = 32620,
           colourTheme = "plasma") {
    UK <- krigedData

    krige_data <-
      cbind(UK$krige_output@data, UK$krige_output@coords) %>%
      `colnames<-`(c('Z', 'var', 'stdev', 'X', 'Y')) %>%
      sf::st_as_sf(coords = c("X", "Y"),
                   crs = crs,
                   remove = FALSE)

    krige_raster <-
      stars::st_rasterize(krige_data) %>%
      sf::st_as_sf(crs)

    ggplot2::ggplot() +
      ggplot2::geom_sf(data = krige_raster, aes(fill = Z, colour = Z)) +
      # stars::geom_stars(data = krige_raster) +
      ggplot2::geom_contour(
        data = krige_data,
        aes(x = X, y = Y, z = Z),
        bins = contour_interval,
        inherit.aes = TRUE
      ) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::scale_fill_viridis_c(option = colourTheme) +
      ggplot2::scale_colour_viridis_c(option = colourTheme)
  }
