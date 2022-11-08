x <- depth_sf %>%
  dplyr::rename(z = Depth)

universalKriging <- function(x) {
  plot_grid <- x %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_transform(crs = 32620) %>%
    sf::st_make_grid(cellsize = 500, what = 'centers') %>%
    sf::st_as_sf() %>%
    cbind(., sf::st_coordinates(.))

  sp_grid <- as(plot_grid, "Spatial")
  sp::gridded(sp_grid) <- TRUE
  sp_grid <- as(sp_grid, "SpatialPixels")


  try(UK <-
        automap::autoKrige(formula = z ~ 1,
                           as(x, "Spatial"),
                           sp_grid, ))
  # plot(UK)
  krige_data <-
    cbind(UK$krige_output@data, UK$krige_output@coords)
  colnames(krige_data) <- c('Z', 'var', 'stdev', 'X', 'Y')
  krige_data <-
    sf::st_as_sf(krige_data, coords = c("X", "Y"), crs = 32620) %>%
    stars::st_rasterize()

  krige_df <-
    akima::interp(
      x = sf::st_coordinates(krige_data)[, 1],
      y = sf::st_coordinates(krige_data)[, 2],
      z = as.vector(krige_data$Z)
    )

  krige_contour <- data.frame(
    x = rep(krige_df$x, ncol(krige_df$z)),
    y = rep(krige_df$y, each = nrow(krige_df$z)),
    z = as.numeric(krige_df$z)
  )

  krige_contour
}

point_plot <- ggplot2::ggplot() +
  ggplot2::geom_contour(data = krige_contour, ggplot2::aes(x = x, y = y, z = z))
+
  # ggplot2::geom_sf(data = otherCages) +
  # ggplot2::geom_sf(data = AOI) +
  # ggplot2::geom_sf(data = plot_data, ggplot2::aes(colour = concentration)) +
  # ggplot2::theme_minimal() +
  # ggplot2::scale_color_viridis_c("Concentration\n(mg/Kg)",
  #                                na.value = "grey",
  #                                option = 'magma') +
  # ggplot2::coord_sf(expand = FALSE) +
  # ggplot2::labs(title = "Measured sample values",
  #               x = "Longitude",
  #               y = "Latitude") +
  # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 320))

interp_plot <- ggplot2::ggplot() +
  stars::geom_stars(data = krige_data) +
  ggplot2::geom_contour(data = krige_contour, ggplot2::aes(x = x, y = y, z = z)) +
  # ggplot2::geom_sf(data = otherCages) +
  # ggplot2::geom_sf(data = AOI) +
  ggplot2::geom_sf(data = plot_data,
                   colour = 'red',
                   shape = 3) +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_viridis_c("Concentration\n(mg/Kg)",
                                na.value = "grey",
                                option = 'magma') +
  ggplot2::coord_sf(expand = FALSE) +
  ggplot2::labs(title = "Universal Kriging interpolation",
                x = "Longitude",
                y = "Latitude") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 320))
