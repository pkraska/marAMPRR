ampBasemap <- function (data, coastline, aoi_buffer = 1000){
  AOI <- data[['si']] %>%
    sf::st_buffer(dist = 1000) %>%
    sf::st_bbox()

  AOI_coastline <- coastline %>%
    sf::st_crop(AOI)

  insetMap_bbox <- data[['si']] %>%
    sf::st_buffer(dist = 1000) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc(crs = 32620)

  insetMap_coastline <-
    rnaturalearth::ne_countries(
      returnclass = 'sf',
      scale = 10,
      country = c('Canada', 'United States of America')
    ) %>%
    # filter(abbrev %in% c('Can.', 'U.S.A.')) %>%
    sf::st_transform(crs = 32620) %>%
    sf::st_crop(sf::st_as_sfc(sf::st_bbox(sf::st_buffer(data[['si']], 50000))))

  insetMap_border <-
    sf::st_as_sfc(sf::st_bbox(sf::st_buffer(data[['si']], 50000)))

  insetMap <- ggplot() +
    geom_sf(data = insetMap_border, fill = 'white', col = 'grey30') +
    geom_sf(data = insetMap_coastline) +
    geom_sf(data = insetMap_bbox, col = 'red', fill = 'red') +
    coord_sf(expand = FALSE) +
    theme_void()

  baseMap <- ggplot() +
    geom_sf(data = insetMap_bbox, fill = NA) +
    geom_sf(data = AOI_coastline) +
    geom_sf(data = data[['si']]) +
    coord_sf(expand = FALSE) +
    theme_minimal()
  # +
    # labs(subtitle = paste0(facilityNum, " - ", unique(data[[facilityNum]][['si']]$facilityName)))

  ggdraw() +
    cowplot::draw_plot(baseMap) +
    cowplot::draw_plot(
      insetMap,
      x = 0.74,
      y = 0.66,
      width = 0.25,
      height = 0.25
    )
}
