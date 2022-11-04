#' AMP Basemap creation for use in plotting
#'
#' @param data list of AMP data from ampDataLoad function
#' @param coastline sf object from ampMapSetup function
#' @param aoi_buffer distance in meters for how far from sample locations to
#'   buffer for map creation
#' @import dplyr
#' @import sf
#' @import ggplot2
#' @import rnaturalearth
#' @import cowplot
#' @name %>%
#'
#'
#' @return ggplot2 plot of sf objects
#' @export
#'
#' @examples ampBasemap()
ampBasemap <-
  function (data,
            coastline,
            aoi_buffer = 1000,
            insetMap = FALSE,
            facility = NA,
            cageExtents = FALSE) {

    if (is.character(facility)) {
      data[['si']] <- data[['si']] %>%
        filter(facilityNumberInProject %in% facility)
    }

    AOI <- data[['si']] %>%
      sf::st_buffer(dist = 1000) %>%
      sf::st_bbox()

    AOI_coastline <- coastline %>%
      sf::st_crop(AOI)

    insetMap_bbox <- data[['si']] %>%
      sf::st_buffer(dist = 1000) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc(crs = 32620)

    if (insetMap == TRUE) {
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

      insetMap <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = insetMap_border,
                         fill = 'white',
                         col = 'grey30') +
        ggplot2::geom_sf(data = insetMap_coastline) +
        ggplot2::geom_sf(data = insetMap_bbox,
                         col = 'red',
                         fill = 'red') +
        ggplot2::coord_sf(expand = FALSE) +
        ggplot2::theme_void()
    }
    baseMap <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = insetMap_bbox, fill = NA) +
      ggplot2::geom_sf(data = AOI_coastline, col = 'grey', fill = 'grey') +
      ggplot2::geom_sf(data = data[['si']], aes(col = facilityNumber)) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::theme_bw()

    if (cageExtents != FALSE){
      baseMap +
        ggplot2::geom_sf(data = cageExtents, col = 'black')
    }
    # +
    # labs(subtitle = paste0(facilityNum, " - ", unique(data[[facilityNum]][['si']]$facilityName)))
    if (insetMap == TRUE) {
      cowplot::ggdraw() +
        cowplot::draw_plot(baseMap) +
        cowplot::draw_plot(
          insetMap,
          x = 0.74,
          y = 0.66,
          width = 0.25,
          height = 0.25
        )
    }
    if (insetMap == FALSE){
      return(baseMap)
    }
  }
