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

    map_bbox <- data[['si']] %>%
      sf::st_buffer(dist = 1000) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc(crs = 32620)

    baseMap <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = map_bbox, fill = NA) +
      ggplot2::geom_sf(data = AOI_coastline,
                       col = 'grey',
                       fill = 'grey') +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::theme_bw()

    if (cageExtents != FALSE) {
      baseMap +
        ggplot2::geom_sf(data = sf::st_read(cageExtents), col = 'black')
    }

    return(baseMap)

  }
