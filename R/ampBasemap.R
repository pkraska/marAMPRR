#' AMP Basemap creation for use in plotting
#'
#' @param data list of AMP data from ampDataLoad function
#' @param coastline sf object from ampMapSetup function
#' @param aoi_buffer distance in meters for how far from sample locations to
#'   buffer for map creation
#' @import dplyr
#' @importFrom magrittr %>%
#' @import sf
#' @import ggplot2
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
            facility = NA,
            analysis = NA,
            cageExtents = FALSE,
            leases = FALSE,
            sampleLocations = FALSE) {
    if (is.character(facility)) {
      data[['si']] <- data[['si']] %>%
        filter(facilityNumberInProject %in% facility)
    }

    AOI <- data[['si']] %>%
      sf::st_buffer(dist = aoi_buffer) %>%
      sf::st_bbox()

    AOI_coastline <- coastline %>%
      sf::st_crop(AOI)

    map_bbox <- data[['si']] %>%
      sf::st_buffer(dist = aoi_buffer) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc(crs = 32620)

    baseMap <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = map_bbox, fill = NA) +
      ggplot2::geom_sf(data = AOI_coastline,
                       col = 'grey',
                       fill = 'grey')

    if (cageExtents != FALSE) {
      baseMap <- baseMap +
        ggplot2::geom_sf(
          data = sf::st_crop(sf::st_transform(sf::st_read(cageExtents), crs = 32620), AOI),
          col = 'black',
          fill = 'black'
        )
    }

    if (leases != FALSE) {
      baseMap <- baseMap +
        ggplot2::geom_sf(
          data = sf::st_crop(sf::st_transform(sf::st_read(leases), crs = 32620), AOI),
          col = 'dark gray',
          fill = NA
        )
    }

    if (sampleLocations == TRUE) {
      baseMap <- baseMap +
        ggplot2::geom_sf(data = data[['si']],
                         size = ifelse(aoi_buffer > 1000, 1000/aoi_buffer, 0.8),
                         col = 'black',
                         fill = "white", shape = 21)
    }

    if (!is.na(analysis)) {
      baseMap <- baseMap +
        sf::geom_sf(data = analysis, col = analysisMeasure)
    }

    final_basemap <- baseMap +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::theme_bw()

    return(final_basemap)

  }
