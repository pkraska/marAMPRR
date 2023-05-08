#' AMP parameter Universal Kriging
#'
#' @param dataframe sf dataframe of data with coordinates variables included
#' @param z parameter column name to be interpolated
#' @param cellsize grid cell size in meters
#' @param crs ending CRS, should keep as 32620 unless you have reasons not to
#'
#' @import dplyr
#' @import sf
#' @import ggplot2
#' @import stars
#'
#' @return dataframe of kriged valuess
#' @export
#'
#' @examples ampKrige()
ampKrige <-
  function(dataframe,
           z,
           cellsize,
           crs = 32620) {
    plot_grid <- dataframe %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_transform(crs) %>%
      sf::st_make_grid(cellsize, what = 'centers') %>%
      sf::st_as_sf() %>%
      cbind(., sf::st_coordinates(.))

    print(plot_grid)

    sp_grid <- as(object = plot_grid, Class = "Spatial")
    sp::gridded(sp_grid) <- TRUE
    sp_grid <- as(sp_grid, "SpatialPixels")


  print(sp_grid)

    try(UK <-
          automap::autoKrige(formula = eval(parse(text = z)) ~ coords.x1 + coords.x2,
                             as(dataframe, "Spatial"),
                             sp_grid))

    plot(UK)
    return(UK)
  }
