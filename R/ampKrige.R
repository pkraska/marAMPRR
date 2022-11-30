#' AMP parameter Universal Kriging
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
    return(UK)
  }
