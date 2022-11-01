#' AMP map setup
#'
#' @param dl_coastline logical; if TRUE will download the Statistics Canada coastline file
#' @param dl_us_coastline logical; if TRUE will download the NOAA coastline file
#' @param path character string; location to save downloads to
#' @param country `canada` or `usa`
#'
#' @return Downloads Canadian or USA coastline shapefile data to path
#' @export
#'
#' @examples ampMapSetup()
#'

ampMapSetup <-
  function (path = "data/input/coastline/",
            country = "canada",
            province = "N.B.",
            dl_coastline = TRUE) {
    if (country == "canada") {
      if (dl_coastline == TRUE) {
        if (!dir.exists(path)) {
          message("Creating directory for script outputs at in 'data/input/")
          dir.create(path, recursive = TRUE)
        }
        temp <- tempfile()
        download.file(
          "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip",
          temp
        )

        unzip(temp, exdir = "data/input/coastline")

        coastline <-
          sf::st_read(dsn = paste0(path, "lpr_000b16a_e.shp")) %>%
          sf::st_transform(crs = 32620)
      } else {
        message(paste0(
          "Using user provided coastline shapefile found in ",
          path,
          " folder."
        ))
        coastline <-
          sf::st_read(dsn = paste0(path, "lpr_000b16a_e.shp")) %>%
          sf::st_transform(crs = 32620)
      }
    }

    if (country == "usa") {
      if (dl_coastline == TRUE) {
        temp <- tempfile()
        download.file("https://coast.noaa.gov/htdata/Shoreline/us_medium_shoreline.zip",
                      temp)
        unzip(temp, exdir = path)

        coastline <-
          sf::st_read(paste0(path, "us_medium_shoreline.shp")) %>%
          sf::st_transform(crs = 32620)

      } else {
        message("Using user provided coastline shapefile found in the data/coastline folder.")
        coastline <-
          sf::st_read(paste0(path, "us_medium_shoreline.shp")) %>%
          sf::st_transform(crs = 32620)
      }
    }
    if (!is.na(province)){
      coastline <- coastline %>%
        filter(PREABBR == province)
    }
    return(coastline)
  }
