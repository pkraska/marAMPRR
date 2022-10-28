ampMapSetup <- function (dl_coastline, dl_us_coastline) {
  if (dl_coastline == TRUE) {
    temp <- tempfile()
    download.file(
      "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip",
      temp
    )
    coastlines <-
      sf::st_read(unzip(temp, exdir = "data/input/coastline")[3]) %>%
      sf::st_transform(crs = 32620)

    temp <- tempfile()
    download.file("https://coast.noaa.gov/htdata/Shoreline/us_medium_shoreline.zip",
                  temp)
  } else {
    message("Using user provided coastline shapefile found in the data/coastline folder.")
    coastlines <-
      sf::st_read(list.files("data/input/coastline/", full.names = TRUE)[grep(pattern = ".shp",
                                                                              x = list.files("data/input/coastline/"))]) %>%
      sf::st_transform(crs = 32620)
    coastlinesUS <-
      sf::st_read(list.files("data/input/coastlineUS/", full.names = TRUE)[grep(pattern = ".shp",
                                                                                x = list.files("data/input/coastlineUS/"))]) %>%
      sf::st_transform(crs = 32620)
  }
  if (dl_us_coastline == TRUE) {
    coastlinesUS <-
      sf::st_read(unzip(temp, exdir = "data/input/coastlineUS")[3]) %>%
      sf::st_transform(crs = 32620)

    message(
      "Next time you rune this function, try setting `dl_coastline = FALSE` so you don't download the coastline file again!"
    )
  }

  if (dl_us_coastline == TRUE) {
    temp <- tempfile()
    download.file("https://coast.noaa.gov/htdata/Shoreline/us_medium_shoreline.zip",
                  temp)
  } else {
    message("Using user provided coastline shapefile found in the data/coastline folder.")
    coastlinesUS <-
      sf::st_read(list.files("data/input/coastlineUS/", full.names = TRUE)[grep(pattern = ".shp",
                                                                                x = list.files("data/input/coastlineUS/"))]) %>%
      sf::st_transform(crs = 32620)
  }
}
