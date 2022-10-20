#' Data Wrangling for Aquaculture Monitoring Program
#'
#' @param x  AMP data file for a specific region and year
#' @param excelSheets the sheet name from the excel spreadsheet of data
#' @param facilityNum the facilityNumberInProject from the spreadsheet for the
#'   facility you are interested in
#' @return a list for each aquaculture facility, as well as a `combined` list of
#'   all of the data
#' @import dplyr
#' @import readxl
#' @name %>%
#' @import tidyr
#' @import sf
#'
#' @export
#'
#' @examples
#' # some examples if you want to highlight the usage in the package


ampDataLoad <-
  function(file,
           excelSheets = c("si", "m", "s", "gs", "dpab"),
           facilityNum = FALSE) {
    # Create single list for all data of interest
    raw <- list()
    for (sheet in excelSheets) {
      raw[[sheet]] <-
        readxl::read_excel(file,
                           sheet = sheet,
                           na = "NA")
    }
    if (facilityNum == FALSE) {
      facility <- unique(raw[['si']]$facilityNumberInProject)
    } else {
      facility = facilityNum
    }

    data <- list()
    for (site in facility) {
      # data[[site]] <- list()
      data[[site]][['si']] <- raw[['si']] %>%
        dplyr::filter(facilityNumberInProject == site,
                      !is.na(sampleCode)) %>%
        sf::st_as_sf(
          coords = c('longitude', 'latitude'),
          crs = 4326,
          remove = FALSE
        ) %>%
        sf::st_transform(crs = 32620)

      data[['combined']][['si']] <- data[['combined']][['si']] %>%
        dplyr::bind_rows(data[[site]][['si']])

      for (sheet in excelSheets[!(excelSheets %in% 'si')]) {
        sheetColNames <- colnames(raw[[sheet]])
        data[[site]][[sheet]] <- raw[[sheet]] %>%
          dplyr::mutate(sampleCode = as.character(sampleCode)) %>%
          dplyr::left_join(raw[['si']], by = 'sampleCode') %>%
          dplyr::filter(facilityNumberInProject == site) %>%
          dplyr::select(sampleCode, sheetColNames[!(sheetColNames %in% c('sampleCode', 'sampleType'))])
      }

      # Metals data transformation
      if ("m" %in% excelSheets) {
        data[[site]][['m']] <- data[[site]][['si']] %>%
          dplyr::left_join(data[[site]][['m']], by = 'sampleCode') %>%
          tidyr::pivot_longer(
            cols = ends_with("PerKg"),
            names_to = "metal",
            values_to = "concentration"
          ) %>%
          dplyr::filter(!is.na(concentration)) %>%
          dplyr::mutate(
            concentration = suppressWarnings(as.numeric(concentration)),
            metal = stringr::str_replace(
              string = metal,
              pattern = "MgPerKg",
              replacement = ""
            )
          ) %>%
          dplyr::select(sampleCode,
                        latitude,
                        longitude,
                        metal,
                        concentration)
      }

      # Sulfides data transformation
      if ("s" %in% excelSheets) {
        data[[site]][['s']] <- data[[site]][['si']] %>%
          dplyr::left_join(data[[site]][['s']], by = 'sampleCode') %>%
          tidyr::pivot_longer(
            cols = starts_with("UV1sulphideUM"),
            names_to = "wavelength",
            values_to = "concentration"
          ) %>%
          dplyr::filter(!is.na(concentration)) %>%
          dplyr::mutate(
            concentration = suppressWarnings(as.numeric(concentration)),
            wavelength = stringr::str_replace(
              string = wavelength,
              pattern = "UV1sulphideUM",
              replacement = ""
            ),
            wavelength = stringr::str_replace(
              string = wavelength,
              pattern = "NM",
              replacement = "nm"
            )
          ) %>%
          dplyr::select(sampleCode,
                        latitude,
                        longitude,
                        wavelength,
                        concentration)
      }

      # DPAB data transformation
      if ("dpab" %in% excelSheets) {
        data[[site]][['dpab']] <- data[[site]][['si']] %>%
          dplyr::left_join(data[[site]][['dpab']], by = 'sampleCode') %>%
          tidyr::pivot_longer(
            cols = ends_with("NgPg"),
            names_to = "compound",
            values_to = "concentration"
          ) %>%
          dplyr::filter(!is.na(concentration)) %>%
          dplyr::mutate(
            concentration = suppressWarnings(as.numeric(concentration)),
            compound = stringr::str_replace(
              string = compound,
              pattern = "NgPg",
              replacement = ""
            )
          ) %>%
          dplyr::mutate(dryConc = concentration / (1 - (MoisturePercent /
                                                          100))) %>%
          dplyr::select(
            sampleCode,
            latitude,
            longitude,
            compound,
            concentration,
            moisturePercent = MoisturePercent,
            dryConc
          )
      }
    }

    return(data)
  }
