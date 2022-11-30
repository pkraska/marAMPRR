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
#' @import tidyr
#' @import sf
#'
#' @export
#'
#' @examples
#' # some examples if you want to highlight the usage in the package


ampDataLoad <-
  function(file,
           excelSheets = c("si", "m", "s", "gs", "dpab")) {
    # Create single list for all data of interest
    raw <- list()
    data <- list()

    for (sheets in excelSheets) {
      raw[[sheets]] <-
        readxl::read_excel(file,
                           sheet = sheets,
                           na = "NA") %>%
        dplyr::mutate(sampleCode = as.character(sampleCode))
    }
    data[['si']] <- raw[['si']] %>%
      dplyr::filter(!is.na(sampleCode),
                    !is.na(longitude),
                    !is.na(latitude)) %>%
      sf::st_as_sf(
        coords = c('longitude', 'latitude'),
        crs = 4326,
        remove = FALSE
      ) %>%
      sf::st_transform(crs = 32620) %>%
      dplyr::mutate(sampleCode = as.character(sampleCode))


    for (sheet in excelSheets[!(excelSheets %in% 'si')]) {
      sheetColNames <- colnames(raw[[sheet]])
      data[[sheet]] <- data[['si']] %>%
        dplyr::mutate(sampleCode = as.character(sampleCode)) %>%
        dplyr::left_join(raw[[sheet]], by = 'sampleCode') %>%
        dplyr::select(sampleCode, sheetColNames[!(sheetColNames %in% c('sampleCode', 'sampleType'))])
    }


    # Metals data transformation
    if ("m" %in% excelSheets) {
      print("METALS")
      data[['m']] <- data[['si']] %>%
        sf::st_join(data[['m']], by = 'sampleCode') %>%
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
        dplyr::select(sampleCode = sampleCode.x,
                      latitude,
                      longitude,
                      metal,
                      concentration)
    }

    # Sulfides data transformation
    if ("s" %in% excelSheets) {
      print("SULFIDES")
      data[['s']] <- data[['si']] %>%
        sf::st_join(data[['s']], by = 'sampleCode') %>%
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
        dplyr::select(sampleCode = sampleCode.x,
                      latitude,
                      longitude,
                      depth = depthPoreWaterExtractionCM,
                      wavelength,
                      concentration)
    }

    # DPAB data transformation
    if ("dpab" %in% excelSheets) {
      print("DPAB")
      data[['dpab']] <- data[['si']] %>%
        sf::st_join(data[['dpab']], by = 'sampleCode') %>%
        tidyr::pivot_longer(cols = ends_with("NgPg"),
                            names_to = "compound",
                            values_to = "concentration") %>%
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
          sampleCode = sampleCode.x,
          latitude,
          longitude,
          compound,
          concentration,
          moisturePercent = MoisturePercent,
          dryConc
        )
    }

    data <- data[order(names(data))]
    return(data)
  }
