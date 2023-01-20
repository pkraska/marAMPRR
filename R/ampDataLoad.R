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

      test <- data[['s']] %>%
        sf::st_drop_geometry() %>%
        select(
          sampleCode,
          conc_ise = ISEsulphideUM,
          conc_mb_1 = mBlueSulphideUM_1cm,
          conc_mb_2 = mBlueSulphideUM_2cm,
          conc_uv_1 = sulphideUV1CM,
          conc_uv_2 = sulphideUV2CM,
          conc_ise = ISEsulphideUM
        ) %>%
        mutate(
          conc_uv_1 = as.character(conc_uv_1),
          conc_uv_2 = as.character(conc_uv_2),
          conc_ise = case_when(conc_ise == "<LOQ" ~ "-999",
                               TRUE ~ conc_ise),
          conc_mb_1 = case_when(conc_mb_1 == "<LOQ" ~ "-999",
                                TRUE ~ conc_mb_1),
          conc_mb_2 = case_when(conc_mb_2 == "<LOQ" ~ "-999",
                                TRUE ~ conc_mb_2),
          conc_uv_1 = case_when(conc_uv_1 == "<LOQ" ~ "-999",
                                TRUE ~ conc_uv_1),
          conc_uv_2 = case_when(conc_uv_2 == "<LOQ" ~ "-999",
                                TRUE ~ conc_uv_2)
        ) %>%
        pivot_longer(cols = -sampleCode,names_to = "method", values_to = "conc") %>%
        mutate(depth = case_when(method == "conc_ise" ~ 1,
                                 grepl(pattern = "_1", x = method) ~ 1,
                                 grepl(pattern = "_2", x = method) ~ 2,
                                 TRUE ~ NA_real_),
               method = case_when(grepl("ise", method) ~ "ISE",
                                  grepl("mb", method) ~  "MB",
                                  grepl("uv", method)~ "UV"),
               conc = as.numeric(conc))
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
