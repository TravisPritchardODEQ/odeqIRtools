#' Creates the csv file needed for displaying the standards info on the webmap.
#'
#' This function pulls the standards information from the georeferenced standards layer and joins
#' the codes to the relaionship table. This is the data source for when you click on the standards
#' layer and you get the popup with all the standards in one place. For 2018 and 2022 this is passed to
#' Dave Pray to integrate into the standards layer.
#'
#' @param GeoRef_Standards_path The Georeferenced standards layer. Use the GDB file in the GIS library
#' @param relation_table The excel doc with the relationship tables for the standards layer. Should be on the sharepoint site
#' @param write_path The path to save the outputs
#' @return Returns nothing. Saves 2 Rdata files and the strd layer display csv to write_path
#' @export


strd_lyr_display <- function(GeoRef_Standards_path,
                             relation_table,
                             write_path ) {




  options(scipen = 99999999)

  # Copy the oregon standards out of the geodatabase ------------------------



  subset(rgdal::ogrDrivers(), grepl("GDB", name))
  fc_list <- rgdal::ogrListLayers(GeoRef_Standards_path)

  #oregon_standards <- readOGR(dsn=path,layer="Oregon_Standards")


  oregon_standards <- sf::st_read(dsn=GeoRef_Standards_path,layer="Oregon_Standards")

  standards <- as.data.frame(oregon_standards)

  standards <- sf::st_zm(oregon_standards) %>%
    dplyr::mutate_if(is.factor, as.character)
  save(standards, file = paste0(write_path, "/standards.Rdata"))

  # load("Other tools/Standard_layer_display/standards.Rdata")


  # Get relationships -------------------------------------------------------




  BenUse_codes <- openxlsx::read.xlsx(relation_table, sheet = 'BenUse_codes')

  # BenUse_codes_flat <- BenUse_codes %>%
  #   group_by(ben_use_code) %>%
  #   summarise(Designated_uses =  str_c(str_sort(ben_use), collapse = "; "))

  WQStds_codes <- openxlsx::read.xlsx(relation_table, sheet = 'WQStds_codes')
  wqstds_by_useID <- openxlsx::read.xlsx(relation_table, sheet = 'wqstds_by_useID')

  wqstrd_combined <- wqstds_by_useID %>%
    dplyr::left_join(WQStds_codes) %>%
    dplyr::mutate(WQstd_OAR = dplyr::case_when(!is.na(OAR_Criteria) ~ paste0(wqstd, " (OAR: ", OAR_Criteria, ")"),
                                 TRUE ~ wqstd
    )
    )


  ben_use_wqstrd <- BenUse_codes %>%
    dplyr::left_join(wqstrd_combined)

  ben_use_wqstrd_collapsed <- ben_use_wqstrd %>%
    dplyr::group_by(ben_use_code) %>%
    dplyr::summarise(Designated_uses =  stringr::str_c(stringr::str_sort(unique(ben_use)), collapse = "; "),
              applicable_strds = stringr::str_c(stringr::str_sort(unique(WQstd_OAR)), collapse = "; "))


  data.table::fwrite(ben_use_wqstrd_collapsed, paste0(write_path, "/ben_use_wqstrd.csv"),
         row.names = FALSE)

  standards_ben_use <- standards %>%
    dplyr::left_join(ben_use_wqstrd_collapsed) %>%
    dplyr::mutate(GNIS_Name = ifelse(is.na(GNIS_Name), "Unnamed Stream", GNIS_Name ))


  temp_criteria <- openxlsx::read.xlsx(relation_table, sheet = 'temperature_criteria') %>%
    dplyr::mutate(TempCode = as.character(TempCode)) %>%
    dplyr::rename(`Temperature Criterion (7dADM-°C)` = `Criterion.(7dADM-°C)`
    )

  temp_spawning_dates <- openxlsx::read.xlsx(relation_table, sheet = 'Spawning_dates') %>%
    dplyr::mutate(SpawnCode = as.character(SpawnCode),
           DO_SpawnCode = as.character(DO_SpawnCode)) %>%
    dplyr::select(-DO_SpawnCode) %>%
    dplyr::rename(Temperature_Spawn_dates = Spawn_date_range)

  DO_spawning_dates <- openxlsx::read.xlsx(relation_table, sheet = 'Spawning_dates') %>%
    dplyr::mutate(SpawnCode = as.character(SpawnCode),
           DO_SpawnCode = as.character(DO_SpawnCode)) %>%
    dplyr::select(-SpawnCode) %>%
    dplyr::rename(DO_Spawn_dates = Spawn_date_range)

  DO_criteria <- openxlsx::read.xlsx(relation_table, sheet = 'DO_criteria') %>%
    dplyr::mutate(DO_code = as.character(DO_code)) %>%
    dplyr::mutate(DO_code = ifelse(nchar(DO_code) == 1, paste0("0", DO_code), DO_code ))

  pH_criteria <- openxlsx::read.xlsx(relation_table, sheet = 'pH_criteria') %>%
    dplyr::mutate(pH_code = as.character(pH_code)) %>%
    dplyr::mutate(pH_code = ifelse(nchar(pH_code) == 1, paste0("0", pH_code), pH_code ))

  ALtoxics_criteria <-  openxlsx::read.xlsx(relation_table, sheet = 'ALtoxics_criteria') %>%
    dplyr::mutate(WaterType_code = as.character(WaterType_code)) %>%
    dplyr::rename(WaterTypeCode = WaterType_code) %>%
    dplyr::mutate(WaterTypeCode = ifelse(nchar(WaterTypeCode) == 1, paste0("0", WaterTypeCode), WaterTypeCode ))

  Bacteria_criteria <- openxlsx::read.xlsx(relation_table, sheet = 'Bacteria_criteria') %>%
    dplyr::mutate(bacteria_code = as.character(bacteria_code)) %>%
    dplyr::rename(BacteriaCode = bacteria_code) %>%
    dplyr::mutate(BacteriaCode = ifelse(nchar(BacteriaCode) == 1, paste0("0", BacteriaCode), BacteriaCode )) %>%
    dplyr::rename(Bacteria_criteria = bacteria_criteria,
           Bacteria_indicator = bacteria_indicator)

  standards_ben_use_all <- standards_ben_use %>%
    dplyr::left_join(temp_criteria, by = "TempCode") %>%
    dplyr::left_join(temp_spawning_dates, by = "SpawnCode") %>%
    dplyr::left_join(DO_spawning_dates, by = "DO_SpawnCode") %>%
    dplyr::left_join(DO_criteria, by = "DO_code") %>%
    dplyr::left_join(pH_criteria,by = "pH_code") %>%
    dplyr::left_join(ALtoxics_criteria,  by = "WaterTypeCode") %>%
    dplyr::left_join(Bacteria_criteria, by = "BacteriaCode")


  standards_ben_use_all_smaller <- standards_ben_use_all %>%
    sf::st_set_geometry(NULL)

  # write.csv(standards_ben_use_all, file = "Other tools/Standard_layer_display/standards_layer_for_display.csv",
  #           row.names = FALSE)

  data.table::fwrite(standards_ben_use_all_smaller, paste0(write_path, "/standards_layer_for_display.csv"),
         row.names = FALSE)

}
