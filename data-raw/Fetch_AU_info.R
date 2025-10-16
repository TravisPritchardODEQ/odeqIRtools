library(arcgisbinding)
library(tidyverse)
arc.check_product()


#
# rivstream <- arc.select(arc.open('https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/34')) |>
#   select(AU_ID, AU_Name,AU_UseCode, HUC12)
#
# waterbody <- arc.select(arc.open('https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/43')) |>
#   select(AU_ID, AU_Name,AU_UseCode, HUC12)
#
# watershed <-  arc.select(arc.open('https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/IR_2022_Final/FeatureServer/44')) |>
#   select(AU_ID, AU_Name,AU_UseCode, HUC12)
#
#
# AU_info <- bind_rows(rivstream, waterbody, watershed) |>
#   filter(!is.na(HUC12)) |>
#   distinct()
#




AU_info <- arc.select(arc.open('https://services.arcgis.com/uUvqNMGPm7axC2dD/arcgis/rest/services/Oregon_AUs/FeatureServer/0')) |>
  select(AU_ID, AU_Name,AU_UseCode, HUC12) |>
  distinct()

usethis::use_data(AU_info, overwrite = TRUE)


