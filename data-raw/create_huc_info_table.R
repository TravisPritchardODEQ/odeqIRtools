library(tidyverse)
library(sf)

#This script pulls in HUC info from an ARCGIS geodatabase and creates a table of HUC numbers and names. This data is used
#To create HUC names in IR output files.


geodb <-'C:/Users/tpritch/Documents/GIS files/HUCs/HUCs.gdb'



extract_huc <- function(layer_name){

  import <- st_read(geodb, layer = layer_name)  |>
    st_drop_geometry() |>
    select(Shape_Length, -Shape_Area  ) |>
    rename_with(~paste0(layer_name, "_name"), 'name')


}


HUC12_import <- extract_huc("HUC12")

HUC10_import <- extract_huc("HUC10")

HUC8_import <- extract_huc("HUC8") |>
  select(huc8, HUC8_name)


HUC6_import <- extract_huc("HUC6")

HUC4_import <- extract_huc("HUC4")



HUC_info <- HUC12_import |>
  mutate(huc10 = str_sub(huc12, 1,10)) |>
  left_join(HUC10_import) |>
  mutate(huc8 = str_sub(huc12, 1,8)) |>
  left_join(HUC8_import)  |>
  mutate(huc6 = str_sub(huc12, 1,6)) |>
  left_join(HUC6_import)  |>
  mutate(huc4 = str_sub(huc12, 1,4)) |>
  left_join(HUC4_import) |>
  rename_with(toupper)



usethis::use_data(HUC_info)
