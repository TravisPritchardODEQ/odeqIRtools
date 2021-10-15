library(tidyverse)
library(openxlsx)


# Bring in previous listings --------------------------------------------------------------------------------------

WS_GNIS_previous_listings_import <- read.xlsx("//deqhq1/WQASSESSMENT/2022IRFiles/Code/Watershed_Impairements_parameters.xlsx")

WS_GNIS_previous_listings <- WS_GNIS_previous_listings_import %>%
  select(AU_ID, Char_Name, AU_GNIS, Pollu_ID, wqstd_code, Period, IR_category) %>%
  rename(GNIS_previous_IR_impairement = IR_category)



AU_previous_categories_import <- read.csv("E:/Documents/IR2018/ATTAINS/Rollup/Basin_categories/ALL BASINS_Parameters.csv")

AU_previous_categories <- AU_previous_categories_import %>%
  select(AU_ID, Char_Name,  Pollu_ID, wqstd_code, Period, IR_category) %>%
  rename(AU_previous_IR_category = IR_category) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))



usethis::use_data(WS_GNIS_previous_listings, overwrite = TRUE)
usethis::use_data(AU_previous_categories, overwrite = TRUE)
