library(tidyverse)
library(openxlsx)


# Bring in previous listings --------------------------------------------------------------------------------------

WS_GNIS_previous_listings_import <- read.xlsx("//deqhq1/WQASSESSMENT/2022IRFiles/Code/Watershed_Impairements_parameters.xlsx")

WS_GNIS_previous_listings <- WS_GNIS_previous_listings_import %>%
  select(AU_ID, Char_Name, AU_GNIS, Pollu_ID, wqstd_code, Period, IR_category) %>%
  rename(GNIS_previous_IR_impairement = IR_category,
         period = Period) %>%
  mutate(period = case_when(period == 'Year Round' ~ 'year_round',
                            period == 'Spawning' ~ 'Spawn',
                            period == "<Null>" ~ NA_character_))%>%
  distinct()




AU_previous_categories_import <-  readxl::read_excel("C:/Users/tpritch/Desktop/previous_listings.xlsx",
                                             col_types = c("numeric", "text", "text",
                                                           "text", "text", "text", "text", "text",
                                                           "text", "text", "text", "numeric",
                                                           "numeric", "text", "text", "text",
                                                           "text", "text"))


AU_previous_categories <- AU_previous_categories_import %>%
  select(AU_ID, Char_Name,  Pollu_ID, wqstd_code, Period, IR_category) %>%
  rename(AU_previous_IR_category = IR_category) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code)) %>%
  rename(period = Period) %>%
  mutate(period = case_when(period == 'Year Round' ~ 'year_round',
                            period == 'Spawning' ~ 'Spawn',
                            period == "<Null>" ~ NA_character_)) %>%

  distinct()



usethis::use_data(WS_GNIS_previous_listings, overwrite = TRUE)
usethis::use_data(AU_previous_categories, overwrite = TRUE)
