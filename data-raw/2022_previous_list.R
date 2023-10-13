library(tidyverse)
library(openxlsx)


#Read in AU file

prev_list_AU_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Previous List Data/AU_all_rollup.xlsx",
                                 sheet = 'AU_all')



prev_list_AU <- prev_list_AU_import |>
  select(AU_ID, Pollutant, Pollu_ID, wqstd_code, period, AU_parameter_category,Year_listed,year_assessed, Rationale, previous_rationale) |>
  mutate(prev_rat = case_when(Rationale == previous_rationale ~ Rationale,
                              Rationale != previous_rationale & !is.na(previous_rationale) ~ paste0("2022: ",Rationale, " ~ ", previous_rationale ),
                              is.na(previous_rationale) & !is.na(Rationale) ~ paste0("2022: ",Rationale)
                              )) |>
  select(-Rationale, -previous_rationale) |>
  rename(prev_category = AU_parameter_category,
         prev_rationale = prev_rat,
         year_last_assessed = year_assessed)

prev_list_GNIS_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Previous List Data/GNIS_rollup_final.xlsx")

prev_list_GNIS <- prev_list_GNIS_import |>
  select(AU_ID, AU_GNIS_Name, Pollutant, Pollu_ID, wqstd_code, period, GNIS_final_category, Rationale) |>
  rename(prev_GNIS_category = GNIS_final_category,
         prev_GNIS_rationale = Rationale)


prev_list_MLoc_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Previous List Data/MLoc_rollup_final.xlsx")

prev_list_Mloc <- prev_list_MLoc_import |>
  select(AU_ID, MLocID, Pollutant, Pollu_ID, wqstd_code, period, MLocID_IR_category, Rationale) |>
  rename(prev_MLoc_category = MLocID_IR_category,
         prev_MLoc_rationale = Rationale)


prev_list_delist <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Previous List Data/Error Delistings.xlsx")


save(prev_list_AU, prev_list_GNIS, prev_list_Mloc, prev_list_delist, file = 'data-raw/previous_list.Rdata')

usethis::use_data(prev_list_AU,overwrite = TRUE)
usethis::use_data(prev_list_GNIS, overwrite = TRUE)
usethis::use_data(prev_list_Mloc, overwrite = TRUE)
usethis::use_data(prev_list_delist, overwrite = TRUE)

