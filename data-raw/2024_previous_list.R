library(tidyverse)
library(openxlsx)


#Read in AU file

prev_list_AU_import <- read.xlsx("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR 2024/Final List/IR_2024_Rollup-post_public_comment.xlsx",
                                 sheet = 'AU_decisions')



prev_list_AU <- prev_list_AU_import |>
  transmute(AU_ID, Pollutant = Char_Name, Pollu_ID, wqstd_code, period, AU_parameter_category = final_AU_cat,
            Year_listed,year_assessed = year_last_assessed, Rationale, previous_rationale = prev_rationale) |>
  mutate(prev_rat = case_when(Rationale == previous_rationale ~ Rationale,
                              Rationale != previous_rationale & !is.na(previous_rationale) ~ paste0("2024: ",Rationale, " ~ ", previous_rationale ),
                              is.na(previous_rationale) & !is.na(Rationale) ~ paste0("2024: ",Rationale)
                              )) |>
  select(-Rationale, -previous_rationale) |>
  rename(prev_category = AU_parameter_category,
         prev_rationale = prev_rat,
         year_last_assessed = year_assessed)

prev_list_GNIS_import <- read.xlsx("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR 2024/Final List/IR_2024_Rollup-post_public_comment.xlsx",
                                   sheet = 'GNIS_decisions')

prev_list_GNIS <- prev_list_GNIS_import |>
  transmute(AU_ID, AU_GNIS_Name, Pollutant = Char_Name, Pollu_ID, wqstd_code,
            period, GNIS_final_category = final_GNIS_cat, Rationale = Rationale_GNIS, previous_rationale = prev_GNIS_rationale) |>
  mutate(previous_rationale = case_when(is.na(Rationale) & previous_rationale == 'See previous IR reports' ~ 'For pre-2022 rationale, see previous IR reports',
                                        TRUE ~ previous_rationale)) |>
  # rename(prev_GNIS_category = GNIS_final_category,
  #        prev_GNIS_rationale = Rationale) |>
  mutate(prev_GNIS_rationale = case_when(Rationale == previous_rationale ~ Rationale,
                              Rationale != previous_rationale & !is.na(previous_rationale) ~ paste0("2024: ",Rationale, " ~ ", previous_rationale ),
                              is.na(previous_rationale) & !is.na(Rationale) ~ paste0("2024: ",Rationale),
                              is.na(Rationale) ~ previous_rationale
  )) |>
  rename(prev_GNIS_category = GNIS_final_category) |>
  select(-Rationale, -previous_rationale)



prev_list_MLoc_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Previous List Data/MLoc_rollup_final.xlsx")

prev_list_Mloc <- prev_list_MLoc_import |>
  select(AU_ID, MLocID, Pollutant, Pollu_ID, wqstd_code, period, MLocID_IR_category, Rationale) |>
  rename(prev_MLoc_category = MLocID_IR_category,
         prev_MLoc_rationale = Rationale)


prev_list_delist <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Previous List Data/Error Delistings.xlsx")

# Updates ---------------------------------------------------------------------------------------------------------
prev_list_AU <- prev_list_AU |>
  dplyr::mutate(wqstd_code = as.character(wqstd_code),
                Pollu_ID = as.character(Pollu_ID)) |>
  dplyr::mutate(Pollu_ID = dplyr::case_when(wqstd_code == '15' & Pollu_ID %in% c('77','78','79' ) ~ '77',
                                            TRUE ~ Pollu_ID),
                Pollutant = dplyr::case_when(wqstd_code == '15' & Pollu_ID %in% c('77','78','79' ) ~ 'Endosulfan',
                                             TRUE ~ Pollutant))


save(prev_list_AU, prev_list_GNIS, prev_list_Mloc, prev_list_delist, file = 'data-raw/previous_list.Rdata')

usethis::use_data(prev_list_AU,overwrite = TRUE)
usethis::use_data(prev_list_GNIS, overwrite = TRUE)
usethis::use_data(prev_list_Mloc, overwrite = TRUE)
usethis::use_data(prev_list_delist, overwrite = TRUE)





