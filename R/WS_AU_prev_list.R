#' WS_AU_prev_list
#'
#' This function adds previous cycle AU assessments to GNIS assessments taht have been rolled up to the AU scale.
#'
#'@param df input dataframe. Usually the output of rollup_WS_AU()
#'@export



WS_AU_prev_list <- function(df){


  WS_AU_rollup_joined <- df |>
    left_join(select(prev_list_AU, -prev_category, -prev_rationale, -Pollutant)) |>
    mutate(Year_listed = case_when(status_change %in% c("Attain to Impaired", "Insufficient to Impaired","New Assessment") &
                                     IR_category_AU_26  %in% c('5','4A','4B', '4C') &
                                     is.na(Year_listed) ~ '2024',
                                   TRUE ~ Year_listed),
           year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~"2026",
                                          TRUE ~ year_last_assessed))

}
