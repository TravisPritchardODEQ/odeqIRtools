#' rollup_WS_AU
#'
#' This function rolls up GNIS assessments to the AU scale.
#'
#' @param df input dataframe of GNIS assessments
#' @param char_name_field field that contains the characteristic name.No quotes.
#' @export




rollup_WS_AU <- function(df, char_name_field){

  WS_AU_rollup <- df |>
    ungroup() %>%
    mutate(Rationale_GNIS = case_when(!is.na(Rationale_GNIS) ~ paste0(AU_GNIS_Name, ": ",Rationale_GNIS ),
                                      TRUE ~ Rationale_GNIS)) |>
    group_by(AU_ID, {{char_name_field}}, Pollu_ID, wqstd_code, period,prev_AU_category,prev_AU_rationale) %>%
    summarise(IR_category_AU_24 = max(final_GNIS_cat),
              Rationale_AU = str_c(Rationale_GNIS,collapse =  " ~ " ) ) %>%
    ungroup() |>
    mutate(IR_category_AU_24 = case_when( str_detect(IR_category_AU_24, "3") & str_detect(prev_AU_category, "2|5") ~ prev_AU_category,
                                          TRUE ~ IR_category_AU_24)) |>
    mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period )) |>
    mutate(status_change = case_when(IR_category_AU_24 == prev_AU_category & is.na(Rationale_AU) ~ "No change in status- No New Assessment",
                                     IR_category_AU_24 == prev_AU_category & !is.na(Rationale_AU)~ "No change in status- New Assessment",
                                     IR_category_AU_24 == '2' & prev_AU_category %in% c('5','4A','4B', '4C') ~ "Delist",
                                     prev_AU_category == 'Unassessed' | is.na(prev_AU_category)  ~ "New Assessment",
                                     prev_AU_category == '2' & IR_category_AU_24  %in% c('5','4A','4B', '4C') ~ "Attain to Impaired",
                                     prev_AU_category %in% c('3D','3','3B', '3C') & IR_category_AU_24 %in% c('5','4A','4B', '4C') ~ "Insufficient to Impaired",
                                     prev_AU_category %in% c('3D','3','3B', '3C') & IR_category_AU_24 %in% c('2') ~ "Insufficient to Attain"
    ))


}
