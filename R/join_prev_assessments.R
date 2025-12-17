#' join_prev_assessments
#'
#' This function joins in previous assessment conclusions to current assessment process.
#' If AY_type == 'WS', the data will be joined to previous impairements bu AU_GNIS as well as overall AU categorization.
#' IF AU_type is 'other', the data will be jopined to overall AU assessment conclusion.
#'
#' @param df Dataframe of new assessment conclusions
#' @param AU_type What sort of AU type is the join opertaing on "WS" or Other"
#' @export






join_prev_assessments <- function(df, AU_type){

  # test dataset ----------------------------------------------------------------------------------------------------

  # df <- WS_GNIS_rollup
  # AU_type <- "WS"

  if(AU_type == "WS"){

    df_names <- names(df)

    param_GNIS <- prev_list_GNIS |>
      filter(Pollu_ID %in% df$Pollu_ID,
             wqstd_code %in% df$wqstd_code,
             period %in% df$period)

    GNIS_join <- df |>
      ungroup() |>
      mutate(#AU_GNIS = str_c(AU_ID, AU_GNIS_Name, sep = ";"),
        Pollu_ID = as.character(Pollu_ID),
        wqstd_code = as.character(wqstd_code)) |>
      full_join(select(param_GNIS, -Pollutant), join_by(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code, period)) |>
      mutate(IR_category_GNIS_26 = case_when(is.na(IR_category_GNIS_26) ~ "Unassessed",
                                             TRUE ~ IR_category_GNIS_26)) |>
      mutate(prev_GNIS_rationale = case_when(!is.na(prev_GNIS_rationale) ~ paste("2024 rationale:", prev_GNIS_rationale),
                                             !is.na(prev_GNIS_category) & is.na(prev_GNIS_rationale) ~ "See previous IR reports",
                                             TRUE ~ prev_GNIS_rationale),
             prev_GNIS_category = case_when(is.na(prev_GNIS_category) ~ "Unassessed",
                                            TRUE ~ prev_GNIS_category)) |>
      mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_26, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE),
             prev_GNIS_category = factor(prev_GNIS_category, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |>
      mutate(final_GNIS_cat = pmax(IR_category_GNIS_26,prev_GNIS_category )) |>
      arrange(AU_ID, AU_GNIS_Name)

    GNIS_join_names <- names(GNIS_join)



    overall_join <- GNIS_join %>%
      left_join(select(prev_list_AU, -Pollutant)) %>%
      select(all_of(GNIS_join_names), prev_category, prev_rationale) |>
      rename(prev_AU_category = prev_category,
             prev_AU_rationale = prev_rationale)




  } else {

    # non-watershed ---------------------------------------------------------------------------------------------------



    df_names <- names(df)

    param_AU_previous_categories <- prev_list_AU |>
      filter(Pollu_ID %in% df$Pollu_ID,
             wqstd_code %in% df$wqstd_code,
             period %in% df$period) |>
      filter(str_detect(AU_ID, "WS", negate = TRUE))



    overall_join <- df %>%
      mutate(Pollu_ID = as.character(Pollu_ID),
             wqstd_code = as.character(wqstd_code)) %>%
      full_join(select(param_AU_previous_categories, -Pollutant)) |>
      mutate(IR_category = case_when( is.na(IR_category) ~ "Unassessed",
                                      TRUE ~IR_category )) |>
      mutate(prev_category = case_when(is.na(prev_category) ~ "Unassessed",
                                       TRUE ~ prev_category)) |>
      mutate(IR_category = factor(IR_category, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE),
             prev_category = factor(prev_category, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |>
      mutate(final_AU_cat = pmax(prev_category, IR_category, na.rm = TRUE) )
  }



  if(nrow(df) != nrow(overall_join)){

    warning("Previous IR category join error. Input and output dataframes are not the same length.")
  }


  return(overall_join)
}
