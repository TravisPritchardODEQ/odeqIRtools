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
      mutate(#AU_GNIS = str_c(AU_ID, AU_GNIS_Name, sep = ";"),
        Pollu_ID = as.character(Pollu_ID),
        wqstd_code = as.character(wqstd_code)) |>
      full_join(select(param_GNIS, -Pollutant), join_by(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code, period)) |>
      rename(prev_GNIS_cat = GNIS_final_category) |>
      mutate(IR_category_GNIS_24 = case_when(is.na(IR_category_GNIS_24) ~ "Unassessed",
                                             TRUE ~ IR_category_GNIS_24)) |>
      mutate(prev_GNIS_rationale = case_when(!is.na(prev_GNIS_rationale) ~ paste("2022 rationale:", prev_GNIS_rationale),
                                             is.na(prev_GNIS_rationale) ~ "See previous IR"

      ))

    GNIS_join_names <- names(GNIS_join)

    overall_join <- GNIS_join %>%
      left_join(select(prev_list_AU, -Pollutant)) %>%
      select(all_of(GNIS_join_names), prev_category, prev_rationale)


  } else {

    # non-watershed ---------------------------------------------------------------------------------------------------


    df_names <- names(df)

    overall_join <- df %>%
      mutate(Pollu_ID = as.character(Pollu_ID),
             wqstd_code = as.character(wqstd_code)) %>%
      left_join(select(AU_previous_categories, -Char_Name)) %>%
      select(all_of(df_names), AU_previous_IR_category)

  }

  # if(nrow(df) != nrow(overall_join)){
  #
  #   stop("Previous IR category join error. Input and output dataframes are not the same length.")
  # }
  #

  return(overall_join)
}
