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

  df <- other_category
  AU_type <- "Other"

  if(AU_type == "WS"){


    df_names <- names(df)


    GNIS_join <- df %>%
      mutate(AU_GNIS = str_c(AU_ID, AU_GNIS_Name, sep = ";"),
             Pollu_ID = as.character(Pollu_ID)) %>%
      left_join(WS_GNIS_previous_listings) %>%
      select(all_of(df_names), GNIS_previous_IR_impairement)

    GNIS_join_names <- names(GNIS_join)

    overall_join <- GNIS_join %>%
      left_join(AU_previous_categories) %>%
      select(all_of(GNIS_join_names), AU_previous_IR_category)
  } else {

    # non-watershed ---------------------------------------------------------------------------------------------------

    df <- other_category

    df_names <- names(df)
    overall_join <- df %>%
      mutate(Pollu_ID = as.character(Pollu_ID)) %>%
      left_join(AU_previous_categories) %>%
      select(all_of(df_names), AU_previous_IR_category)

  }

  return(overall_join)
}
