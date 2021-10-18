#' aggregate_data
#'
#' This function joins dataframe to the results identified in teh data aggregation process. It will subsittiute
#' IRResultNWQSunit for the mean IRResultNWQSunit of the group. It will keep one result, and discard the rest.
#' The source data, 'aggregate_data' needs to be updated every cycle.
#'
#'  @param df
#'  @export




aggregate_data <- function(df){

data_to_agg <- df %>%
  left_join(aggregate_data) %>%
  filter(!is.na(group_num)) %>%
  arrange(group_num) %>%
  group_by(group_num) %>%
  mutate(analysis_comment = paste0(Result_UID, collapse = ", "),
         keep = ifelse(row_number() == 1, 1, 0 )) %>%
  mutate(analysis_comment = paste("Result is the average of result_UIDs:",analysis_comment, " - due to multiple results at same date")) %>%
  ungroup() %>%
  select(Result_UID, mean_result, keep, analysis_comment)


data_to_analyze <- df %>%
  left_join(data_to_agg) %>%
  filter(keep == 1 | is.na(keep)) %>%
  mutate(IRResultNWQSunit = ifelse(!is.na(mean_result), mean_result, IRResultNWQSunit )) %>%
  select(-mean_result, -keep)

return(data_to_analyze)

}
