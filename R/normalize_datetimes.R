#' Normalize Datetimes
#'
#' This function takes in datetime data that is collected under different timezones, and normalizes them to the same
#' time zone. This function is needed because R does not allow multiple timezones in a vector.
#'
#' @param df dataframe to modify
#' @param datecol Name of the column that contains date values
#' @param timecol Name of the column that contains time values
#' @param tzcol Name of column that contains timezone values
#' @param target_tz Timezone data should be normalized to
#' @param norm_time_colname Name of column new data should be written to
#' @export





normalize_datetime <- function(df, datecol = 'SampleStartDate', timecol = 'SampleStartTime', tzcol = 'SampleStartTZ',
                            target_tz = "America/Los_Angeles", norm_time_colname = 'time_norm', vector_only = FALSE){


  datetime <- df %>%
    dplyr::mutate(datetime_combo = paste(!!sym(datecol), !!sym(timecol))) %>%
    dplyr::pull(datetime_combo)



  tmp <- lapply(1:nrow(df), function(i) {
    lubridate::with_tz(lubridate::ymd_hms(datetime[[i]], tz = df[tzcol][i,]), target_tz)
  })

  final <- unlist(tmp)
  attributes(final) <- attributes(tmp[[ 1 ]])
  final


  if(!vector_only){

  df[norm_time_colname] <- final

  return(df)
  } else {

    return(final)

  }



}


