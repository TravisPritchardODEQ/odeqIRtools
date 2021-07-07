#' air_temp_exclusion
#'
#' This function performs the air temperature exclusion check as identified in the Water Temperature standard.
#' A dataframe of data from AWQMS or the IR database is passed through the air_station_lookup function and then joined
#' with the OR_air_temp dataset which contains daily max air temp values (Air_Temp_daily_max) and calculated
#' 90th percentile of annual seven-day average maximum air temperatures calculated using 10 years of air temperature
#' data (air_temp_exclusion_value). above_exclusion_1d identifies if that day's maximum air temperature was above
#' the air temp exclusion value. above_exclusion_7d identifies if any of the 7 previous days (inclusive of current day)
#' were above the air temperature exclusion value.
#'
#' @param df dataframe to join air temperature data to. Intended to be an export from AWQMS or the IR database
#' @param date_col single element string identifying the name of the date column.
#' @param mloc_col single element string identifying the name of the monitoring location column.
#' @param lat_col single element string identifying the name of the latitude column.
#' @param long_col single element string identifying the name of the longitude column.
#' @return Original dataframe with NOAA air temperature station ID, Name, and air temperature data appended
#' @export



air_temp_exclusion <- function(df, date_col = 'Date', monloc_col= 'MLocID',
                               lat_col = 'Lat_DD', long_col = 'Long_DD'){

  # Structure the Date to date_col join by statement
  # This allows us to join the "Date" field in OR_air_temp to whatever the data column is named in the starting
  # dataframe.
  by_date = rlang::set_names('Date', dplyr::quo_name(date_col))

  join_air_station <- air_station_lookup(df, monloc_col= monloc_col, lat_col = lat_col, long_col = long_col)

  join_data <- join_air_station %>%
    dplyr::left_join(dplyr::select(OR_air_temp, Air_Station, Date, Air_Temp_daily_max,
                                   air_temp_exclusion_value, above_exclusion_1d, above_exclusion_7d),
                     by = c('Air_Station', by_date ) )

  return(join_data)

}

