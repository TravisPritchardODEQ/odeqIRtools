#' air_station_lookup
#'
#' This function matches monitoring stations to their nearest NOAA air temperature station. This tool references data
#' created by shp files generated from NOAA datasets that represent stations with enugh data to be usable for the air
#' temperature exclusion.
#'
#' @param df Dataframe of data or monitoring locations that need to be matched to air temp stations
#' @param monloc_col Name of column in df that contains monitoring location name data. Default is "MLOCID"
#' @param Lat_col Name of column in df that contains Latitude information. Default is "Lat_DD"
#' @param Long_col Name of column in df that contains Longitude information. Default is "Long_DD"
#' @return Original dataframe with NOAA air temperature station ID and Name appended
#' @export


air_station_lookup <- function(df, monloc_col= 'MLocID', lat_col = 'Lat_DD', long_col = 'Long_DD'){

distinct_mlocs <- df %>%
  dplyr::select(all_of(monloc_col), dplyr::all_of(long_col), dplyr::all_of(lat_col)) %>%
  dplyr::distinct()

monlocs_sf <- sf::st_as_sf(distinct_mlocs, coords=c(long_col ,lat_col), crs = sf::st_crs(Airtemp_stations_sf))

intersection <- data.frame(sf::st_intersection(Airtemp_stations_sf, monlocs_sf)) %>%
  dplyr::select(dplyr::all_of(monloc_col), Air_Station )

rejoined <- df %>%
  dplyr::left_join(intersection, by = monloc_col )

print('Data joined to nearest neighbor air temperature stations identified in the 2022 IR process')

return(rejoined)
}

