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


air_station_lookup <- function(df, monloc_col= 'MLocID', Lat_col = 'Lat_DD', Long_col = 'Long_DD'){

distinct_mlocs <- df %>%
  select(all_of(monloc_col), all_of(Long_col), all_of(Lat_col)) %>%
  distinct()

monlocs_sf <- sf::st_as_sf(df, coords=c(Long_col ,Lat_col), crs = sf::st_crs(Airtemp_stations_sf))

intersection <- data.frame(sf::st_intersection(Airtemp_stations_sf, monlocs_sf)) %>%
  select(all_of(monloc_col), Air_Station , Air_Station_Name)

rejoined <- df %>%
  left_join(intersection, by = monloc_col )


return(rejoined)
}

