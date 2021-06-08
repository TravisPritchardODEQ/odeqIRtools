#' This file generates the spacial data used for the air temperature exclusion check.
#' Files inside the "data-raw/Air_temp_stations" folder should be an exported shp file from ArcGIS.
#' To generate the shp files, load in the usage NOAA stations, and run the "Generate Thyssen polygons" tool.

library(sf)


Airtemp_stations_sf <- read_sf(dsn = "data-raw/Air_temp_stations")


usethis::use_data(Airtemp_stations_sf, overwrite = TRUE)
